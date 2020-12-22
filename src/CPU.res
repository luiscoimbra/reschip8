open Js_typed_array

type address = int

type uiMap = array<array<int>>

let memory_offset = 0x200

type t = {
  // Chip8 is capable of access up to 4kb of memory
  // Interpreter uses the first 512 bytes. from 0x000 to 0x1FF
  // The rest of the memory is reserved for the program data > 0x200
  memory: Uint8Array.t,
  // REGISTERS
  // 16 General Purpose 8-bit (Vx) from V0 to VF
  v: Uint8Array.t,
  // Index addressess 16-bit from 0000 to FFFF
  i: address,
  // Timers (when non-zero, it shoud automatically decrement at a rate of 60hz)
  // DT = Delay, ST = Sound
  dt: int,
  st: int,
  // Program Counter 16-bit - Currently executing address, starts at 200 on chip8
  // spaces less than 200 are reserved to sprites or any other needs
  pc: address,
  // Stack Pointer 8-bit - point to the topmost level of the stack
  sp: int,
  // Stack of returning addressess from subroutines
  stack: Uint16Array.t,
  // EXTERNALS
  // Maps references to UI 64x32
  ui: uiMap,
  // Maps key current state
  key: int,
  halted: bool,
}

type variables = KK(int) | N(int) | NNN(int) | X(int) | Y(int)

let getVariable = opcode =>
  switch opcode {
  | X(code) => code->land(0x0f00)->lsr(8)
  | Y(code) => code->land(0x00f0)->lsr(4)
  | NNN(code) => code->land(0xfff)
  | KK(code) => code->land(0x00ff)
  | N(code) => code->land(0x000f)
  }

let getEmpty = {
  memory: Uint8Array.fromLength(4096),
  v: Uint8Array.fromLength(16),
  i: 0,
  dt: 0,
  st: 0,
  pc: memory_offset,
  sp: 0,
  stack: Uint16Array.fromLength(16),
  ui: Belt.Array.make(32, Belt.Array.make(64, 0)),
  key: -1,
  halted: false,
}

type key = Number(int) | A | B | C | D | E | F

let fontSet = Uint8Array.make([
  0xF0,
  0x90,
  0x90,
  0x90,
  0xF0, // 0
  0x20,
  0x60,
  0x20,
  0x20,
  0x70, // 1
  0xF0,
  0x10,
  0xF0,
  0x80,
  0xF0, // 2
  0xF0,
  0x10,
  0xF0,
  0x10,
  0xF0, // 3
  0x90,
  0x90,
  0xF0,
  0x10,
  0x10, // 4
  0xF0,
  0x80,
  0xF0,
  0x10,
  0xF0, // 5
  0xF0,
  0x80,
  0xF0,
  0x90,
  0xF0, // 6
  0xF0,
  0x10,
  0x20,
  0x40,
  0x40, // 7
  0xF0,
  0x90,
  0xF0,
  0x90,
  0xF0, // 8
  0xF0,
  0x90,
  0xF0,
  0x10,
  0xF0, // 9
  0xF0,
  0x90,
  0xF0,
  0x90,
  0x90, // A
  0xE0,
  0x90,
  0xE0,
  0x90,
  0xE0, // B
  0xF0,
  0x80,
  0x80,
  0x80,
  0xF0, // C
  0xE0,
  0x90,
  0x90,
  0x90,
  0xE0, // D
  0xF0,
  0x80,
  0xF0,
  0x80,
  0xF0, // E
  0xF0,
  0x80,
  0xF0,
  0x80,
  0x80, // F])
])

let loadFontSet = cpu => {
  let {memory} = cpu
  for i in 0 to Uint8Array.length(fontSet) {
    Uint8Array.unsafe_set(memory, i, Uint8Array.unsafe_get(fontSet, i))
  }
  {...cpu, memory: memory}
}

// We will have a CPU with rom data fully stored
let loadRom = romBuffer => {
  switch romBuffer {
  | Some(rom) => {
      let {memory} = getEmpty
      for i in 0 to Js.Array.length(rom) - 1 {
        Uint8Array.unsafe_set(memory, memory_offset + i, int_of_string("0x" ++ rom[i]))
      }

      {
        ...getEmpty,
        memory: memory,
      }
    }
  | None => getEmpty
  }
}

// Read memory address from PC and PC +1 counter
// Update PC to PC + 2
// Return Opcode
let fetch = cpu => {
  let {pc, memory} = cpu
  let codes = [Uint8Array.unsafe_get(memory, pc), Uint8Array.unsafe_get(memory, pc + 1)]
  ({...cpu, pc: pc + 2}, lsl(codes[0], 8) + codes[1])
}

let decode = opcode => opcode->Instruction.get

open Instruction
let execute = (cpu, (opcode, instruction)) =>
  switch instruction {
  | CLS => {...cpu, ui: Belt.Array.make(32, Belt.Array.make(64, 0))}
  | RET => {...cpu, pc: Uint16Array.unsafe_get(cpu.stack, cpu.sp), sp: cpu.sp - 1}
  | JP_addr => {...cpu, pc: opcode->NNN->getVariable}
  | CALL_addr => {
      let sp = cpu.sp + 1
      Uint16Array.unsafe_set(cpu.stack, sp, cpu.pc)
      {...cpu, sp: sp, pc: opcode->NNN->getVariable}
    }
  | SE_Vx_byte =>
    Uint8Array.unsafe_get(cpu.v, opcode->X->getVariable) === opcode->KK->getVariable
      ? {...cpu, pc: cpu.pc + 2}
      : cpu
  | SNE_Vx_byte =>
    Uint8Array.unsafe_get(cpu.v, opcode->X->getVariable) !== opcode->KK->getVariable
      ? {...cpu, pc: cpu.pc + 2}
      : cpu
  | SE_Vx_Vy =>
    Uint8Array.unsafe_get(cpu.v, opcode->X->getVariable) ===
      Uint8Array.unsafe_get(cpu.v, opcode->Y->getVariable)
      ? {...cpu, pc: cpu.pc + 2}
      : cpu
  | LD_Vx_byte => {
      Uint8Array.unsafe_set(cpu.v, opcode->X->getVariable, opcode->KK->getVariable)
      cpu
    }
  | ADD_Vx_byte => {
      let vx = Uint8Array.unsafe_get(cpu.v, opcode->X->getVariable)
      Uint8Array.unsafe_set(cpu.v, opcode->X->getVariable, vx + opcode->KK->getVariable)
      cpu
    }
  | LD_Vx_Vy => {
      let vy = Uint8Array.unsafe_get(cpu.v, opcode->Y->getVariable)
      Uint8Array.unsafe_set(cpu.v, opcode->X->getVariable, vy)
      cpu
    }
  | OR_Vx_Vy => {
      let _or = lor(
        Uint8Array.unsafe_get(cpu.v, opcode->X->getVariable),
        Uint8Array.unsafe_get(cpu.v, opcode->Y->getVariable),
      )
      Uint8Array.unsafe_set(cpu.v, opcode->X->getVariable, _or)
      cpu
    }
  | AND_Vx_Vy => {
      let _and = land(
        Uint8Array.unsafe_get(cpu.v, opcode->X->getVariable),
        Uint8Array.unsafe_get(cpu.v, opcode->Y->getVariable),
      )
      Uint8Array.unsafe_set(cpu.v, opcode->X->getVariable, _and)
      cpu
    }
  | XOR_Vx_Vy => {
      let _xor = lxor(
        Uint8Array.unsafe_get(cpu.v, opcode->X->getVariable),
        Uint8Array.unsafe_get(cpu.v, opcode->Y->getVariable),
      )
      Uint8Array.unsafe_set(cpu.v, opcode->X->getVariable, _xor)
      cpu
    }
  | ADD_Vx_Vy => {
      let (x, y) = (
        Uint8Array.unsafe_get(cpu.v, opcode->X->getVariable),
        Uint8Array.unsafe_get(cpu.v, opcode->Y->getVariable),
      )
      Uint8Array.unsafe_set(cpu.v, opcode->X->getVariable, x + y)
      Uint8Array.unsafe_set(cpu.v, 0xf, x + y > 0xff ? 1 : 0)
      cpu
    }
  | SUB_Vx_Vy => {
      let (x, y) = (
        Uint8Array.unsafe_get(cpu.v, opcode->X->getVariable),
        Uint8Array.unsafe_get(cpu.v, opcode->Y->getVariable),
      )
      Uint8Array.unsafe_set(cpu.v, opcode->X->getVariable, x - y)
      Uint8Array.unsafe_set(cpu.v, 0xf, x > y ? 1 : 0)
      cpu
    }
  | SHR_Vx_Vy => {
      let x = Uint8Array.unsafe_get(cpu.v, opcode->X->getVariable)
      Uint8Array.unsafe_set(cpu.v, opcode->X->getVariable, lsr(x, 1))
      Uint8Array.unsafe_set(cpu.v, 0xf, land(x, 1) === 1 ? 1 : 0)
      cpu
    }
  | SUBN_Vx_Vy => {
      let (x, y) = (
        Uint8Array.unsafe_get(cpu.v, opcode->X->getVariable),
        Uint8Array.unsafe_get(cpu.v, opcode->Y->getVariable),
      )
      Uint8Array.unsafe_set(cpu.v, opcode->X->getVariable, y - x)
      Uint8Array.unsafe_set(cpu.v, 0xf, y > x ? 1 : 0)
      cpu
    }
  | SHL_Vx => {
      let x = Uint8Array.unsafe_get(cpu.v, opcode->X->getVariable)
      Uint8Array.unsafe_set(cpu.v, opcode->X->getVariable, lsl(x, 1))
      Uint8Array.unsafe_set(cpu.v, 0xf, lsr(x, 7))
      cpu
    }
  | SNE_Vx_Vy => {
      ...cpu,
      pc: Uint8Array.unsafe_get(cpu.v, opcode->X->getVariable) !==
        Uint8Array.unsafe_get(cpu.v, opcode->Y->getVariable)
        ? cpu.pc + 2
        : cpu.pc,
    }
  | LD_I_addr => {...cpu, i: opcode->NNN->getVariable}
  | JP_V0_addr => {...cpu, pc: opcode->NNN->getVariable + Uint8Array.unsafe_get(cpu.v, 0)}
  | RND_Vx_byte =>
    Uint8Array.unsafe_set(
      cpu.v,
      opcode->X->getVariable,
      land(Js.Math.random_int(0, 0xff), opcode->KK->getVariable),
    )
    cpu
  // | _ => raise(Not_found)
  }
