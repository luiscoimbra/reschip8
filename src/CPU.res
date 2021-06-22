open Js_typed_array

type address = int

type uiMap = array<array<int>>

let memory_offset = 0x200
let fps = 15 //ms

type dimension = {width: int, height: int}
let uiDimension = {
  width: 64,
  height: 32,
}

type keyStatus = Up | Down

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
  keys: array<keyStatus>,
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
  keys: Belt.Array.make(16, Down),
  halted: false,
}

type key = Number(int) | A | B | C | D | E | F

let loadFontSet = cpu => {
  Js.log("Loading fontset")
  let {memory} = cpu
  for i in 0 to Uint8Array.length(Fontset.get) {
    Uint8Array.unsafe_set(memory, i, Uint8Array.unsafe_get(Fontset.get, i))
  }
  {...cpu, memory: memory}
}

exception Rom_not_found

// We will have a CPU with rom data fully stored
let loadRom = romBuffer => {
  Js.log("Adding rom to memory")
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
  | None => raise(Rom_not_found)
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
  | DRW_Vx_Vy_n => {
      let (x, y, n) = (
        Uint8Array.unsafe_get(cpu.v, opcode->X->getVariable),
        Uint8Array.unsafe_get(cpu.v, opcode->Y->getVariable),
        opcode->N->getVariable,
      )
      Uint8Array.unsafe_set(cpu.v, 0xf, 0)
      for spriteX in 0 to n {
        let spriteRow = Uint8Array.unsafe_get(cpu.memory, cpu.i + spriteX)
        for bit in 0 to 7 {
          let pixel = lsr(land(spriteRow, lsl(1, 7 - bit)), 7 - bit)
          let (w, h) = (mod(x + bit, 64), mod(y + spriteX, 32))
          if land(cpu.ui[h][w], pixel) === 1 {
            Uint8Array.unsafe_set(cpu.v, 0xf, 1)
          }
          cpu.ui[h][w] = lxor(cpu.ui[h][w], pixel)
        }
      }
      cpu
    }
  | SKP_Vx => {
      let x = Uint8Array.unsafe_get(cpu.v, opcode->X->getVariable)
      switch cpu.keys[x] {
      | Down => {...cpu, pc: cpu.pc + 2}
      | Up => cpu
      }
    }
  | SKNP_Vx => {
      let x = Uint8Array.unsafe_get(cpu.v, opcode->X->getVariable)
      switch cpu.keys[x] {
      | Up => {...cpu, pc: cpu.pc + 2}
      | Down => cpu
      }
    }
  | LD_Vx_DT => {
      Uint8Array.unsafe_set(cpu.v, opcode->X->getVariable, cpu.dt)
      cpu
    }
  | LD_Vx_K => {...cpu, halted: true}
  | LD_DT_Vx => {...cpu, dt: Uint8Array.unsafe_get(cpu.v, opcode->X->getVariable)}
  | LD_ST_Vx => {...cpu, st: Uint8Array.unsafe_get(cpu.v, opcode->X->getVariable)}
  | ADD_I_Vx => {...cpu, i: cpu.i + Uint8Array.unsafe_get(cpu.v, opcode->X->getVariable)}
  | LD_F_Vx => {...cpu, i: Uint8Array.unsafe_get(cpu.v, opcode->X->getVariable) * 5}
  | LD_B_Vx => {
      let x = Uint8Array.unsafe_get(cpu.v, opcode->X->getVariable)
      let (hundreds, tens, ones) = (mod(x / 100, 10) * 100, mod(x / 10, 10) * 10, mod(x / 1, 10))
      Uint8Array.unsafe_set(cpu.v, cpu.i, hundreds)
      Uint8Array.unsafe_set(cpu.v, cpu.i + 1, tens)
      Uint8Array.unsafe_set(cpu.v, cpu.i + 2, ones)
      cpu
    }
  | LD_I_Vx => {
      for v in 0 to Uint8Array.unsafe_get(cpu.v, opcode->X->getVariable) {
        Uint8Array.unsafe_set(cpu.memory, cpu.i + v, Uint8Array.unsafe_get(cpu.v, v))
      }
      cpu
    }
  | LD_Vx_I => {
      for v in 0 to Uint8Array.unsafe_get(cpu.v, opcode->X->getVariable) {
        Uint8Array.unsafe_set(cpu.v, v, Uint8Array.unsafe_get(cpu.memory, cpu.i + v))
      }
      cpu
    }
  }

let rec cycle = cpu => {
  let (cpu, opcode) = cpu->fetch
  // opcode->Js.Int.toStringWithRadix(~radix=16)->Js.log
  let decoded = opcode->decode

  
  let cpu = cpu->execute(decoded)

  // ConsoleInterface.draw(cpu.ui)
  // Js.log(decoded)
  let _ = Js.Global.setTimeout(() => cycle(cpu), 200)
}

let init = rom => {
  Js.log("Starting CPU")
  // rom->Js.log
  // ConsoleInterface.init()
  rom->loadRom->loadFontSet->cycle
}
