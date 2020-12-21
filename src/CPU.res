type address = int

type uiMap = Js.Array.t<Js.Array.t<int>>

let memory_offset = 0x200

type t = {
  // Chip8 is capable of access up to 4kb of memory
  // Interpreter uses the first 512 bytes. from 0x000 to 0x1FF
  // The rest of the memory is reserved for the program data > 0x200
  memory: Js_typed_array.Uint8Array.t,
  // REGISTERS
  // 16 General Purpose 8-bit (Vx) from V0 to VF
  v: Js_typed_array.Uint8Array.t,
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
  stack: Js_typed_array.Uint16Array.t,
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
  | X(code) => code->Pervasives.land(0x0f00)->Pervasives.lsr(8)
  | Y(code) => code->Pervasives.land(0x00f0)->Pervasives.lsr(4)
  | NNN(code) => code->Pervasives.land(0xfff)
  | KK(code) => code->Pervasives.land(0x00ff)
  | N(code) => code->Pervasives.land(0x000f)
  }

let getEmpty = {
  memory: Js_typed_array.Uint8Array.fromLength(4096),
  v: Js_typed_array.Uint8Array.fromLength(16),
  i: 0,
  dt: 0,
  st: 0,
  pc: memory_offset,
  sp: 0,
  stack: Js_typed_array.Uint16Array.fromLength(16),
  ui: Belt.Array.make(32, Belt.Array.make(64, 0)),
  key: -1,
  halted: false,
}

type key = Number(int) | A | B | C | D | E | F

let fontSet = Js_typed_array.Uint8Array.make([
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
  for i in 0 to Js_typed_array.Uint8Array.length(fontSet) {
    Js_typed_array.Uint8Array.unsafe_set(
      memory,
      i,
      Js_typed_array.Uint8Array.unsafe_get(fontSet, i),
    )
  }
  {...cpu, memory: memory}
}

// We will have a CPU with rom data fully stored
let loadRom = romBuffer => {
  switch romBuffer {
  | Some(rom) => {
      let {memory} = getEmpty
      for i in 0 to Js.Array.length(rom) - 1 {
        Js_typed_array.Uint8Array.unsafe_set(
          memory,
          memory_offset + i,
          int_of_string("0x" ++ rom[i]),
        )
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
  let codes = [
    Js_typed_array.Uint8Array.unsafe_get(memory, pc),
    Js_typed_array.Uint8Array.unsafe_get(memory, pc + 1),
  ]
  ({...cpu, pc: pc + 2}, Pervasives.lsl(codes[0], 8) + codes[1])
}
let decode = (cpu, opcode) => (cpu, opcode)
// // let a = Instruction.find(opcode)
// Js.log(a)
// (cpu, a)

// let execute = (cpu, instructions) => instructions(cpu)

let setV = (cpu, value) => {
  Js.log(value)
  cpu
}
