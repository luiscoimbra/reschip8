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

let getEmpty = (): t => {
  {
    memory: Js_typed_array.Uint8Array.fromLength(4096),
    v: Js_typed_array.Uint8Array.fromLength(16),
    i: 0,
    dt: 0,
    st: 0,
    pc: 200,
    sp: 0,
    stack: Js_typed_array.Uint16Array.fromLength(16),
    ui: Belt.Array.make(32, Belt.Array.make(64, 0)),
    key: -1,
    halted: false,
  }
}

let loadRom = (romBuffer): t => {
  switch romBuffer {
  | Some(rom) => {
      let {memory} = getEmpty()
      for i in 0 to Js.Array.length(rom) - 1 {
        Js_typed_array.Uint8Array.unsafe_set(
          memory,
          memory_offset + i,
          int_of_string("0x" ++ rom[i]),
        )
      }

      {
        ...getEmpty(),
        memory: memory,
      }
    }
  | None => getEmpty()
  }
}

let fetch = () => "impl"
let decode = () => "impl"
let execute = () => "impl"
