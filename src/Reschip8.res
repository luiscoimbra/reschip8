let rom = Lazy.force(RomLoader.load)

// Js.log(rom->RomLoader.toBuffer)

let (cpu, code) = rom->RomLoader.toBuffer->CPU.loadRom->CPU.fetch

Js.log(Instruction.find(0x6100))

let a = Instruction.find(0x6000)

let b = switch a {
| Some(v) =>
  switch v.t {
  | LD_Vx_byte => "is load vx"
  }
| None => "Instruction not found"
}

b->Js.log
