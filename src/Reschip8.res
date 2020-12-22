let rom = Lazy.force(RomLoader.load)

let (cpu, code) = rom->RomLoader.toBuffer->CPU.loadRom->CPU.fetch

Js.log(code)

// let (opcode, instruction) = CPU.decode(0x6000)

CPU.execute(cpu, CPU.decode(0x00ee))->Js.log

// let b = switch a {
// | Some(v) =>
//   switch v.t {
//   | LD_Vx_byte => "is load vx"
//   | _ => "NOT IMPLEMENTED"
//   }
// | None => "Instruction not found"
// }

// b->Js.log
