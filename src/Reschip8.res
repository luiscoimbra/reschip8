let rom = Lazy.force(RomLoader.load)->RomLoader.toBuffer

rom->Js.log
"cool ha"->Js.log

CPU.init(rom)
