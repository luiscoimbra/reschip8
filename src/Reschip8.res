let rom = Lazy.force(RomLoader.load)->RomLoader.toBuffer

CPU.init(rom)
