


let a = Lazy.force(RomLoader.load)

Js.log(a->RomLoader.toBuffer)

let romLoaded = a->RomLoader.toBuffer->CPU.loadRom

Js.log(Js_typed_array.Uint8Array.unsafe_get(romLoaded.memory, 549))
