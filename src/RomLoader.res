let load = lazy {
  Js.log("Reading ROM from disk")
  Node.Fs.readFileSync("./roms/MAZE", #hex)
}

let toBuffer = hex => Js.String.match_(%re("/.{1,2}/g"), hex)
