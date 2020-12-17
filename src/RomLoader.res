let load = lazy {
  Js.log("reading dir")
  Node.Fs.readFileSync("./roms/MAZE", #hex)
}

let toBuffer = hex => Js.String.match_(%re("/.{1,2}/g"), hex)
