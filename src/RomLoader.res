let load = lazy {
  // Js.log("Reading ROM from disk")
  // Node.Fs.readFileSync("./roms/MAZE", #hex)
  Js.log("Reading temporary from memory")
  "60006100a222c2013201a21ed0147004304012046000710431201204121c8040201020408010"
}

let toBuffer = hex => Js.String.match_(%re("/.{1,2}/g"), hex)
