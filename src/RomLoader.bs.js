// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Caml_option = require("bs-platform/lib/js/caml_option.js");

var load = {
  LAZY_DONE: false,
  VAL: (function () {
      console.log("reading dir");
      return Fs.readFileSync("./roms/MAZE", "hex");
    })
};

function toBuffer(hex) {
  return Caml_option.null_to_opt(hex.match(/.{1,2}/g));
}

exports.load = load;
exports.toBuffer = toBuffer;
/* fs Not a pure module */