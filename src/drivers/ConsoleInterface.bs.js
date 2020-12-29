// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Blessed = require("blessed");
var Caml_array = require("bs-platform/lib/js/caml_array.js");

var screen = Blessed.screen();

screen.render();

function init(param) {
  
}

function draw(ui) {
  for(var y = 0; y <= 31; ++y){
    for(var x = 0; x <= 63; ++x){
      if (Caml_array.get(Caml_array.get(ui, y), x) === 1) {
        screen.fillRegion("5", " ", x, x + 2 | 0, y, y + 1 | 0);
      } else {
        screen.fillRegion("2", " ", x, x + 32 | 0, y, y + 1 | 0);
      }
    }
  }
  return screen.render();
}

exports.screen = screen;
exports.init = init;
exports.draw = draw;
/* screen Not a pure module */
