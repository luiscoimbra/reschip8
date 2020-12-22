// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Js_math = require("bs-platform/lib/js/js_math.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");
var Instruction = require("./Instruction.bs.js");

function getVariable(opcode) {
  switch (opcode.TAG | 0) {
    case /* KK */0 :
        return opcode._0 & 255;
    case /* N */1 :
        return opcode._0 & 15;
    case /* NNN */2 :
        return opcode._0 & 4095;
    case /* X */3 :
        return ((opcode._0 & 3840) >>> 8);
    case /* Y */4 :
        return ((opcode._0 & 240) >>> 4);
    
  }
}

var getEmpty_memory = new Uint8Array(4096);

var getEmpty_v = new Uint8Array(16);

var getEmpty_stack = new Uint16Array(16);

var getEmpty_ui = Belt_Array.make(32, Belt_Array.make(64, 0));

var getEmpty = {
  memory: getEmpty_memory,
  v: getEmpty_v,
  i: 0,
  dt: 0,
  st: 0,
  pc: 512,
  sp: 0,
  stack: getEmpty_stack,
  ui: getEmpty_ui,
  key: -1,
  halted: false
};

var fontSet = new Uint8Array([
      240,
      144,
      144,
      144,
      240,
      32,
      96,
      32,
      32,
      112,
      240,
      16,
      240,
      128,
      240,
      240,
      16,
      240,
      16,
      240,
      144,
      144,
      240,
      16,
      16,
      240,
      128,
      240,
      16,
      240,
      240,
      128,
      240,
      144,
      240,
      240,
      16,
      32,
      64,
      64,
      240,
      144,
      240,
      144,
      240,
      240,
      144,
      240,
      16,
      240,
      240,
      144,
      240,
      144,
      144,
      224,
      144,
      224,
      144,
      224,
      240,
      128,
      128,
      128,
      240,
      224,
      144,
      144,
      144,
      224,
      240,
      128,
      240,
      128,
      240,
      240,
      128,
      240,
      128,
      128
    ]);

function loadFontSet(cpu) {
  var memory = cpu.memory;
  for(var i = 0 ,i_finish = fontSet.length; i <= i_finish; ++i){
    memory[i] = fontSet[i];
  }
  return {
          memory: memory,
          v: cpu.v,
          i: cpu.i,
          dt: cpu.dt,
          st: cpu.st,
          pc: cpu.pc,
          sp: cpu.sp,
          stack: cpu.stack,
          ui: cpu.ui,
          key: cpu.key,
          halted: cpu.halted
        };
}

function loadRom(romBuffer) {
  if (romBuffer === undefined) {
    return getEmpty;
  }
  var memory = getEmpty_memory;
  for(var i = 0 ,i_finish = romBuffer.length; i < i_finish; ++i){
    memory[512 + i | 0] = Caml_format.caml_int_of_string("0x" + Caml_array.get(romBuffer, i));
  }
  return {
          memory: memory,
          v: getEmpty_v,
          i: 0,
          dt: 0,
          st: 0,
          pc: 512,
          sp: 0,
          stack: getEmpty_stack,
          ui: getEmpty_ui,
          key: -1,
          halted: false
        };
}

function $$fetch(cpu) {
  var pc = cpu.pc;
  var memory = cpu.memory;
  var codes = [
    memory[pc],
    memory[pc + 1 | 0]
  ];
  return [
          {
            memory: cpu.memory,
            v: cpu.v,
            i: cpu.i,
            dt: cpu.dt,
            st: cpu.st,
            pc: pc + 2 | 0,
            sp: cpu.sp,
            stack: cpu.stack,
            ui: cpu.ui,
            key: cpu.key,
            halted: cpu.halted
          },
          (Caml_array.get(codes, 0) << 8) + Caml_array.get(codes, 1) | 0
        ];
}

var decode = Instruction.get;

function execute(cpu, param) {
  var opcode = param[0];
  switch (param[1]) {
    case /* CLS */0 :
        return {
                memory: cpu.memory,
                v: cpu.v,
                i: cpu.i,
                dt: cpu.dt,
                st: cpu.st,
                pc: cpu.pc,
                sp: cpu.sp,
                stack: cpu.stack,
                ui: Belt_Array.make(32, Belt_Array.make(64, 0)),
                key: cpu.key,
                halted: cpu.halted
              };
    case /* RET */1 :
        return {
                memory: cpu.memory,
                v: cpu.v,
                i: cpu.i,
                dt: cpu.dt,
                st: cpu.st,
                pc: cpu.stack[cpu.sp],
                sp: cpu.sp - 1 | 0,
                stack: cpu.stack,
                ui: cpu.ui,
                key: cpu.key,
                halted: cpu.halted
              };
    case /* JP_addr */2 :
        return {
                memory: cpu.memory,
                v: cpu.v,
                i: cpu.i,
                dt: cpu.dt,
                st: cpu.st,
                pc: getVariable({
                      TAG: /* NNN */2,
                      _0: opcode
                    }),
                sp: cpu.sp,
                stack: cpu.stack,
                ui: cpu.ui,
                key: cpu.key,
                halted: cpu.halted
              };
    case /* CALL_addr */3 :
        var sp = cpu.sp + 1 | 0;
        cpu.stack[sp] = cpu.pc;
        return {
                memory: cpu.memory,
                v: cpu.v,
                i: cpu.i,
                dt: cpu.dt,
                st: cpu.st,
                pc: getVariable({
                      TAG: /* NNN */2,
                      _0: opcode
                    }),
                sp: sp,
                stack: cpu.stack,
                ui: cpu.ui,
                key: cpu.key,
                halted: cpu.halted
              };
    case /* SE_Vx_byte */4 :
        if (cpu.v[getVariable({
                    TAG: /* X */3,
                    _0: opcode
                  })] === getVariable({
                TAG: /* KK */0,
                _0: opcode
              })) {
          return {
                  memory: cpu.memory,
                  v: cpu.v,
                  i: cpu.i,
                  dt: cpu.dt,
                  st: cpu.st,
                  pc: cpu.pc + 2 | 0,
                  sp: cpu.sp,
                  stack: cpu.stack,
                  ui: cpu.ui,
                  key: cpu.key,
                  halted: cpu.halted
                };
        } else {
          return cpu;
        }
    case /* SNE_Vx_byte */5 :
        if (cpu.v[getVariable({
                    TAG: /* X */3,
                    _0: opcode
                  })] !== getVariable({
                TAG: /* KK */0,
                _0: opcode
              })) {
          return {
                  memory: cpu.memory,
                  v: cpu.v,
                  i: cpu.i,
                  dt: cpu.dt,
                  st: cpu.st,
                  pc: cpu.pc + 2 | 0,
                  sp: cpu.sp,
                  stack: cpu.stack,
                  ui: cpu.ui,
                  key: cpu.key,
                  halted: cpu.halted
                };
        } else {
          return cpu;
        }
    case /* SE_Vx_Vy */6 :
        if (cpu.v[getVariable({
                    TAG: /* X */3,
                    _0: opcode
                  })] === cpu.v[getVariable({
                    TAG: /* Y */4,
                    _0: opcode
                  })]) {
          return {
                  memory: cpu.memory,
                  v: cpu.v,
                  i: cpu.i,
                  dt: cpu.dt,
                  st: cpu.st,
                  pc: cpu.pc + 2 | 0,
                  sp: cpu.sp,
                  stack: cpu.stack,
                  ui: cpu.ui,
                  key: cpu.key,
                  halted: cpu.halted
                };
        } else {
          return cpu;
        }
    case /* LD_Vx_byte */7 :
        cpu.v[getVariable({
                  TAG: /* X */3,
                  _0: opcode
                })] = getVariable({
              TAG: /* KK */0,
              _0: opcode
            });
        return cpu;
    case /* ADD_Vx_byte */8 :
        var vx = cpu.v[getVariable({
                  TAG: /* X */3,
                  _0: opcode
                })];
        cpu.v[getVariable({
                  TAG: /* X */3,
                  _0: opcode
                })] = vx + getVariable({
              TAG: /* KK */0,
              _0: opcode
            }) | 0;
        return cpu;
    case /* LD_Vx_Vy */9 :
        var vy = cpu.v[getVariable({
                  TAG: /* Y */4,
                  _0: opcode
                })];
        cpu.v[getVariable({
                  TAG: /* X */3,
                  _0: opcode
                })] = vy;
        return cpu;
    case /* OR_Vx_Vy */10 :
        var _or = cpu.v[getVariable({
                  TAG: /* X */3,
                  _0: opcode
                })] | cpu.v[getVariable({
                  TAG: /* Y */4,
                  _0: opcode
                })];
        cpu.v[getVariable({
                  TAG: /* X */3,
                  _0: opcode
                })] = _or;
        return cpu;
    case /* AND_Vx_Vy */11 :
        var _and = cpu.v[getVariable({
                  TAG: /* X */3,
                  _0: opcode
                })] & cpu.v[getVariable({
                  TAG: /* Y */4,
                  _0: opcode
                })];
        cpu.v[getVariable({
                  TAG: /* X */3,
                  _0: opcode
                })] = _and;
        return cpu;
    case /* XOR_Vx_Vy */12 :
        var _xor = cpu.v[getVariable({
                  TAG: /* X */3,
                  _0: opcode
                })] ^ cpu.v[getVariable({
                  TAG: /* Y */4,
                  _0: opcode
                })];
        cpu.v[getVariable({
                  TAG: /* X */3,
                  _0: opcode
                })] = _xor;
        return cpu;
    case /* ADD_Vx_Vy */13 :
        var x = cpu.v[getVariable({
                  TAG: /* X */3,
                  _0: opcode
                })];
        var y = cpu.v[getVariable({
                  TAG: /* Y */4,
                  _0: opcode
                })];
        cpu.v[getVariable({
                  TAG: /* X */3,
                  _0: opcode
                })] = x + y | 0;
        cpu.v[15] = (x + y | 0) > 255 ? 1 : 0;
        return cpu;
    case /* SUB_Vx_Vy */14 :
        var x$1 = cpu.v[getVariable({
                  TAG: /* X */3,
                  _0: opcode
                })];
        var y$1 = cpu.v[getVariable({
                  TAG: /* Y */4,
                  _0: opcode
                })];
        cpu.v[getVariable({
                  TAG: /* X */3,
                  _0: opcode
                })] = x$1 - y$1 | 0;
        cpu.v[15] = x$1 > y$1 ? 1 : 0;
        return cpu;
    case /* SHR_Vx_Vy */15 :
        var x$2 = cpu.v[getVariable({
                  TAG: /* X */3,
                  _0: opcode
                })];
        cpu.v[getVariable({
                  TAG: /* X */3,
                  _0: opcode
                })] = (x$2 >>> 1);
        cpu.v[15] = (x$2 & 1) === 1 ? 1 : 0;
        return cpu;
    case /* SUBN_Vx_Vy */16 :
        var x$3 = cpu.v[getVariable({
                  TAG: /* X */3,
                  _0: opcode
                })];
        var y$2 = cpu.v[getVariable({
                  TAG: /* Y */4,
                  _0: opcode
                })];
        cpu.v[getVariable({
                  TAG: /* X */3,
                  _0: opcode
                })] = y$2 - x$3 | 0;
        cpu.v[15] = y$2 > x$3 ? 1 : 0;
        return cpu;
    case /* SHL_Vx */17 :
        var x$4 = cpu.v[getVariable({
                  TAG: /* X */3,
                  _0: opcode
                })];
        cpu.v[getVariable({
                  TAG: /* X */3,
                  _0: opcode
                })] = (x$4 << 1);
        cpu.v[15] = (x$4 >>> 7);
        return cpu;
    case /* SNE_Vx_Vy */18 :
        return {
                memory: cpu.memory,
                v: cpu.v,
                i: cpu.i,
                dt: cpu.dt,
                st: cpu.st,
                pc: cpu.v[getVariable({
                          TAG: /* X */3,
                          _0: opcode
                        })] !== cpu.v[getVariable({
                          TAG: /* Y */4,
                          _0: opcode
                        })] ? cpu.pc + 2 | 0 : cpu.pc,
                sp: cpu.sp,
                stack: cpu.stack,
                ui: cpu.ui,
                key: cpu.key,
                halted: cpu.halted
              };
    case /* LD_I_addr */19 :
        return {
                memory: cpu.memory,
                v: cpu.v,
                i: getVariable({
                      TAG: /* NNN */2,
                      _0: opcode
                    }),
                dt: cpu.dt,
                st: cpu.st,
                pc: cpu.pc,
                sp: cpu.sp,
                stack: cpu.stack,
                ui: cpu.ui,
                key: cpu.key,
                halted: cpu.halted
              };
    case /* JP_V0_addr */20 :
        return {
                memory: cpu.memory,
                v: cpu.v,
                i: cpu.i,
                dt: cpu.dt,
                st: cpu.st,
                pc: getVariable({
                      TAG: /* NNN */2,
                      _0: opcode
                    }) + cpu.v[0] | 0,
                sp: cpu.sp,
                stack: cpu.stack,
                ui: cpu.ui,
                key: cpu.key,
                halted: cpu.halted
              };
    case /* RND_Vx_byte */21 :
        cpu.v[getVariable({
                  TAG: /* X */3,
                  _0: opcode
                })] = Js_math.random_int(0, 255) & getVariable({
              TAG: /* KK */0,
              _0: opcode
            });
        return cpu;
    case /* DRW_Vx_Vy_n */22 :
    case /* SKP_Vx */23 :
    case /* SKNP_Vx */24 :
    case /* LD_Vx_DT */25 :
    case /* LD_Vx_K */26 :
    case /* LD_DT_Vx */27 :
    case /* LD_ST_Vx */28 :
    case /* ADD_I_Vx */29 :
    case /* LD_F_Vx */30 :
    case /* LD_B_Vx */31 :
    case /* LD_I_Vx */32 :
    case /* LD_Vx_I */33 :
        break;
    
  }
  throw {
        RE_EXN_ID: "Match_failure",
        _1: [
          "CPU.res",
          187,
          2
        ],
        Error: new Error()
      };
}

var memory_offset = 512;

exports.memory_offset = memory_offset;
exports.getVariable = getVariable;
exports.getEmpty = getEmpty;
exports.fontSet = fontSet;
exports.loadFontSet = loadFontSet;
exports.loadRom = loadRom;
exports.$$fetch = $$fetch;
exports.decode = decode;
exports.execute = execute;
/* getEmpty Not a pure module */
