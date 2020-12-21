// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Caml_option = require("bs-platform/lib/js/caml_option.js");

var instructionSet = [
  {
    pattern: 0,
    mask: 61440,
    t: /* SYS */0
  },
  {
    pattern: 224,
    mask: 65535,
    t: /* CLS */1
  },
  {
    pattern: 238,
    mask: 65535,
    t: /* RET */2
  },
  {
    pattern: 4096,
    mask: 61440,
    t: /* JP_addr */3
  },
  {
    pattern: 8192,
    mask: 61440,
    t: /* CALL_addr */4
  },
  {
    pattern: 12288,
    mask: 61440,
    t: /* SE_Vx_byte */5
  },
  {
    pattern: 16384,
    mask: 61440,
    t: /* SNE_Vx_byte */6
  },
  {
    pattern: 20480,
    mask: 61455,
    t: /* SE_Vx_Vy */7
  },
  {
    pattern: 24576,
    mask: 61440,
    t: /* LD_Vx_byte */8
  },
  {
    pattern: 28672,
    mask: 61440,
    t: /* ADD_Vx_byte */9
  },
  {
    pattern: 32768,
    mask: 61455,
    t: /* LD_Vx_Vy */10
  },
  {
    pattern: 32769,
    mask: 61455,
    t: /* OR_Vx_Vy */11
  },
  {
    pattern: 32770,
    mask: 61455,
    t: /* AND_Vx_Vy */12
  },
  {
    pattern: 32771,
    mask: 61455,
    t: /* XOR_Vx_Vy */13
  },
  {
    pattern: 32772,
    mask: 61455,
    t: /* ADD_Vx_Vy */14
  },
  {
    pattern: 32773,
    mask: 61455,
    t: /* SUB_Vx_Vy */15
  },
  {
    pattern: 32774,
    mask: 61455,
    t: /* SHR_Vx_Vy */16
  },
  {
    pattern: 32775,
    mask: 61455,
    t: /* SUBN_Vx_Vy */17
  },
  {
    pattern: 32782,
    mask: 61455,
    t: /* SHL_Vx */18
  },
  {
    pattern: 36864,
    mask: 61455,
    t: /* SNE_Vx_Vy */19
  },
  {
    pattern: 40960,
    mask: 61440,
    t: /* LD_I_addr */20
  },
  {
    pattern: 45056,
    mask: 61440,
    t: /* JP_V0_addr */21
  },
  {
    pattern: 49152,
    mask: 61440,
    t: /* RND_Vx_byte */22
  },
  {
    pattern: 53248,
    mask: 61440,
    t: /* DRW_Vx_Vy_n */23
  },
  {
    pattern: 57502,
    mask: 61695,
    t: /* SKP_Vx */24
  },
  {
    pattern: 57505,
    mask: 61695,
    t: /* SKNP_Vx */25
  },
  {
    pattern: 61447,
    mask: 61695,
    t: /* LD_Vx_DT */26
  },
  {
    pattern: 61450,
    mask: 61695,
    t: /* LD_Vx_K */27
  },
  {
    pattern: 61461,
    mask: 61695,
    t: /* LD_DT_Vx */28
  },
  {
    pattern: 61464,
    mask: 61695,
    t: /* LD_ST_Vx */29
  },
  {
    pattern: 61470,
    mask: 61695,
    t: /* ADD_I_Vx */30
  },
  {
    pattern: 61481,
    mask: 61695,
    t: /* LD_F_Vx */31
  },
  {
    pattern: 61491,
    mask: 61695,
    t: /* LD_B_Vx */32
  },
  {
    pattern: 61525,
    mask: 61695,
    t: /* LD_I_Vx */33
  },
  {
    pattern: 61541,
    mask: 61695,
    t: /* LD_Vx_I */34
  }
];

function find(opcode) {
  return Caml_option.undefined_to_opt(instructionSet.find(function (i) {
                  return (opcode & i.mask) === i.pattern;
                }));
}

exports.instructionSet = instructionSet;
exports.find = find;
/* No side effect */
