type t =
  // | SYS
  | CLS
  | RET
  | JP_addr
  | CALL_addr
  | SE_Vx_byte
  | SNE_Vx_byte
  | SE_Vx_Vy
  | LD_Vx_byte
  | ADD_Vx_byte
  | LD_Vx_Vy
  | OR_Vx_Vy
  | AND_Vx_Vy
  | XOR_Vx_Vy
  | ADD_Vx_Vy
  | SUB_Vx_Vy
  | SHR_Vx_Vy
  | SUBN_Vx_Vy
  | SHL_Vx
  | SNE_Vx_Vy
  | LD_I_addr
  | JP_V0_addr
  | RND_Vx_byte
  | DRW_Vx_Vy_n
  | SKP_Vx
  | SKNP_Vx
  | LD_Vx_DT
  | LD_Vx_K
  | LD_DT_Vx
  | LD_ST_Vx
  | ADD_I_Vx
  | LD_F_Vx
  | LD_B_Vx
  | LD_I_Vx
  | LD_Vx_I

type rec instruction = {pattern: int, mask: int, t: t}

let instructionSet: array<instruction> = [
  // {t: SYS, pattern: 0x0000, mask: 0xf000},
  {t: CLS, pattern: 0x00e0, mask: 0xffff},
  {t: RET, pattern: 0x00ee, mask: 0xffff},
  {t: JP_addr, pattern: 0x1000, mask: 0xf000},
  {t: CALL_addr, pattern: 0x2000, mask: 0xf000},
  {t: SE_Vx_byte, pattern: 0x3000, mask: 0xf000},
  {t: SNE_Vx_byte, pattern: 0x4000, mask: 0xf000},
  {t: SE_Vx_Vy, pattern: 0x5000, mask: 0xf00f},
  {t: LD_Vx_byte, pattern: 0x6000, mask: 0xf000},
  {t: ADD_Vx_byte, pattern: 0x7000, mask: 0xf000},
  {t: LD_Vx_Vy, pattern: 0x8000, mask: 0xf00f},
  {t: OR_Vx_Vy, pattern: 0x8001, mask: 0xf00f},
  {t: AND_Vx_Vy, pattern: 0x8002, mask: 0xf00f},
  {t: XOR_Vx_Vy, pattern: 0x8003, mask: 0xf00f},
  {t: ADD_Vx_Vy, pattern: 0x8004, mask: 0xf00f},
  {t: SUB_Vx_Vy, pattern: 0x8005, mask: 0xf00f},
  {t: SHR_Vx_Vy, pattern: 0x8006, mask: 0xf00f},
  {t: SUBN_Vx_Vy, pattern: 0x8007, mask: 0xf00f},
  {t: SHL_Vx, pattern: 0x800e, mask: 0xf00f},
  {t: SNE_Vx_Vy, pattern: 0x9000, mask: 0xf00f},
  {t: LD_I_addr, pattern: 0xa000, mask: 0xf000},
  {t: JP_V0_addr, pattern: 0xb000, mask: 0xf000},
  {t: RND_Vx_byte, pattern: 0xc000, mask: 0xf000},
  {t: DRW_Vx_Vy_n, pattern: 0xd000, mask: 0xf000},
  {t: SKP_Vx, pattern: 0xe09e, mask: 0xf0ff},
  {t: SKNP_Vx, pattern: 0xe0a1, mask: 0xf0ff},
  {t: LD_Vx_DT, pattern: 0xf007, mask: 0xf0ff},
  {t: LD_Vx_K, pattern: 0xf00a, mask: 0xf0ff},
  {t: LD_DT_Vx, pattern: 0xf015, mask: 0xf0ff},
  {t: LD_ST_Vx, pattern: 0xf018, mask: 0xf0ff},
  {t: ADD_I_Vx, pattern: 0xf01e, mask: 0xf0ff},
  {t: LD_F_Vx, pattern: 0xf029, mask: 0xf0ff},
  {t: LD_B_Vx, pattern: 0xf033, mask: 0xf0ff},
  {t: LD_I_Vx, pattern: 0xf055, mask: 0xf0ff},
  {t: LD_Vx_I, pattern: 0xf065, mask: 0xf0ff},
]

let get = opcode =>
  switch Js.Array.find(i => Pervasives.land(opcode, i.mask) === i.pattern, instructionSet) {
  | Some(i) => (opcode, i.t)
  | None => raise(Not_found)
  }
