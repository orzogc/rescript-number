module type NumberBits = {
  let bits: int
}

module type NumberBitwise = {
  type t

  let lnot: t => t

  let land: (t, t) => t

  let lor: (t, t) => t

  let lxor: (t, t) => t

  let lsl: (t, int) => t

  let lsr: (t, int) => t

  let asr: (t, int) => t

  let rsl: (t, int) => t

  let rsr: (t, int) => t
}
