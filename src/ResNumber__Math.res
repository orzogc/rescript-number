module type SignedMath = {
  type t

  let neg: t => option<t>

  let negExn: t => t

  let negUnsafe: t => t

  let abs: t => option<t>

  let absExn: t => t

  let absUnsafe: t => t

  let sign: t => option<ResNumber__Others.sign>

  let signExn: t => ResNumber__Others.sign

  let signRaw: t => float
}

module type IntegerMath = {
  type t

  let clz: t => int

  let imul: (t, t) => t

  let pow: (~base: t, ~exp: t) => option<t>

  let powExn: (~base: t, ~exp: t) => t

  let powUnsafe: (~base: t, ~exp: t) => t

  let random: (t, t) => t
}

module type FloatMath = {
  type t

  include SignedMath with type t := t

  let acos: t => t

  let acosh: t => t

  let asin: t => t

  let asinh: t => t

  let atan: t => t

  let atan2: (~y: t, ~x: t) => t

  let atanh: t => t

  let cbrt: t => t

  let ceil: t => t

  let ceilInt: t => option<int>

  let ceilIntExn: t => int

  let ceilIntClamped: t => int

  let ceilIntUnsafe: t => int

  let cos: t => t

  let cosh: t => t

  let exp: t => t

  let expm1: t => t

  let floor: t => t

  let floorInt: t => option<int>

  let floorIntExn: t => int

  let floorIntClamped: t => int

  let floorIntUnsafe: t => int

  let fround: t => t

  let hypot: (t, t) => t

  let hypotMany: array<t> => t

  let log: t => t

  let log10: t => t

  let log1p: t => t

  let log2: t => t

  let pow: (~base: t, ~exp: t) => t

  let random: unit => t

  let randomRange: (t, t) => t

  let round: t => t

  let roundInt: t => option<int>

  let roundIntExn: t => int

  let roundIntClamped: t => int

  let roundIntUnsafe: t => int

  let sin: t => t

  let sinh: t => t

  let sqrt: t => t

  let tan: t => t

  let tanh: t => t

  let trunc: t => t

  let truncInt: t => option<int>

  let truncIntExn: t => int

  let truncIntClamped: t => int

  let truncIntUnsafe: t => int
}
