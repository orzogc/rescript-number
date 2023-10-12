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

  let acos: t => option<t>

  let acosExn: t => t

  let acosUnsafe: t => t

  let acosh: t => option<t>

  let acoshExn: t => t

  let acoshUnsafe: t => t

  let asin: t => option<t>

  let asinExn: t => t

  let asinUnsafe: t => t

  let asinh: t => t

  let atan: t => t

  let atan2: (~y: t, ~x: t) => t

  let atanh: t => option<t>

  let atanhExn: t => t

  let atanhUnsafe: t => t

  let cbrt: t => t

  let ceil: t => t

  let ceilInt: t => option<int>

  let ceilIntExn: t => int

  let ceilIntClamped: t => int

  let ceilIntUnsafe: t => int

  let cos: t => option<t>

  let cosExn: t => t

  let cosUnsafe: t => t

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

  let log: t => option<t>

  let logExn: t => t

  let logUnsafe: t => t

  let log10: t => option<t>

  let log10Exn: t => t

  let log10Unsafe: t => t

  let log1p: t => option<t>

  let log1pExn: t => t

  let log1pUnsafe: t => t

  let log2: t => option<t>

  let log2Exn: t => t

  let log2Unsafe: t => t

  let pow: (~base: t, ~exp: t) => option<t>

  let powExn: (~base: t, ~exp: t) => t

  let powUnsafe: (~base: t, ~exp: t) => t

  let random: unit => t

  let randomRange: (t, t) => t

  let round: t => t

  let roundInt: t => option<int>

  let roundIntExn: t => int

  let roundIntClamped: t => int

  let roundIntUnsafe: t => int

  let sin: t => option<t>

  let sinExn: t => t

  let sinUnsafe: t => t

  let sinh: t => t

  let sqrt: t => option<t>

  let sqrtExn: t => t

  let sqrtUnsafe: t => t

  let tan: t => option<t>

  let tanExn: t => t

  let tanUnsafe: t => t

  let tanh: t => t

  let trunc: t => t

  let truncInt: t => option<int>

  let truncIntExn: t => int

  let truncIntClamped: t => int

  let truncIntUnsafe: t => int
}
