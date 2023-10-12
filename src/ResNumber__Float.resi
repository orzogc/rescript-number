open ResNumber__Conversion
open ResNumber__Math
open ResNumber__Number

module type FloatExtra = {
  type t

  let isFinite: t => bool

  let isInteger: t => bool

  let isNaN: t => bool

  let isSafeInteger: t => bool
}

module type Float = {
  type t

  include NumberConversion with type t := t

  include FloatToString with type t := t

  include Number with type t := t

  include FloatMath with type t := t

  include FloatExtra with type t := t
}

module Float64: Float with type t = float
