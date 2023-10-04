open ResNumber__Conversion
open ResNumber__Math
open ResNumber__Number

module type Float = {
  type t

  include NumberConversion with type t := t

  include Number with type t := t

  include FloatMath with type t := t
}

module Float64: Float with type t = float
