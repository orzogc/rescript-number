open ResNumber__Bitwise
open ResNumber__Conversion
open ResNumber__Math
open ResNumber__Number
open ResNumber__Operation

module type Integer = {
  type t

  include NumberConversion with type t := t

  include Number with type t := t

  include NumberIncDec with type t := t
}

module type IntegerSign = {
  let isSigned: bool

  let isUnsigned: bool
}

module type FixedBitsInteger = {
  type t

  include IntegerSign

  include NumberBits

  include Integer with type t := t

  include NumberBitwise with type t := t

  include IntegerMath with type t := t
}

module type SignedInteger = {
  type t

  include FixedBitsInteger with type t := t

  include SignedMath with type t := t
}

module type UnsignedInteger = {
  type t

  include FixedBitsInteger with type t := t
}

module MakeIntegerConversion: (
  IntModule: {
    type t = float

    include NumberRange with type t := t

    let fromIntUnsafe: int => t

    let fromFloatUnsafe: float => t
  },
) => (NumberConversion with type t = float)

module MakeNumberIncDec: (
  M: {
    type t

    let one: t

    include NumberAddition with type t := t

    include NumberSubtraction with type t := t
  },
) => (NumberIncDec with type t = M.t)

module MakeInteger: (
  IntModule: {
    type t = float

    include NumberRange with type t := t

    let fromIntUnsafe: int => t

    let fromFloatUnsafe: float => t
  },
) => (Integer with type t = float)

module MakeFixedBitsIntegerConversion: (IntRange: NumberRange with type t = int) =>
(NumberConversion with type t = int)

module MakeFixedBitsInt: (IntRange: NumberRange with type t = int) => (Integer with type t = int)

module MakeIntegerMath: (
  M: {
    type t

    include NumberFromFloat with type t := t

    include NumberToFloat with type t := t

    let clz: t => int

    let imul: (t, t) => t
  },
) => (IntegerMath with type t = M.t)

module MakeFixedBitsInteger: (
  M: {
    type t = int

    let isSigned: bool

    include NumberBits

    include NumberRange with type t := t
  },
) => (FixedBitsInteger with type t = int)

module MakeSignedInteger: (
  M: {
    type t = int

    include NumberBits

    include NumberRange with type t := t
  },
) => (SignedInteger with type t = int)

module MakeUnsignedInteger: (
  M: {
    type t = int

    include NumberBits

    include NumberRange with type t := t
  },
) => (UnsignedInteger with type t = int)

module Int8: SignedInteger with type t = int

module Uint8: UnsignedInteger with type t = int

module Int16: SignedInteger with type t = int

module Uint16: UnsignedInteger with type t = int

module Int32: SignedInteger with type t = int

module Uint32: UnsignedInteger with type t = float
