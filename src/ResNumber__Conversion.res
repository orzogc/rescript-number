module type NumberFromInt = {
  type t

  let fromInt: int => option<t>

  let fromIntExn: int => t

  let fromIntClamped: int => t

  let fromIntUnsafe: int => t
}

module type NumberToInt = {
  type t

  let toInt: t => option<int>

  let toIntExn: t => int

  let toIntClamped: t => int

  let toIntUnsafe: t => int
}

module type NumberFromFloat = {
  type t

  let fromFloat: float => option<t>

  let fromFloatExn: float => t

  let fromFloatClamped: float => t

  let fromFloatUnsafe: float => t
}

module type NumberToFloat = {
  type t

  let toFloat: t => float
}

module type NumberFromString = {
  type t

  let fromString: string => option<t>

  let fromStringExn: string => t
}

module type NumberToString = {
  type t

  let toString: t => string

  let toStringWithRadixExn: (t, ~radix: int) => string

  let toExponential: t => string

  let toExponentialWithPrecisionExn: (t, ~digits: int) => string

  let toPrecision: t => string

  let toPrecisionWithPrecisionExn: (t, ~digits: int) => string
}

module type NumberConversion = {
  type t

  include NumberFromInt with type t := t

  include NumberToInt with type t := t

  include NumberFromFloat with type t := t

  include NumberToFloat with type t := t

  include NumberFromString with type t := t

  include NumberToString with type t := t
}

module type IntegerFromString = {
  type t

  let fromStringWithRadix: (string, ~radix: int) => option<t>

  let fromStringWithRadixExn: (string, ~radix: int) => t
}

module type FloatToString = {
  type t

  let toFixed: t => string

  let toFixedWithPrecisionExn: (t, ~digits: int) => string
}
