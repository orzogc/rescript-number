open Js

open ResNumber__Conversion
open ResNumber__Math
open ResNumber__Number
open ResNumber__Operation
open ResNumber__Others
open ResNumber__Utils

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

let raiseNaN = () => invalid_arg(`NaN`)

module Float64: Float with type t = float = {
  type t = float

  let zero = 0.0

  let one = 1.0

  let minValue = neg_infinity

  let maxValue = infinity

  let toString = f => f->Float.toString

  let fromIntUnsafe = i => i->Int.toFloat

  let fromInt = i => Some(i->fromIntUnsafe)

  let fromIntExn = fromIntUnsafe

  let fromIntClamped = fromIntUnsafe

  let toIntUnsafe = f => f->floatToIntUnsafe->lor(0)

  let toInt = f => f->isInteger && f->inInt32Range ? Some(f->toIntUnsafe) : None

  let toIntExn = f =>
    if !(f->isInteger) {
      invalid_arg(`float number ${f->toString} is not an integer`)
    } else if !(f->inInt32Range) {
      raiseOverflow(f, module(Int32Range))
    } else {
      f->toIntUnsafe
    }

  let toIntClamped = f =>
    if f < Int.min->Int.toFloat {
      Int.min
    } else if f > Int.max->Int.toFloat {
      Int.max
    } else {
      f->toIntUnsafe
    }

  let fromFloat = f => !(f->isNaN) ? Some(f) : None

  let fromFloatExn = f => !(f->isNaN) ? f : raiseNaN()

  let fromFloatClamped = f => !(f->isNaN) ? f : zero

  let fromFloatUnsafe = f => f

  let toFloat = f => f

  let fromString = stringToFloat

  let fromStringExn = s =>
    switch s->fromString {
    | Some(f) => f
    | None => invalid_arg(`the string is not a float number: ${s}`)
    }

  let toStringWithRadixExn = (f, ~radix) => f->Float.toStringWithRadix(~radix)

  let toExponential = f => f->Float.toExponential

  let toExponentialWithPrecisionExn = (f, ~digits) => f->Float.toExponentialWithPrecision(~digits)

  let toPrecision = f => f->Float.toPrecision

  let toPrecisionWithPrecisionExn = (f, ~digits) => f->Float.toPrecisionWithPrecision(~digits)

  let toFixed = f => f->Float.toFixed

  let toFixedWithPrecisionExn = (f, ~digits) => f->Float.toFixedWithPrecision(~digits)

  let compare = (a, b) =>
    !(a->isNaN) && !(b->isNaN)
      ? Some(
          if a < b {
            Less
          } else if a > b {
            Greater
          } else {
            Equal
          },
        )
      : None

  let compareExn = (a, b) =>
    switch compare(a, b) {
    | Some(v) => v
    | None => invalid_arg(`${a->toString} or ${b->toString} is NaN`)
    }

  let eq = (a: t, b: t) => a === b

  let ne = (a, b) => !eq(a, b)

  let lt = (a: t, b: t) => a < b

  let le = (a: t, b: t) => a <= b

  let gt = (a: t, b: t) => a > b

  let ge = (a: t, b: t) => a >= b

  let min = (a: t, b: t) => PervasivesU.min(a, b)

  let minMany = arr => {
    let len = arr->Array2.length

    if len > 0 {
      Some(len === 1 ? arr->Array2.unsafe_get(0) : arr->Math.minMany_float)
    } else {
      None
    }
  }

  let minManyExn = arr => {
    let len = arr->Array2.length

    if len > 0 {
      len === 1 ? arr->Array2.unsafe_get(0) : arr->Math.minMany_float
    } else {
      raiseEmptyArray()
    }
  }

  let minManyUnsafe = arr => arr->Math.minMany_float

  let max = (a: t, b: t) => PervasivesU.max(a, b)

  let maxMany = arr => {
    let len = arr->Array2.length

    if len > 0 {
      Some(len === 1 ? arr->Array2.unsafe_get(0) : arr->Math.maxMany_float)
    } else {
      None
    }
  }

  let maxManyExn = arr => {
    let len = arr->Array2.length

    if len > 0 {
      len === 1 ? arr->Array2.unsafe_get(0) : arr->Math.maxMany_float
    } else {
      raiseEmptyArray()
    }
  }

  let maxManyUnsafe = arr => arr->Math.maxMany_float

  module Add = {
    let addUnsafe = (a, b) => a +. b

    let add = (a, b) => a->addUnsafe(b)->fromFloat

    let addExn = (a, b) => a->addUnsafe(b)->fromFloatExn

    let addClamped = (a, b) => a->addUnsafe(b)->fromFloatClamped
  }

  include Add

  let subUnsafe = (a, b) => a -. b

  let sub = (a, b) => a->subUnsafe(b)->fromFloat

  let subExn = (a, b) => a->subUnsafe(b)->fromFloatExn

  let subClamped = (a, b) => a->subUnsafe(b)->fromFloatClamped

  let mulUnsafe = (a, b) => a *. b

  let mul = (a, b) => a->mulUnsafe(b)->fromFloat

  let mulExn = (a, b) => a->mulUnsafe(b)->fromFloatExn

  let mulClamped = (a, b) => a->mulUnsafe(b)->fromFloatClamped

  let divUnsafe = (a, b) => a /. b

  let div = (a, b) => b !== zero ? a->divUnsafe(b)->fromFloat : None

  let divExn = (a, b) => b !== zero ? a->divUnsafe(b)->fromFloatExn : raise(Division_by_zero)

  let remUnsafe = (a, b) => a->mod_float(b)

  let rem = (a, b) => b !== zero ? a->remUnsafe(b)->fromFloat : None

  let remExn = (a, b) => b !== zero ? a->remUnsafe(b)->fromFloatExn : raise(Division_by_zero)

  include (
    MakeNumberSum({
      type t = t

      include Add
    }): NumberSum with type t := t
  )

  let negUnsafe = f => -.f

  let neg = f => f->negUnsafe->fromFloat

  let negExn = f => f->negUnsafe->fromFloatExn

  let absUnsafe = f => f->abs_float

  let abs = f => f->absUnsafe->fromFloat

  let absExn = f => f->absUnsafe->fromFloatExn

  let sign = f =>
    switch f->Math.sign_float {
    | -1.0 => Some(Negative)
    | 0.0 => Some(Zero)
    | 1.0 => Some(Positive)
    | _ => None
    }

  let signExn = f =>
    switch f->sign {
    | Some(v) => v
    | None => raiseNaN()
    }

  let signRaw = f => f->Math.sign_float

  let acosUnsafe = f => f->Math.acos

  let acos = f => f->acosUnsafe->fromFloat

  let acosExn = f => f->acosUnsafe->fromFloatExn

  let acoshUnsafe = f => f->Math.acosh

  let acosh = f => f->acoshUnsafe->fromFloat

  let acoshExn = f => f->acoshUnsafe->fromFloatExn

  let asinUnsafe = f => f->Math.asin

  let asin = f => f->asinUnsafe->fromFloat

  let asinExn = f => f->asinUnsafe->fromFloatExn

  let asinh = f => f->Math.asinh

  let atan = f => f->Math.atan

  let atan2 = (~y, ~x) => PervasivesU.atan2(y, x)

  let atanhUnsafe = f => f->Math.atanh

  let atanh = f => f->atanhUnsafe->fromFloat

  let atanhExn = f => f->atanhUnsafe->fromFloatExn

  let cbrt = f => f->Math.cbrt

  let ceil = f => f->Math.ceil_float

  let int32minMinus1 = Int.min->Int.toFloat -. 1.0

  let ceilIntUnsafe = f => f->ceil->toIntUnsafe

  let ceilInt = f => f > int32minMinus1 && f <= Int.max->Int.toFloat ? Some(f->ceilIntUnsafe) : None

  let ceilIntExn = f =>
    switch f->ceilInt {
    | Some(v) => v
    | None => raiseOverflow(f->ceil, module(Int32Range))
    }

  let ceilIntClamped = f =>
    if f < Int.min->Int.toFloat {
      Int.min
    } else if f > Int.max->Int.toFloat {
      Int.max
    } else {
      f->ceilIntUnsafe
    }

  let cosUnsafe = f => f->Math.cos

  let cos = f => f->cosUnsafe->fromFloat

  let cosExn = f => f->cosUnsafe->fromFloatExn

  let cosh = f => f->Math.cosh

  let exp = f => f->Math.exp

  let expm1 = f => f->Math.expm1

  let floor = f => f->Math.floor_float

  let int32maxPlus1 = Int.max->Int.toFloat +. 1.0

  let floorIntUnsafe = f => f->floor->toIntUnsafe

  let floorInt = f =>
    f >= Int.min->Int.toFloat && f < int32maxPlus1 ? Some(f->floorIntUnsafe) : None

  let floorIntExn = f =>
    switch f->floorInt {
    | Some(v) => v
    | None => raiseOverflow(f->floor, module(Int32Range))
    }

  let floorIntClamped = f =>
    if f < Int.min->Int.toFloat {
      Int.min
    } else if f > Int.max->Int.toFloat {
      Int.max
    } else {
      f->floorIntUnsafe
    }

  let fround = f => f->Math.fround

  let hypot = (a, b) => Math.hypot(a, b)

  let hypotMany = arr => arr->Math.hypotMany

  let logUnsafe = f => f->Math.log

  let log = f => f->logUnsafe->fromFloat

  let logExn = f => f->logUnsafe->fromFloatExn

  let log10Unsafe = f => f->Math.log10

  let log10 = f => f->log10Unsafe->fromFloat

  let log10Exn = f => f->log10Unsafe->fromFloatExn

  let log1pUnsafe = f => f->Math.log1p

  let log1p = f => f->log1pUnsafe->fromFloat

  let log1pExn = f => f->log1pUnsafe->fromFloatExn

  let log2Unsafe = f => f->Math.log2

  let log2 = f => f->log2Unsafe->fromFloat

  let log2Exn = f => f->log2Unsafe->fromFloatExn

  let powUnsafe = (~base, ~exp) => Math.pow_float(~base, ~exp)

  let pow = (~base, ~exp) => powUnsafe(~base, ~exp)->fromFloat

  let powExn = (~base, ~exp) => powUnsafe(~base, ~exp)->fromFloatExn

  let random = () => Math.random()

  let randomRange = (min, max) => Math.random() *. (max -. min) +. min

  let round = f => f->Math.round

  let int32minMinusHalf1 = Int.min->Int.toFloat -. 0.5

  let int32maxPlusHalf1 = Int.max->Int.toFloat +. 0.5

  let roundIntUnsafe = f => f->Math.round->toIntUnsafe

  let roundInt = f =>
    f >= int32minMinusHalf1 && f < int32maxPlusHalf1 ? Some(f->roundIntUnsafe) : None

  let roundIntExn = f =>
    switch f->roundInt {
    | Some(v) => v
    | None => raiseOverflow(f->round, module(Int32Range))
    }

  let roundIntClamped = f =>
    if f < Int.min->Int.toFloat {
      Int.min
    } else if f > Int.max->Int.toFloat {
      Int.max
    } else {
      f->roundIntUnsafe
    }

  let sinUnsafe = f => f->Math.sin

  let sin = f => f->sinUnsafe->fromFloat

  let sinExn = f => f->sinUnsafe->fromFloatExn

  let sinh = f => f->Math.sinh

  let sqrtUnsafe = f => f->Math.sqrt

  let sqrt = f => f->sqrtUnsafe->fromFloat

  let sqrtExn = f => f->sqrtUnsafe->fromFloatExn

  let tanUnsafe = f => f->Math.tan

  let tan = f => f->tanUnsafe->fromFloat

  let tanExn = f => f->tanUnsafe->fromFloatExn

  let tanh = f => f->Math.tanh

  let trunc = f => f->Math.trunc

  let truncIntUnsafe = f => f->trunc->toIntUnsafe

  let truncInt = f => f > int32minMinus1 && f < int32maxPlus1 ? Some(f->truncIntUnsafe) : None

  let truncIntExn = f =>
    switch f->truncInt {
    | Some(v) => v
    | None => raiseOverflow(f->trunc, module(Int32Range))
    }

  let truncIntClamped = f =>
    if f < Int.min->Int.toFloat {
      Int.min
    } else if f > Int.max->Int.toFloat {
      Int.max
    } else {
      f->truncIntUnsafe
    }

  let isFinite = isFinite

  let isInteger = isInteger

  let isNaN = isNaN

  let isSafeInteger = isSafeInteger
}
