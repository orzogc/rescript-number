open Js

open ResNumber__Conversion
open ResNumber__Math
open ResNumber__Number
open ResNumber__Operation
open ResNumber__Others
open ResNumber__Utils

module type Float = {
  type t

  include NumberConversion with type t := t

  include Number with type t := t

  include FloatMath with type t := t
}

let toInt32 = f => f->floatToIntUnsafe->lor(0)

let raiseNaN = () => invalid_arg(`NaN`)

let raiseOperationNaN = (a, b, s) =>
  invalid_arg(`${a->Js.Float.toString} ${s} ${b->Js.Float.toString} = Nan`)

module Float64: Float with type t = float = {
  type t = float

  let zero = 0.0

  let one = 1.0

  let minValue = neg_infinity

  let maxValue = infinity

  let toString = f => f->Js.Float.toString

  let fromIntUnsafe = i => i->Int.toFloat

  let fromInt = i => Some(i->fromIntUnsafe)

  let fromIntExn = fromIntUnsafe

  let fromIntClamped = fromIntUnsafe

  let toInt = f => f->isInteger && f->inInt32Range ? Some(f->floatToIntUnsafe) : None

  let toIntExn = f =>
    if !(f->isInteger) {
      invalid_arg(`float number ${f->toString} is not a integer`)
    } else if !(f->inInt32Range) {
      raiseOverflow(f, module(Int32Range))
    } else {
      f->floatToIntUnsafe
    }

  let toIntClamped = f =>
    if f < Int.min->Int.toFloat {
      Int.min
    } else if f > Int.max->Int.toFloat {
      Int.max
    } else {
      f->truncate
    }

  let toIntUnsafe = f => f->toInt32

  let fromFloat = f => !(f->isNaN) ? Some(f) : None

  let fromFloatExn = f =>
    switch f->fromFloat {
    | Some(v) => v
    | None => raiseNaN()
    }

  let fromFloatClamped = f => !(f->isNaN) ? f : 0.0

  let fromFloatUnsafe = f => f

  let toFloat = f => f

  let fromString = stringToFloat

  let fromStringExn = s =>
    switch s->fromString {
    | Some(f) => f
    | None => invalid_arg(`the string is not a float number: ${s}`)
    }

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
      let f = len === 1 ? arr->Array2.unsafe_get(0) : arr->Math.minMany_float

      !(f->isNaN) ? Some(f) : None
    } else {
      None
    }
  }

  let minManyExn = arr => {
    let len = arr->Array2.length

    if len > 0 {
      let f = len === 1 ? arr->Array2.unsafe_get(0) : arr->Math.minMany_float

      !(f->isNaN) ? f : raiseNaN()
    } else {
      raiseEmptyArray()
    }
  }

  let minManyUnsafe = arr => arr->Math.minMany_float

  let max = (a: t, b: t) => PervasivesU.max(a, b)

  let maxMany = arr => {
    let len = arr->Array2.length

    if len > 0 {
      let f = len === 1 ? arr->Array2.unsafe_get(0) : arr->Math.maxMany_float

      !(f->isNaN) ? Some(f) : None
    } else {
      None
    }
  }

  let maxManyExn = arr => {
    let len = arr->Array2.length

    if len > 0 {
      let f = len === 1 ? arr->Array2.unsafe_get(0) : arr->Math.maxMany_float

      !(f->isNaN) ? f : raiseNaN()
    } else {
      raiseEmptyArray()
    }
  }

  let maxManyUnsafe = arr => arr->Math.maxMany_float

  module Add = {
    let addUnsafe = (a, b) => a +. b

    let add = (a, b) => {
      let f = a->addUnsafe(b)

      !(f->isNaN) ? Some(f) : None
    }

    let addExn = (a, b) =>
      switch a->add(b) {
      | Some(f) => f
      | None => raiseOperationNaN(a, b, "+")
      }

    let addClamped = (a, b) =>
      switch a->add(b) {
      | Some(f) => f
      | None => zero
      }
  }

  include Add

  module Sub = {
    let subUnsafe = (a, b) => a -. b

    let sub = (a, b) => {
      let f = a->subUnsafe(b)

      !(f->isNaN) ? Some(f) : None
    }

    let subExn = (a, b) =>
      switch a->sub(b) {
      | Some(f) => f
      | None => raiseOperationNaN(a, b, "-")
      }

    let subClamped = (a, b) =>
      switch a->sub(b) {
      | Some(f) => f
      | None => zero
      }
  }

  include Sub

  let mulUnsafe = (a, b) => a *. b

  let mul = (a, b) => {
    let f = a->mulUnsafe(b)

    !(f->isNaN) ? Some(f) : None
  }

  let mulExn = (a, b) =>
    switch a->mul(b) {
    | Some(f) => f
    | None => raiseOperationNaN(a, b, "*")
    }

  let mulClamped = (a, b) =>
    switch a->mul(b) {
    | Some(f) => f
    | None => zero
    }

  let divUnsafe = (a, b) => a /. b

  let div = (a, b) =>
    if b !== 0.0 {
      let f = a->divUnsafe(b)

      !(f->isNaN) ? Some(f) : None
    } else {
      None
    }

  let divExn = (a, b) =>
    if b !== 0.0 {
      let f = a->divUnsafe(b)

      if f->isNaN {
        raiseOperationNaN(a, b, "/")
      } else {
        f
      }
    } else {
      raise(Division_by_zero)
    }

  let remUnsafe = (a, b) => a->mod_float(b)

  let rem = (a, b) =>
    if b !== 0.0 {
      let f = a->remUnsafe(b)

      !(f->isNaN) ? Some(f) : None
    } else {
      None
    }

  let remExn = (a, b) =>
    if b !== 0.0 {
      let f = a->remUnsafe(b)

      if f->isNaN {
        raiseOperationNaN(a, b, "%")
      } else {
        f
      }
    } else {
      raise(Division_by_zero)
    }

  include (
    MakeNumberIncDec({
      type t = t

      let one = one

      include Add

      include Sub
    }): NumberIncDec with type t := t
  )

  include (
    MakeNumberSum({
      type t = t

      include Add
    }): NumberSum with type t := t
  )

  let negUnsafe = f => -.f

  let neg = f => !(f->isNaN) ? Some(f->negUnsafe) : None

  let negExn = f =>
    switch f->neg {
    | Some(v) => v
    | None => raiseNaN()
    }

  let absUnsafe = f => f->abs_float

  let abs = f => !(f->isNaN) ? Some(f->absUnsafe) : None

  let absExn = f =>
    switch f->abs {
    | Some(v) => v
    | None => raiseNaN()
    }

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

  let acos = f => f->Math.acos

  let acosh = f => f->Math.acosh

  let asin = f => f->Math.asin

  let asinh = f => f->Math.asinh

  let atan = f => f->Math.atan

  let atan2 = (~y, ~x) => PervasivesU.atan2(y, x)

  let atanh = f => f->Math.atanh

  let cbrt = f => f->Math.cbrt

  let ceil = f => f->Math.ceil_float

  let int32minMinus1 = Int.min->Int.toFloat -. 1.0

  let ceilInt = f =>
    f > int32minMinus1 && f <= Int.max->Int.toFloat ? Some(f->ceil->floatToIntUnsafe) : None

  let ceilIntExn = f =>
    switch f->ceilInt {
    | Some(v) => v
    | None => raiseOverflow(f->ceil, module(Int32Range))
    }

  let ceilIntClamped = f => f->Math.ceil_int

  let ceilIntUnsafe = f => f->ceil->toInt32

  let cos = f => f->Math.cos

  let cosh = f => f->Math.cosh

  let exp = f => f->Math.exp

  let expm1 = f => f->Math.expm1

  let floor = f => f->Math.floor_float

  let int32maxPlus1 = Int.max->Int.toFloat +. 1.0

  let floorInt = f =>
    f >= Int.min->Int.toFloat && f < int32maxPlus1 ? Some(f->floor->floatToIntUnsafe) : None

  let floorIntExn = f =>
    switch f->floorInt {
    | Some(v) => v
    | None => raiseOverflow(f->floor, module(Int32Range))
    }

  let floorIntClamped = f => f->Math.floor_int

  let floorIntUnsafe = f => f->floor->toInt32

  let fround = f => f->Math.fround

  let hypot = (a, b) => Math.hypot(a, b)

  let hypotMany = arr => arr->Math.hypotMany

  let log = f => f->Math.log

  let log10 = f => f->Math.log10

  let log1p = f => f->Math.log1p

  let log2 = f => f->Math.log2

  let pow = (~base, ~exp) => Math.pow_float(~base, ~exp)

  let random = () => Math.random()

  let randomRange = (min, max) => Math.random() *. (max -. min) +. min

  let round = f => f->Math.round

  let int32minMinusHalf1 = Int.min->Int.toFloat -. 0.5

  let int32maxPlusHalf1 = Int.max->Int.toFloat +. 0.5

  let roundInt = f =>
    f >= int32minMinusHalf1 && f < int32maxPlusHalf1 ? Some(f->round->floatToIntUnsafe) : None

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
      f->round->floatToIntUnsafe
    }

  let roundIntUnsafe = f => f->Math.round->toInt32

  let sin = f => f->Math.sin

  let sinh = f => f->Math.sinh

  let sqrt = f => f->Math.sqrt

  let tan = f => f->Math.tan

  let tanh = f => f->Math.tanh

  let trunc = f => f->Math.trunc

  let truncInt = f =>
    f > int32minMinus1 && f < int32maxPlus1 ? Some(f->trunc->floatToIntUnsafe) : None

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
      f->trunc->floatToIntUnsafe
    }

  let truncIntUnsafe = f => f->trunc->toInt32
}
