open Js

open ResNumber__Bitwise
open ResNumber__Conversion
open ResNumber__Math
open ResNumber__Number
open ResNumber__Operation
open ResNumber__Others
open ResNumber__Utils

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

let toInt32 = i => i->lor(0)

@warning("-27")
let toUint32 = (i: int): float => %raw(`i >>> 0`)

module MakeIntegerConversion = (IntRange: NumberRange with type t = float): (
  NumberConversion with type t = float
) => {
  type t = IntRange.t

  let minValue = IntRange.minValue

  let maxValue = IntRange.maxValue

  let toString = i => i->Float.toString

  if minValue > maxValue {
    invalid_arg(`minValue ${minValue->toString} is greater than maxValue ${maxValue->toString}`)
  }

  if minValue < minSafeInteger || maxValue > maxSafeInteger {
    invalid_arg(
      `the range of integer overflows, minValue: ${minValue->toString}, maxValue: ${maxValue->toString}, max range: ${minSafeInteger->toString} - ${maxSafeInteger->toString}`,
    )
  }

  let fromInt = i =>
    i->Int.toFloat >= minValue && i->Int.toFloat <= maxValue ? Some(i->Int.toFloat) : None

  let fromIntExn = i =>
    i->Int.toFloat >= minValue && i->Int.toFloat <= maxValue
      ? i->Int.toFloat
      : raiseOverflow(i, module(IntRange))

  let fromIntClamped = i =>
    if i->Int.toFloat < minValue {
      minValue
    } else if i->Int.toFloat > maxValue {
      maxValue
    } else {
      i->Int.toFloat
    }

  let fromIntUnsafe = i => i->Int.toFloat

  let toInt = i => i->inInt32Range ? Some(i->floatToIntUnsafe) : None

  let toIntExn = i => i->inInt32Range ? i->floatToIntUnsafe : raiseOverflow(i, module(Int32Range))

  let toIntClamped = i =>
    if i < Int.min->Int.toFloat {
      Int.min
    } else if i > Int.max->Int.toFloat {
      Int.max
    } else {
      i->floatToIntUnsafe
    }

  let toIntUnsafe = i => i->floatToIntUnsafe->toInt32

  let fromFloat = f => f->isInteger && f >= minValue && f <= maxValue ? Some(f) : None

  let fromFloatExn = f =>
    if !(f->isInteger) {
      invalid_arg(`float number ${f->toString} is not a integer`)
    } else if f < minValue || f > maxValue {
      raiseOverflow(f, module(IntRange))
    } else {
      f
    }

  let fromFloatUnsafe = f => f->Math.trunc

  let fromFloatClamped = f =>
    if f < minValue {
      minValue
    } else if f > maxValue {
      maxValue
    } else {
      f->fromFloatUnsafe
    }

  let toFloat = f => f

  let fromString = s =>
    switch s->stringToFloat {
    | Some(f) => f->fromFloat
    | None => None
    }

  let fromStringExn = s =>
    switch s->stringToFloat {
    | Some(f) => f->fromFloatExn
    | None => invalid_arg(`the string is not a integer: ${s}`)
    }
}

module MakeNumberIncDec = (
  M: {
    type t

    let one: t

    include NumberAddition with type t := t

    include NumberSubtraction with type t := t
  },
): (NumberIncDec with type t = M.t) => {
  type t = M.t

  let inc = n => n->M.add(M.one)

  let incExn = n => n->M.addExn(M.one)

  let incUnsafe = n => n->M.addUnsafe(M.one)

  let dec = n => n->M.sub(M.one)

  let decExn = n => n->M.subExn(M.one)

  let decUnsafe = n => n->M.subUnsafe(M.one)
}

module MakeInteger = (IntRange: NumberRange with type t = float): (Integer with type t = float) => {
  type t = IntRange.t

  include (MakeIntegerConversion(IntRange): NumberConversion with type t := t)

  let zero = 0.0

  let one = 1.0

  let minValue = IntRange.minValue

  let maxValue = IntRange.maxValue

  if minValue > zero || maxValue < one {
    invalid_arg(
      `${zero->toString} and ${one->toString} must be between minValue ${minValue->toString} and maxValue ${maxValue->toString}`,
    )
  }

  let compareExn = (a: t, b: t) =>
    if a < b {
      Less
    } else if a > b {
      Greater
    } else {
      Equal
    }

  let compare = (a, b) => Some(compareExn(a, b))

  let eq = (a: t, b: t) => a === b

  let ne = (a: t, b: t) => !eq(a, b)

  let lt = (a: t, b: t) => a < b

  let le = (a: t, b: t) => a <= b

  let gt = (a: t, b: t) => a > b

  let ge = (a: t, b: t) => a >= b

  let min = (a: t, b: t) => PervasivesU.min(a, b)

  let minMany = arr => {
    let len = arr->Array2.length

    len > 0 ? Some(len === 1 ? arr->Array2.unsafe_get(0) : arr->Math.minMany_float) : None
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

    len > 0 ? Some(len === 1 ? arr->Array2.unsafe_get(0) : arr->Math.maxMany_float) : None
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

  let fromFloatUncheckInteger = f => f >= minValue && f <= maxValue ? Some(f) : None

  let fromFloatUncheckIntegerExn = f =>
    f >= minValue && f <= maxValue ? f : raiseOverflow(f, module(IntRange))

  let fromFloatUncheckIntegerClamped = f =>
    if f < minValue {
      minValue
    } else if f > maxValue {
      maxValue
    } else {
      f
    }

  module Add = {
    let addUnsafe = (a, b) => a +. b

    let add = (a, b) => addUnsafe(a, b)->fromFloatUncheckInteger

    let addExn = (a, b) => addUnsafe(a, b)->fromFloatUncheckIntegerExn

    let addClamped = (a, b) => addUnsafe(a, b)->fromFloatUncheckIntegerClamped
  }

  include Add

  module Sub = {
    let subUnsafe = (a, b) => a -. b

    let sub = (a, b) => subUnsafe(a, b)->fromFloatUncheckInteger

    let subExn = (a, b) => subUnsafe(a, b)->fromFloatUncheckIntegerExn

    let subClamped = (a, b) => subUnsafe(a, b)->fromFloatUncheckIntegerClamped
  }

  include Sub

  let mulUnsafe = (a, b) => a *. b

  let mul = (a, b) => mulUnsafe(a, b)->fromFloatUncheckInteger

  let mulExn = (a, b) => mulUnsafe(a, b)->fromFloatUncheckIntegerExn

  let mulClamped = (a, b) => mulUnsafe(a, b)->fromFloatUncheckIntegerClamped

  let divUnsafe = (a, b) => (a /. b)->fromFloatUnsafe

  let div = (a, b) => b !== zero ? Some(divUnsafe(a, b)) : None

  let divExn = (a, b) => b !== zero ? divUnsafe(a, b) : raise(Division_by_zero)

  let remUnsafe = (a, b) => mod_float(a, b)

  let rem = (a, b) => b !== zero ? Some(remUnsafe(a, b)) : None

  let remExn = (a, b) => b !== zero ? remUnsafe(a, b) : raise(Division_by_zero)

  include (
    MakeNumberSum({
      type t = t

      include Add
    }): NumberSum with type t := t
  )

  include (
    MakeNumberIncDec({
      type t = t

      let one = one

      include Add

      include Sub
    }): NumberIncDec with type t := t
  )
}

module MakeFixedBitsIntegerConversion = (IntRange: NumberRange with type t = int): (
  NumberConversion with type t = int
) => {
  type t = IntRange.t

  let minValue = IntRange.minValue

  let maxValue = IntRange.maxValue

  let toString = i => i->Int.toString

  if minValue > maxValue {
    invalid_arg(`minValue ${minValue->toString} is greater than maxValue ${maxValue->toString}`)
  }

  if minValue < Int.min || maxValue > Int.max {
    invalid_arg(
      `the range of integer overflows, minValue: ${minValue->toString}, maxValue: ${maxValue->toString}, max range: ${Int.min->toString} - ${Int.max->toString}`,
    )
  }

  let isInt32 = minValue === Int.min && maxValue === Int.max

  let fromInt = !isInt32 ? i => i >= minValue && i <= maxValue ? Some(i) : None : i => Some(i)

  let fromIntExn = !isInt32
    ? i => i >= minValue && i <= maxValue ? i : raiseOverflow(i, module(IntRange))
    : i => i

  let fromIntClamped = !isInt32
    ? i =>
        if i < minValue {
          minValue
        } else if i > maxValue {
          maxValue
        } else {
          i
        }
    : i => i

  let fromIntUnsafe = i => i

  let toInt = (i: t) => Some(i)

  let toIntExn = i => i

  let toIntClamped = i => i

  let toIntUnsafe = i => i

  let fromFloat = f =>
    f->isInteger && f >= minValue->Int.toFloat && f <= maxValue->Int.toFloat
      ? Some(f->floatToIntUnsafe)
      : None

  let fromFloatExn = f =>
    if !(f->isInteger) {
      invalid_arg(`float number ${f->Float.toString} is not a integer`)
    } else if f < minValue->Int.toFloat || f > maxValue->Int.toFloat {
      raiseOverflow(f, module(IntRange))
    } else {
      f->floatToIntUnsafe
    }

  let fromFloatUnsafe = f => f->floatToIntUnsafe->toInt32

  let fromFloatClamped = f =>
    if f < minValue->Int.toFloat {
      minValue
    } else if f > maxValue->Int.toFloat {
      maxValue
    } else {
      f->fromFloatUnsafe
    }

  let toFloat = i => i->Int.toFloat

  let fromString = s =>
    switch s->stringToInt {
    | Some(i) => i->fromInt
    | None => None
    }

  let fromStringExn = s =>
    switch s->stringToInt {
    | Some(i) => i->fromIntExn
    | None => invalid_arg(`the string is not a integer: ${s}`)
    }
}

module MakeFixedBitsInt = (IntRange: NumberRange with type t = int): (
  Integer with type t = int
) => {
  type t = IntRange.t

  include (MakeFixedBitsIntegerConversion(IntRange): NumberConversion with type t := t)

  let zero = 0

  let one = 1

  let minValue = IntRange.minValue

  let maxValue = IntRange.maxValue

  if minValue > zero || maxValue < one {
    invalid_arg(
      `${zero->toString} and ${one->toString} must be between minValue ${minValue->toString} and maxValue ${maxValue->toString}`,
    )
  }

  let compareExn = (a: t, b: t) =>
    if a < b {
      Less
    } else if a > b {
      Greater
    } else {
      Equal
    }

  let compare = (a, b) => Some(compareExn(a, b))

  let eq = (a: t, b: t) => a === b

  let ne = (a: t, b: t) => !eq(a, b)

  let lt = (a: t, b: t) => a < b

  let le = (a: t, b: t) => a <= b

  let gt = (a: t, b: t) => a > b

  let ge = (a: t, b: t) => a >= b

  let min = (a: t, b: t) => PervasivesU.min(a, b)

  let minMany = arr => {
    let len = arr->Array2.length

    len > 0 ? Some(len === 1 ? arr->Array2.unsafe_get(0) : arr->Math.minMany_int) : None
  }

  let minManyExn = arr => {
    let len = arr->Array2.length

    if len > 0 {
      len === 1 ? arr->Array2.unsafe_get(0) : arr->Math.minMany_int
    } else {
      raiseEmptyArray()
    }
  }

  let minManyUnsafe = arr => arr->Math.minMany_int->toInt32

  let max = (a: t, b: t) => PervasivesU.max(a, b)

  let maxMany = arr => {
    let len = arr->Array2.length

    len > 0 ? Some(len === 1 ? arr->Array2.unsafe_get(0) : arr->Math.maxMany_int) : None
  }

  let maxManyExn = arr => {
    let len = arr->Array2.length

    if len > 0 {
      len === 1 ? arr->Array2.unsafe_get(0) : arr->Math.maxMany_int
    } else {
      raiseEmptyArray()
    }
  }

  let maxManyUnsafe = arr => arr->Math.maxMany_int->toInt32

  let fromFloatUncheckInteger = f =>
    f >= minValue->Int.toFloat && f <= maxValue->Int.toFloat ? Some(f->floatToIntUnsafe) : None

  let fromFloatUncheckIntegerExn = f =>
    f >= minValue->Int.toFloat && f <= maxValue->Int.toFloat
      ? f->floatToIntUnsafe
      : raiseOverflow(f, module(IntRange))

  let fromFloatUncheckIntegerClamped = f =>
    if f < minValue->Int.toFloat {
      minValue
    } else if f > maxValue->Int.toFloat {
      maxValue
    } else {
      f->floatToIntUnsafe
    }

  let isAddMaybeOverflow =
    minValue->Int.toFloat +. minValue->Int.toFloat < Int.min->Int.toFloat ||
      maxValue->Int.toFloat +. maxValue->Int.toFloat > Int.max->Int.toFloat

  module Add = {
    let addUnsafe = (a, b) => a + b

    let add = isAddMaybeOverflow
      ? (a, b) => (a->Int.toFloat +. b->Int.toFloat)->fromFloatUncheckInteger
      : (a, b) => addUnsafe(a, b)->fromInt

    let addExn = isAddMaybeOverflow
      ? (a, b) => (a->Int.toFloat +. b->Int.toFloat)->fromFloatUncheckIntegerExn
      : (a, b) => addUnsafe(a, b)->fromIntExn

    let addClamped = isAddMaybeOverflow
      ? (a, b) => (a->Int.toFloat +. b->Int.toFloat)->fromFloatUncheckIntegerClamped
      : (a, b) => addUnsafe(a, b)->fromIntClamped
  }

  include Add

  let isSubMaybeOverflow =
    minValue->Int.toFloat -. maxValue->Int.toFloat < Int.min->Int.toFloat ||
      maxValue->Int.toFloat -. minValue->Int.toFloat > Int.max->Int.toFloat

  module Sub = {
    let subUnsafe = (a, b) => a - b

    let sub = isSubMaybeOverflow
      ? (a, b) => (a->Int.toFloat -. b->Int.toFloat)->fromFloatUncheckInteger
      : (a, b) => subUnsafe(a, b)->fromInt

    let subExn = isSubMaybeOverflow
      ? (a, b) => (a->Int.toFloat -. b->Int.toFloat)->fromFloatUncheckIntegerExn
      : (a, b) => subUnsafe(a, b)->fromIntExn

    let subClamped = isSubMaybeOverflow
      ? (a, b) => (a->Int.toFloat -. b->Int.toFloat)->fromFloatUncheckIntegerClamped
      : (a, b) => subUnsafe(a, b)->fromIntClamped
  }

  include Sub

  let isMulMaybeOverflow =
    minValue->Int.toFloat *. maxValue->Int.toFloat < Int.min->Int.toFloat ||
    minValue->Int.toFloat *. minValue->Int.toFloat > Int.max->Int.toFloat ||
    maxValue->Int.toFloat *. maxValue->Int.toFloat > Int.max->Int.toFloat

  let mulUnsafe = (a, b) => a * b

  let mul = isMulMaybeOverflow
    ? (a, b) => (a->Int.toFloat *. b->Int.toFloat)->fromFloatUncheckInteger
    : (a, b) => mulUnsafe(a, b)->fromInt

  let mulExn = isMulMaybeOverflow
    ? (a, b) => (a->Int.toFloat *. b->Int.toFloat)->fromFloatUncheckIntegerExn
    : (a, b) => mulUnsafe(a, b)->fromIntExn

  let mulClamped = isMulMaybeOverflow
    ? (a, b) => (a->Int.toFloat *. b->Int.toFloat)->fromFloatUncheckIntegerClamped
    : (a, b) => mulUnsafe(a, b)->fromIntClamped

  let divUnsafe = (a, b) => (a->Int.toFloat /. b->Int.toFloat)->fromFloatUnsafe

  let div = (a, b) => b !== zero ? Some(divUnsafe(a, b)) : None

  let divExn = (a, b) => a / b

  let remUnsafe = (a, b) => mod_float(a->Int.toFloat, b->Int.toFloat)->fromFloatUnsafe

  let rem = (a, b) => b !== zero ? Some(remUnsafe(a, b)) : None

  let remExn = (a, b) => mod(a, b)->toInt32

  include (
    MakeNumberSum({
      type t = t

      include Add
    }): NumberSum with type t := t
  )

  include (
    MakeNumberIncDec({
      type t = t

      let one = one

      include Add

      include Sub
    }): NumberIncDec with type t := t
  )
}

module MakeIntegerMath = (
  M: {
    type t

    include NumberFromFloat with type t := t

    include NumberToFloat with type t := t

    let clz: t => int

    let imul: (t, t) => t
  },
): (IntegerMath with type t = M.t) => {
  type t = M.t

  let clz = M.clz

  let imul = M.imul

  let pow_ = (~base, ~exp) => Math.pow_float(~base=base->M.toFloat, ~exp=exp->M.toFloat)

  let pow = (~base, ~exp) => pow_(~base, ~exp)->M.fromFloat

  let powExn = (~base, ~exp) => pow_(~base, ~exp)->M.fromFloatExn

  let powUnsafe = (~base, ~exp) => pow_(~base, ~exp)->M.fromFloatUnsafe

  let random = (min, max) => {
    let min_ = min->M.toFloat
    let max_ = max->M.toFloat

    (Math.random() *. (max_ -. min_) +. min_)->Math.floor_float->M.fromFloatClamped
  }
}

module MakeFixedBitsInteger = (
  M: {
    type t = int

    let isSigned: bool

    include NumberBits

    include NumberRange with type t := t
  },
): (FixedBitsInteger with type t = int) => {
  type t = M.t

  module IntModule = MakeFixedBitsInt(M)

  include (IntModule: Integer with type t := t)

  let bits = M.bits

  if bits <= 0 || bits > 32 {
    invalid_arg(`bits must not be less than 1 or greater than 32: ${bits->toString}`)
  }

  let isSigned = M.isSigned

  let isUnsigned = !isSigned

  if isSigned {
    if minValue >= 0 {
      invalid_arg(`minValue for signed integer must be less than 0: ${minValue->toString}`)
    }

    let n = minValue->Int.toFloat->Math.abs_float

    if n->Math.log2 !== (bits - 1)->Int.toFloat {
      invalid_arg(
        `invalid minValue ${minValue->toString} for ${bits->toString} bits signed integer`,
      )
    }
    if n -. 1.0 !== maxValue->Int.toFloat {
      invalid_arg(
        `invalid maxValue ${maxValue->toString} for ${bits->toString} bits signed integer`,
      )
    }
  }

  if isUnsigned {
    if minValue !== 0 {
      invalid_arg(
        `invalid minValue ${minValue->Int.toString} for ${bits->Int.toString} bits unsigned integer`,
      )
    }
    if 2.0 ** bits->Int.toFloat -. 1.0 !== maxValue->Int.toFloat {
      invalid_arg(
        `invalid maxValue ${maxValue->Int.toString} for ${bits->Int.toString} bits unsigned integer`,
      )
    }
  }

  let is32Bits = bits === 32

  let mask = -1->lsr(32 - bits)

  let maskNot = mask->lnot

  let highestBit = 1->lsl(bits - 1)

  module P = PervasivesU

  let lnot = isUnsigned ? i => i->P.lnot->P.land(mask) : P.lnot

  let land = P.land

  let lor = P.lor

  let lxor = P.lxor

  let modBits = n =>
    n < 0 || n >= bits ? n->toUint32->P.mod_float(bits->Int.toFloat)->floatToIntUnsafe : n

  let lsl = !is32Bits
    ? isSigned
        ? (i, n) => {
            let m = n->modBits

            if m !== 0 {
              let v = i->P.lsl(m)

              v->P.land(highestBit) === 0 ? v->P.land(mask) : v->P.lor(maskNot)
            } else {
              i
            }
          }
        : (i, n) => {
            let m = n->modBits

            m !== 0 ? i->P.lsl(m)->P.land(mask) : i
          }
    : P.lsl

  let lsr = !is32Bits
    ? isSigned
        ? (i, n) => {
            let m = n->modBits

            m !== 0 ? i->P.land(mask)->P.lsr(m) : i
          }
        : (i, n) => i->P.lsr(n->modBits)
    : P.lsr

  let asr = !is32Bits ? (i, n) => i->P.asr(n->modBits) : P.asr

  let rsl = (i, n) => {
    let m = n->modBits

    m !== 0 ? i->lsl(m)->lor(i->lsr(bits - m)) : i
  }

  let rsr = (i, n) => {
    let m = n->modBits

    m !== 0 ? i->lsr(m)->lor(i->lsl(bits - m)) : i
  }

  include (
    MakeIntegerMath({
      type t = M.t

      include (IntModule: Integer with type t := t)

      let clz = !is32Bits
        ? i => {
            let c = i->Math.clz32

            c > 0 ? c - (32 - bits) : c
          }
        : i => i->Math.clz32

      let imul = !is32Bits
        ? isSigned
            ? (a, b) => {
                let i = Math.imul(a, b)

                i->P.land(highestBit) === 0 ? i->P.land(mask) : i->P.lor(maskNot)
              }
            : (a, b) => Math.imul(a, b)->P.land(mask)
        : (a, b) => Math.imul(a, b)
    }): IntegerMath with type t := t
  )
}

module MakeSignedInteger = (
  M: {
    type t = int

    include NumberBits

    include NumberRange with type t := t
  },
): (SignedInteger with type t = int) => {
  type t = M.t

  include (
    MakeFixedBitsInteger({
      include M

      let isSigned = true
    }): FixedBitsInteger with type t := t
  )

  let neg = i => i !== minValue ? Some(-i) : None

  let negExn = i =>
    switch i->neg {
    | Some(v) => v
    | None => raiseOverflow(-.(i->Int.toFloat), module(M))
    }

  let negUnsafe = i => -i

  let abs = i => i !== minValue ? Some(i->Math.abs_int) : None

  let absExn = i =>
    switch i->abs {
    | Some(v) => v
    | None => raiseOverflow(i->Int.toFloat->Math.abs_float, module(M))
    }

  let absUnsafe = i => i->PervasivesU.abs

  let signExn = i =>
    switch i->Math.sign_int {
    | -1 => Negative
    | 1 => Positive
    | _ => Zero
    }

  let sign = i => Some(i->signExn)

  let signRaw = i => i->Int.toFloat->Math.sign_float
}

module MakeUnsignedInteger = (
  M: {
    type t = int

    include NumberBits

    include NumberRange with type t := t
  },
): (UnsignedInteger with type t = int) => {
  type t = M.t

  include (
    MakeFixedBitsInteger({
      include M

      let isSigned = false
    }): FixedBitsInteger with type t := t
  )
}

module Int8 = MakeSignedInteger({
  type t = int

  let bits = 8

  let minValue = -128

  let maxValue = 127
})

module Uint8 = MakeUnsignedInteger({
  type t = int

  let bits = 8

  let minValue = 0

  let maxValue = 255
})

module Int16 = MakeSignedInteger({
  type t = int

  let bits = 16

  let minValue = -32768

  let maxValue = 32767
})

module Uint16 = MakeUnsignedInteger({
  type t = int

  let bits = 16

  let minValue = 0

  let maxValue = 65535
})

module Int32 = MakeSignedInteger({
  type t = int

  let bits = 32

  let minValue = Js.Int.min

  let maxValue = Js.Int.max
})

module Uint32: UnsignedInteger with type t = float = {
  type t = float

  module IntModule = {
    include MakeInteger({
      type t = t

      let minValue = 0.0

      let maxValue = 4294967295.0
    })

    let fromIntUnsafe = i => i->toUint32

    let fromFloatUnsafe = f => f->floatToIntUnsafe->toUint32

    let minManyUnsafe = arr => arr->Math.minMany_float->fromFloatUnsafe

    let maxManyUnsafe = arr => arr->Math.maxMany_float->fromFloatUnsafe

    let addUnsafe = (a, b) => (a +. b)->fromFloatUnsafe

    let subUnsafe = (a, b) => (a -. b)->fromFloatUnsafe

    let mulUnsafe = (a, b) => (a *. b)->fromFloatUnsafe

    let divUnsafe = (a, b) => (a /. b)->fromFloatUnsafe

    let sumUnsafe = arr => arr->sumUnsafe->fromFloatUnsafe

    let incUnsafe = n => n->addUnsafe(one)

    let decUnsafe = n => n->subUnsafe(one)
  }

  include (IntModule: Integer with type t := t)

  let isSigned = false

  let isUnsigned = true

  let bits = 32

  module P = PervasivesU

  let lnot = i => i->floatToIntUnsafe->P.lnot->toUint32

  let land = (a, b) => a->floatToIntUnsafe->P.land(b->floatToIntUnsafe)->toUint32

  let lor = (a, b) => a->floatToIntUnsafe->P.lor(b->floatToIntUnsafe)->toUint32

  let lxor = (a, b) => a->floatToIntUnsafe->P.lxor(b->floatToIntUnsafe)->toUint32

  let lsl = (i, n) => i->floatToIntUnsafe->P.lsl(n)->toUint32

  @warning("-27")
  let lsr = (i: t, n: int): t => %raw(`i >>> n`)

  let asr = lsr

  let modBits = n =>
    n < 0 || n >= bits ? n->toUint32->P.mod_float(bits->Int.toFloat)->floatToIntUnsafe : n

  let rsl = (i, n) => {
    let m = n->modBits

    m !== 0
      ? i->floatToIntUnsafe->P.lsl(m)->P.lor(i->floatToIntUnsafe->P.lsr(bits - m))->toUint32
      : i
  }

  let rsr = (i, n) => {
    let m = n->modBits

    m !== 0
      ? i->floatToIntUnsafe->P.lsr(m)->P.lor(i->floatToIntUnsafe->P.lsl(bits - m))->toUint32
      : i
  }

  include (
    MakeIntegerMath({
      type t = t

      include (IntModule: Integer with type t := t)

      let clz = i => i->floatToIntUnsafe->Math.clz32

      let imul = (a, b) => Math.imul(a->floatToIntUnsafe, b->floatToIntUnsafe)->toUint32
    }): IntegerMath with type t := t
  )
}
