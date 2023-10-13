open Ava
open Belt

open TestUtils

module IntModule = Number.Uint8

let name = "Uint8"

let isSigned = false

let is32Bits = false

let bits = 8

let minValue = 0

let maxValue = 255

let value = 150

let valueNot = 105

let valueShiftLeft = [value, 44, 88, 176, 96, 192, 128, 0]

let valueRotateShiftLeft = [value, 45, 90, 180, 105, 210, 165, 75]

let less = 7

let greater = 27

let lessGreaterAnd = 3

let lessGreaterOr = 31

let lessGreaterXor = 28

let negZero: int = %raw(`-0`)

let zero = IntModule.zero

let one = IntModule.one

let fromIntExn = IntModule.fromIntExn

let fromFloatExn = IntModule.fromFloatExn

let randomValue = () => randomInt(minValue->Int.toFloat, maxValue->Int.toFloat)

let randomLargerInt = () => randomInt(maxValue->Int.toFloat +. 1.0, Js.Int.max->Int.toFloat)

let randomSmallerInt = () => randomInt(Js.Int.min->Int.toFloat, minValue->Int.toFloat)

let testFn = fn => {
  fn(0)
  fn(negZero)
  fn(1)
  fn(value)
  fn(less)
  fn(greater)
  fn(minValue)
  fn(minValue + 1)
  fn(maxValue)
  fn(maxValue - 1)
  loop100Fn(() => fn(randomValue()))
}

test(`test ${name} sign`, t => {
  t->Assert.deepEqual(IntModule.isSigned, isSigned, ())
  t->Assert.deepEqual(IntModule.isUnsigned, !isSigned, ())
})

test(`test ${name} bits`, t => t->Assert.deepEqual(IntModule.bits, bits, ()))

test(`test ${name} from int`, t => {
  let testInRange = i => {
    let n = fromIntExn(i)
    t->Assert.deepEqual(IntModule.fromInt(i), Some(n), ())
    t->Assert.deepEqual(fromIntExn(i), n, ())
    t->Assert.deepEqual(IntModule.fromIntClamped(i), n, ())
    t->Assert.deepEqual(IntModule.fromIntUnsafe(i), n, ())
  }

  testFn(testInRange)

  if isSigned {
    testInRange(-1)
    testInRange(-value)
    testInRange(-maxValue)

    if !is32Bits {
      testInRange(-minValue - 1)
    }
  }

  if !is32Bits {
    let testOutOfRange = (i, value) => {
      t->Assert.deepEqual(IntModule.fromInt(i), None, ())
      t->assertOverflow(() => fromIntExn(i))
      t->Assert.deepEqual(IntModule.fromIntClamped(i), fromIntExn(value), ())
    }

    testOutOfRange(minValue - 1, minValue)
    testOutOfRange(maxValue + 1, maxValue)
    testOutOfRange(minValue - value, minValue)
    testOutOfRange(maxValue + value, maxValue)
    loop100Fn(() => testOutOfRange(randomLargerInt(), maxValue))
    loop100Fn(() => testOutOfRange(randomSmallerInt(), minValue))
  }
})

test(`test ${name} to int`, t => {
  let testToInt = i => {
    let n = fromIntExn(i)
    let i = i->lor(0)
    t->Assert.deepEqual(n->IntModule.toInt, Some(i), ())
    t->Assert.deepEqual(n->IntModule.toIntExn, i, ())
    t->Assert.deepEqual(n->IntModule.toIntClamped, i, ())
    t->Assert.deepEqual(n->IntModule.toIntUnsafe, i, ())
  }

  testFn(testToInt)

  if isSigned {
    testToInt(-1)
    testToInt(-value)
    testToInt(-maxValue)

    if !is32Bits {
      testToInt(-minValue - 1)
    }
  }
})

test(`test ${name} from float`, t => {
  let testIsIntegerAndInRange = i => {
    let f = i->Int.toFloat
    let n = fromIntExn(i)
    t->Assert.deepEqual(IntModule.fromFloat(f), Some(n), ())
    t->Assert.deepEqual(fromFloatExn(f), n, ())
    t->Assert.deepEqual(IntModule.fromFloatClamped(f), n, ())
    t->Assert.deepEqual(IntModule.fromFloatUnsafe(f), n, ())
  }

  testFn(testIsIntegerAndInRange)

  if isSigned {
    testIsIntegerAndInRange(-1)
    testIsIntegerAndInRange(-value)
    testIsIntegerAndInRange(-maxValue)

    if !is32Bits {
      testIsIntegerAndInRange(-minValue - 1)
    }
  }

  let testOutOfRange = (f, value) => {
    t->Assert.deepEqual(IntModule.fromFloat(f), None, ())
    t->assertOverflow(() => fromFloatExn(f))
    t->Assert.deepEqual(IntModule.fromFloatClamped(f), fromIntExn(value), ())
  }

  testOutOfRange(minValue->Int.toFloat -. 1.0, minValue)
  testOutOfRange(maxValue->Int.toFloat +. 1.0, maxValue)
  testOutOfRange(minValue->Int.toFloat -. value->Int.toFloat, minValue)
  testOutOfRange(maxValue->Int.toFloat +. value->Int.toFloat, maxValue)
  loop100Fn(() =>
    testOutOfRange(
      randomFloat(maxValue->Int.toFloat +. 1.0, maxSafeInteger)->Js.Math.floor_float,
      maxValue,
    )
  )
  loop100Fn(() =>
    testOutOfRange(
      randomFloat(minSafeInteger, minValue->Int.toFloat -. 1.0)->Js.Math.floor_float,
      minValue,
    )
  )

  let testIsNotInteger = (f, value) => {
    t->Assert.deepEqual(IntModule.fromFloat(f), None, ())
    t->assertInvalidArgument(() => fromFloatExn(f))
    t->Assert.deepEqual(IntModule.fromFloatClamped(f), fromIntExn(value), ())
  }

  testIsNotInteger(value->Int.toFloat +. 0.5, value)

  if isSigned {
    let f = -.(value->Int.toFloat) -. 0.5
    testIsNotInteger(f, f->Float.toInt)
    testIsNotInteger(minValue->Int.toFloat +. 0.1, minValue + 1)
  } else {
    testIsNotInteger(minValue->Int.toFloat +. 0.1, minValue)
  }

  testIsNotInteger(nan, 0)
  testIsNotInteger(neg_infinity, minValue)
  testIsNotInteger(infinity, maxValue)
  testIsNotInteger(minValue->Int.toFloat -. 0.1, minValue)
  testIsNotInteger(maxValue->Int.toFloat +. 0.1, maxValue)
  testIsNotInteger(maxValue->Int.toFloat -. 0.1, maxValue - 1)
  testIsNotInteger(minValue->Int.toFloat -. value->Int.toFloat -. 0.5, minValue)
  testIsNotInteger(maxValue->Int.toFloat +. value->Int.toFloat +. 0.5, maxValue)
})

test(`test ${name} to float`, t => {
  let testToFloat = i => {
    let f = i->Int.toFloat
    t->Assert.deepEqual(fromFloatExn(f)->IntModule.toFloat, f, ())
  }

  testToFloat(value)
  loop100Fn(() => testToFloat(randomValue()))
})

test(`test ${name} from string`, t => {
  let testInRange = (i, s) => {
    let n = fromIntExn(i)
    t->Assert.deepEqual(IntModule.fromString(s), Some(n), ())
    t->Assert.deepEqual(IntModule.fromStringExn(s), n, ())

    for radix in 2 to 36 {
      let s = i->Js.Int.toStringWithRadix(~radix)
      t->Assert.deepEqual(IntModule.fromStringWithRadix(s, ~radix), Some(n), ())
      t->Assert.deepEqual(IntModule.fromStringWithRadixExn(s, ~radix), n, ())
    }
    t->assertInvalidArgument(() => IntModule.fromStringWithRadixExn(s, ~radix=1))
    t->assertInvalidArgument(() => IntModule.fromStringWithRadixExn(s, ~radix=37))
  }

  testInRange(0, "0")
  testInRange(0, "-0")
  testInRange(1, "1")
  testInRange(value, value->Int.toString)
  testInRange(value, (value->Int.toFloat +. 0.5)->Float.toString)
  testInRange(minValue, minValue->Int.toString)
  testInRange(maxValue, maxValue->Int.toString)
  testInRange(minValue + 1, (minValue + 1)->Int.toString)
  testInRange(maxValue - 1, (maxValue - 1)->Int.toString)
  loop100Fn(() => {
    let value = randomValue()
    testInRange(value, value->Int.toString)
  })

  if isSigned {
    testInRange(-value, -value->Int.toString)
    testInRange(-value, (-.(value->Int.toFloat) -. 0.5)->Float.toString)
    testInRange(-maxValue, -maxValue->Int.toString)

    if !is32Bits {
      testInRange(-minValue - 1, (-minValue - 1)->Int.toString)
    }
  }

  let testOutOfRange = f => {
    let s = f->Float.toString
    t->Assert.deepEqual(IntModule.fromString(s), None, ())
    t->assertOverflow(() => IntModule.fromStringExn(s))

    for radix in 2 to 36 {
      let s = f->Js.Float.toStringWithRadix(~radix)
      t->Assert.deepEqual(IntModule.fromStringWithRadix(s, ~radix), None, ())
      t->assertOverflow(() => IntModule.fromStringWithRadixExn(s, ~radix))
    }
    t->assertInvalidArgument(() => IntModule.fromStringWithRadixExn(s, ~radix=1))
    t->assertInvalidArgument(() => IntModule.fromStringWithRadixExn(s, ~radix=37))
  }

  testOutOfRange(minValue->Int.toFloat -. 1.0)
  testOutOfRange(maxValue->Int.toFloat +. 1.0)
  testOutOfRange(minValue->Int.toFloat -. value->Int.toFloat)
  testOutOfRange(maxValue->Int.toFloat +. value->Int.toFloat)

  if !is32Bits {
    loop100Fn(() => testOutOfRange(randomLargerInt()->Int.toFloat))
    loop100Fn(() => testOutOfRange(randomSmallerInt()->Int.toFloat))
  }

  let testIsNotInteger = s => {
    t->Assert.deepEqual(IntModule.fromString(s), None, ())
    t->assertInvalidArgument(() => IntModule.fromStringExn(s))

    for radix in 2 to 10 {
      t->Assert.deepEqual(IntModule.fromStringWithRadix(s, ~radix), None, ())
      t->assertInvalidArgument(() => IntModule.fromStringWithRadixExn(s, ~radix))
    }
    t->assertInvalidArgument(() => IntModule.fromStringWithRadixExn(s, ~radix=1))
    t->assertInvalidArgument(() => IntModule.fromStringWithRadixExn(s, ~radix=37))
  }

  testIsNotInteger("NaN")
  testIsNotInteger("Infinity")
  testIsNotInteger("-Infinity")
  testIsNotInteger("foo")
  testIsNotInteger("b1")
})

test(`test ${name} to string`, t => {
  let testToString = i => {
    let s = i->Int.toString
    let n = fromIntExn(i)
    t->Assert.deepEqual(n->IntModule.toString, s, ())

    for radix in 2 to 36 {
      let s = i->Js.Int.toStringWithRadix(~radix)
      t->Assert.deepEqual(n->IntModule.toStringWithRadixExn(~radix), s, ())
    }
    t->Assert.throws(() => n->IntModule.toStringWithRadixExn(~radix=1), ())
    t->Assert.throws(() => n->IntModule.toStringWithRadixExn(~radix=37), ())
  }

  testFn(testToString)

  let testToExponential = i => {
    let s = i->Js.Int.toExponential
    let n = fromIntExn(i)
    t->Assert.deepEqual(n->IntModule.toExponential, s, ())

    for digits in 0 to 100 {
      let s = i->Js.Int.toExponentialWithPrecision(~digits)
      t->Assert.deepEqual(n->IntModule.toExponentialWithPrecisionExn(~digits), s, ())
    }
    t->Assert.throws(() => n->IntModule.toExponentialWithPrecisionExn(~digits=-1), ())
    t->Assert.throws(() => n->IntModule.toExponentialWithPrecisionExn(~digits=101), ())
  }

  testFn(testToExponential)

  let testToPrecision = i => {
    let s = i->Js.Int.toPrecision
    let n = fromIntExn(i)
    t->Assert.deepEqual(n->IntModule.toPrecision, s, ())

    for digits in 1 to 100 {
      let s = i->Js.Int.toPrecisionWithPrecision(~digits)
      t->Assert.deepEqual(n->IntModule.toPrecisionWithPrecisionExn(~digits), s, ())
    }
    t->Assert.throws(() => n->IntModule.toPrecisionWithPrecisionExn(~digits=0), ())
    t->Assert.throws(() => n->IntModule.toPrecisionWithPrecisionExn(~digits=101), ())
  }

  testFn(testToPrecision)
})

test(`test ${name} zero`, t => {
  t->Assert.deepEqual(zero, fromIntExn(0), ())
  t->Assert.deepEqual(zero, fromFloatExn(0.0), ())
  t->Assert.deepEqual(zero, fromFloatExn(-0.0), ())
})

test(`test ${name} one`, t => {
  t->Assert.deepEqual(one, fromIntExn(1), ())
  t->Assert.deepEqual(one, fromFloatExn(1.0), ())
})

test(`test ${name} range`, t => {
  t->Assert.deepEqual(IntModule.minValue, fromIntExn(minValue), ())
  t->Assert.deepEqual(IntModule.minValue, fromFloatExn(minValue->Int.toFloat), ())
  t->Assert.deepEqual(IntModule.maxValue, fromIntExn(maxValue), ())
  t->Assert.deepEqual(IntModule.maxValue, fromFloatExn(maxValue->Int.toFloat), ())
})

test(`test ${name} comparison`, t => {
  let testCompare = (less, greater) => {
    let less = fromIntExn(less)
    let greater = fromIntExn(greater)
    let value = fromIntExn(value)
    let minValue = fromIntExn(minValue)
    let maxValue = fromIntExn(maxValue)

    t->Assert.deepEqual(IntModule.compare(less, greater), Some(Less), ())
    t->Assert.deepEqual(IntModule.compare(greater, less), Some(Greater), ())
    t->Assert.deepEqual(IntModule.compare(less, less), Some(Equal), ())
    t->Assert.deepEqual(IntModule.compare(greater, greater), Some(Equal), ())

    t->Assert.deepEqual(IntModule.compareExn(less, greater), Less, ())
    t->Assert.deepEqual(IntModule.compareExn(greater, less), Greater, ())
    t->Assert.deepEqual(IntModule.compareExn(less, less), Equal, ())
    t->Assert.deepEqual(IntModule.compareExn(greater, greater), Equal, ())

    t->Assert.isTrue(less->IntModule.eq(less), ())
    t->Assert.isTrue(greater->IntModule.eq(greater), ())
    t->Assert.isFalse(less->IntModule.eq(greater), ())
    t->Assert.isFalse(greater->IntModule.eq(less), ())

    t->Assert.isTrue(less->IntModule.ne(greater), ())
    t->Assert.isTrue(greater->IntModule.ne(less), ())
    t->Assert.isFalse(less->IntModule.ne(less), ())
    t->Assert.isFalse(greater->IntModule.ne(greater), ())

    t->Assert.isTrue(less->IntModule.lt(greater), ())
    t->Assert.isFalse(greater->IntModule.lt(less), ())
    t->Assert.isFalse(less->IntModule.lt(less), ())
    t->Assert.isFalse(greater->IntModule.lt(greater), ())

    t->Assert.isTrue(less->IntModule.le(greater), ())
    t->Assert.isFalse(greater->IntModule.le(less), ())
    t->Assert.isTrue(less->IntModule.le(less), ())
    t->Assert.isTrue(greater->IntModule.le(greater), ())

    t->Assert.isTrue(greater->IntModule.gt(less), ())
    t->Assert.isFalse(less->IntModule.gt(greater), ())
    t->Assert.isFalse(less->IntModule.gt(less), ())
    t->Assert.isFalse(greater->IntModule.gt(greater), ())

    t->Assert.isTrue(greater->IntModule.ge(less), ())
    t->Assert.isFalse(less->IntModule.ge(greater), ())
    t->Assert.isTrue(less->IntModule.ge(less), ())
    t->Assert.isTrue(greater->IntModule.ge(greater), ())

    t->Assert.deepEqual(IntModule.min(less, greater), less, ())
    t->Assert.deepEqual(IntModule.min(greater, less), less, ())
    t->Assert.deepEqual(IntModule.min(less, less), less, ())
    t->Assert.deepEqual(IntModule.min(greater, greater), greater, ())

    t->Assert.deepEqual(IntModule.minMany([less]), Some(less), ())
    t->Assert.deepEqual(IntModule.minMany([less, greater]), Some(less), ())
    t->Assert.deepEqual(IntModule.minMany([greater, less]), Some(less), ())
    t->Assert.deepEqual(
      IntModule.minMany([less, value, greater, minValue, maxValue]),
      Some(minValue),
      (),
    )
    t->Assert.deepEqual(IntModule.minManyExn([less]), less, ())
    t->Assert.deepEqual(IntModule.minManyExn([less, greater]), less, ())
    t->Assert.deepEqual(IntModule.minManyExn([greater, less]), less, ())
    t->Assert.deepEqual(
      IntModule.minManyExn([minValue, maxValue, less, value, greater]),
      minValue,
      (),
    )
    t->Assert.deepEqual(IntModule.minManyUnsafe([less]), less, ())
    t->Assert.deepEqual(IntModule.minManyUnsafe([less, greater]), less, ())
    t->Assert.deepEqual(IntModule.minManyUnsafe([greater, less]), less, ())
    t->Assert.deepEqual(
      IntModule.minManyUnsafe([less, minValue, maxValue, value, greater]),
      minValue,
      (),
    )

    t->Assert.deepEqual(IntModule.max(less, greater), greater, ())
    t->Assert.deepEqual(IntModule.max(greater, less), greater, ())
    t->Assert.deepEqual(IntModule.max(less, less), less, ())
    t->Assert.deepEqual(IntModule.max(greater, greater), greater, ())

    t->Assert.deepEqual(IntModule.maxMany([less]), Some(less), ())
    t->Assert.deepEqual(IntModule.maxMany([less, greater]), Some(greater), ())
    t->Assert.deepEqual(IntModule.maxMany([greater, less]), Some(greater), ())
    t->Assert.deepEqual(
      IntModule.maxMany([less, value, greater, minValue, maxValue]),
      Some(maxValue),
      (),
    )
    t->Assert.deepEqual(IntModule.maxManyExn([less]), less, ())
    t->Assert.deepEqual(IntModule.maxManyExn([less, greater]), greater, ())
    t->Assert.deepEqual(IntModule.maxManyExn([greater, less]), greater, ())
    t->Assert.deepEqual(
      IntModule.maxManyExn([minValue, maxValue, less, value, greater]),
      maxValue,
      (),
    )
    t->Assert.deepEqual(IntModule.maxManyUnsafe([less]), less, ())
    t->Assert.deepEqual(IntModule.maxManyUnsafe([less, greater]), greater, ())
    t->Assert.deepEqual(IntModule.maxManyUnsafe([greater, less]), greater, ())
    t->Assert.deepEqual(
      IntModule.maxManyUnsafe([less, minValue, maxValue, value, greater]),
      maxValue,
      (),
    )
  }

  testCompare(less, greater)
  loop100Fn(() => {
    let less = randomValue()
    let greater = randomInt(less->Int.toFloat +. 1.0, maxValue->Int.toFloat +. 1.0)
    testCompare(less, greater)
  })

  let zero = fromIntExn(0)
  let negZero = fromIntExn(negZero)
  t->Assert.isTrue(zero->IntModule.eq(negZero), ())
  t->Assert.isTrue(zero->IntModule.le(negZero), ())
  t->Assert.isTrue(zero->IntModule.ge(negZero), ())
  t->Assert.isFalse(zero->IntModule.lt(negZero), ())
  t->Assert.isFalse(zero->IntModule.gt(negZero), ())

  t->Assert.deepEqual(IntModule.minMany([]), None, ())
  t->assertInvalidArgument(() => IntModule.minManyExn([]))
  t->Assert.deepEqual(IntModule.maxMany([]), None, ())
  t->assertInvalidArgument(() => IntModule.maxManyExn([]))
})

test(`test ${name} addition`, t => {
  let testInRange = (a, b) => {
    if isSigned {
      if a !== minValue {
        let negA = fromIntExn(-a)
        let a = fromIntExn(a)
        t->Assert.deepEqual(a->IntModule.add(negA), Some(zero), ())
        t->Assert.deepEqual(a->IntModule.add(negA), negA->IntModule.add(a), ())
        t->Assert.deepEqual(a->IntModule.addExn(negA), zero, ())
        t->Assert.deepEqual(a->IntModule.addExn(negA), negA->IntModule.addExn(a), ())
        t->Assert.deepEqual(a->IntModule.addClamped(negA), zero, ())
        t->Assert.deepEqual(a->IntModule.addClamped(negA), negA->IntModule.addClamped(a), ())
        t->Assert.deepEqual(a->IntModule.addUnsafe(negA), zero, ())
        t->Assert.deepEqual(a->IntModule.addUnsafe(negA), negA->IntModule.addUnsafe(a), ())
      }

      if b !== minValue {
        let negB = fromIntExn(-b)
        let b = fromIntExn(b)
        t->Assert.deepEqual(b->IntModule.add(negB), Some(zero), ())
        t->Assert.deepEqual(b->IntModule.add(negB), negB->IntModule.add(b), ())
        t->Assert.deepEqual(b->IntModule.addExn(negB), zero, ())
        t->Assert.deepEqual(b->IntModule.addExn(negB), negB->IntModule.addExn(b), ())
        t->Assert.deepEqual(b->IntModule.addClamped(negB), zero, ())
        t->Assert.deepEqual(b->IntModule.addClamped(negB), negB->IntModule.addClamped(b), ())
        t->Assert.deepEqual(b->IntModule.addUnsafe(negB), zero, ())
        t->Assert.deepEqual(b->IntModule.addUnsafe(negB), negB->IntModule.addUnsafe(b), ())
      }
    }

    let result = fromIntExn(a + b)
    let a = fromIntExn(a)
    let b = fromIntExn(b)
    t->Assert.deepEqual(a->IntModule.add(b), Some(result), ())
    t->Assert.deepEqual(a->IntModule.add(b), b->IntModule.add(a), ())
    t->Assert.deepEqual(a->IntModule.addExn(b), result, ())
    t->Assert.deepEqual(a->IntModule.addExn(b), b->IntModule.addExn(a), ())
    t->Assert.deepEqual(a->IntModule.addClamped(b), result, ())
    t->Assert.deepEqual(a->IntModule.addClamped(b), b->IntModule.addClamped(a), ())
    t->Assert.deepEqual(a->IntModule.addUnsafe(b), result, ())
    t->Assert.deepEqual(a->IntModule.addUnsafe(b), b->IntModule.addUnsafe(a), ())

    t->Assert.deepEqual(a->IntModule.add(zero), Some(a), ())
    t->Assert.deepEqual(a->IntModule.add(zero), zero->IntModule.add(a), ())
    t->Assert.deepEqual(a->IntModule.addExn(zero), a, ())
    t->Assert.deepEqual(a->IntModule.addExn(zero), zero->IntModule.addExn(a), ())
    t->Assert.deepEqual(a->IntModule.addClamped(zero), a, ())
    t->Assert.deepEqual(a->IntModule.addClamped(zero), zero->IntModule.addClamped(a), ())
    t->Assert.deepEqual(a->IntModule.addUnsafe(zero), a, ())
    t->Assert.deepEqual(a->IntModule.addUnsafe(zero), zero->IntModule.addUnsafe(a), ())

    t->Assert.deepEqual(b->IntModule.add(zero), Some(b), ())
    t->Assert.deepEqual(b->IntModule.add(zero), zero->IntModule.add(b), ())
    t->Assert.deepEqual(b->IntModule.addExn(zero), b, ())
    t->Assert.deepEqual(b->IntModule.addExn(zero), zero->IntModule.addExn(b), ())
    t->Assert.deepEqual(b->IntModule.addClamped(zero), b, ())
    t->Assert.deepEqual(b->IntModule.addClamped(zero), zero->IntModule.addClamped(b), ())
    t->Assert.deepEqual(b->IntModule.addUnsafe(zero), b, ())
    t->Assert.deepEqual(b->IntModule.addUnsafe(zero), zero->IntModule.addUnsafe(b), ())
  }

  testInRange(less, greater)
  testInRange(minValue, 0)
  testInRange(maxValue, 0)
  testInRange(minValue, 1)

  if isSigned {
    testInRange(maxValue, -1)
  }

  let testOutOfRange = (a, b, value) => {
    let a = fromIntExn(a)
    let b = fromIntExn(b)
    let value = fromIntExn(value)
    t->Assert.deepEqual(a->IntModule.add(b), None, ())
    t->Assert.deepEqual(b->IntModule.add(a), None, ())
    t->assertOverflow(() => a->IntModule.addExn(b))
    t->assertOverflow(() => b->IntModule.addExn(a))
    t->Assert.deepEqual(a->IntModule.addClamped(b), value, ())
    t->Assert.deepEqual(b->IntModule.addClamped(a), value, ())
  }

  if isSigned {
    testOutOfRange(minValue, -1, minValue)
    testOutOfRange(minValue, -value, minValue)
  }

  testOutOfRange(maxValue, 1, maxValue)
  testOutOfRange(maxValue, value, maxValue)

  loop100Fn(() => {
    let a = randomValue()
    let b = randomValue()
    let result = a->Int.toFloat +. b->Int.toFloat
    if result < minValue->Int.toFloat {
      testOutOfRange(a, b, minValue)
    } else if result > maxValue->Int.toFloat {
      testOutOfRange(a, b, maxValue)
    } else {
      testInRange(a, b)
    }
    testInRange(a, 1)
    testInRange(b, 1)
  })
})

test(`test ${name} subtraction`, t => {
  let testInRange = (a, b) => {
    let result = fromIntExn(a - b)
    let a = fromIntExn(a)
    let b = fromIntExn(b)
    t->Assert.deepEqual(a->IntModule.sub(b), Some(result), ())
    t->Assert.deepEqual(a->IntModule.subExn(b), result, ())
    t->Assert.deepEqual(a->IntModule.subClamped(b), result, ())
    t->Assert.deepEqual(a->IntModule.subUnsafe(b), result, ())
    t->Assert.deepEqual(a->IntModule.sub(result), Some(b), ())
    t->Assert.deepEqual(a->IntModule.subExn(result), b, ())
    t->Assert.deepEqual(a->IntModule.subClamped(result), b, ())
    t->Assert.deepEqual(a->IntModule.subUnsafe(result), b, ())

    t->Assert.deepEqual(a->IntModule.sub(a), Some(zero), ())
    t->Assert.deepEqual(a->IntModule.subExn(a), zero, ())
    t->Assert.deepEqual(a->IntModule.subClamped(a), zero, ())
    t->Assert.deepEqual(a->IntModule.subUnsafe(a), zero, ())
    t->Assert.deepEqual(a->IntModule.sub(zero), Some(a), ())
    t->Assert.deepEqual(a->IntModule.subExn(zero), a, ())
    t->Assert.deepEqual(a->IntModule.subClamped(zero), a, ())
    t->Assert.deepEqual(a->IntModule.subUnsafe(zero), a, ())

    t->Assert.deepEqual(b->IntModule.sub(b), Some(zero), ())
    t->Assert.deepEqual(b->IntModule.subExn(b), zero, ())
    t->Assert.deepEqual(b->IntModule.subClamped(b), zero, ())
    t->Assert.deepEqual(b->IntModule.subUnsafe(b), zero, ())
    t->Assert.deepEqual(b->IntModule.sub(zero), Some(b), ())
    t->Assert.deepEqual(b->IntModule.subExn(zero), b, ())
    t->Assert.deepEqual(b->IntModule.subClamped(zero), b, ())
    t->Assert.deepEqual(b->IntModule.subUnsafe(zero), b, ())
  }

  testInRange(greater, less)
  testInRange(minValue, 0)
  testInRange(maxValue, 0)
  testInRange(maxValue, 1)

  if isSigned {
    testInRange(less, greater)
    testInRange(minValue, -1)
    testInRange(0, maxValue)
  }

  let testOutOfRange = (a, b, value) => {
    let a = fromIntExn(a)
    let b = fromIntExn(b)
    t->Assert.deepEqual(a->IntModule.sub(b), None, ())
    t->assertOverflow(() => a->IntModule.subExn(b))
    t->Assert.deepEqual(a->IntModule.subClamped(b), fromIntExn(value), ())
  }

  testOutOfRange(minValue, 1, minValue)
  testOutOfRange(minValue, value, minValue)

  if isSigned {
    testOutOfRange(maxValue, -1, maxValue)
    testOutOfRange(maxValue, -value, maxValue)
  }

  loop100Fn(() => {
    let a = randomValue()
    let b = randomValue()

    let result = a->Int.toFloat -. b->Int.toFloat
    if result < minValue->Int.toFloat {
      testOutOfRange(a, b, minValue)
    } else if result > maxValue->Int.toFloat {
      testOutOfRange(a, b, maxValue)
    } else {
      testInRange(a, b)
    }

    let result = b->Int.toFloat -. a->Int.toFloat
    if result < minValue->Int.toFloat {
      testOutOfRange(b, a, minValue)
    } else if result > maxValue->Int.toFloat {
      testOutOfRange(b, a, maxValue)
    } else {
      testInRange(b, a)
    }
  })
})

test(`test ${name} multiplication`, t => {
  let testInRange = (a, b) => {
    let result = fromIntExn(a * b)
    let a = fromIntExn(a)
    let b = fromIntExn(b)
    t->Assert.deepEqual(a->IntModule.mul(b), Some(result), ())
    t->Assert.deepEqual(a->IntModule.mul(b), b->IntModule.mul(a), ())
    t->Assert.deepEqual(a->IntModule.mulExn(b), result, ())
    t->Assert.deepEqual(a->IntModule.mulExn(b), b->IntModule.mulExn(a), ())
    t->Assert.deepEqual(a->IntModule.mulClamped(b), result, ())
    t->Assert.deepEqual(a->IntModule.mulClamped(b), b->IntModule.mulClamped(a), ())
    t->Assert.deepEqual(a->IntModule.mulUnsafe(b), result, ())
    t->Assert.deepEqual(a->IntModule.mulUnsafe(b), b->IntModule.mulUnsafe(a), ())

    t->Assert.deepEqual(a->IntModule.mul(zero), Some(zero), ())
    t->Assert.deepEqual(a->IntModule.mul(zero), zero->IntModule.mul(a), ())
    t->Assert.deepEqual(a->IntModule.mulExn(zero), zero, ())
    t->Assert.deepEqual(a->IntModule.mulExn(zero), zero->IntModule.mulExn(a), ())
    t->Assert.deepEqual(a->IntModule.mulClamped(zero), zero, ())
    t->Assert.deepEqual(a->IntModule.mulClamped(zero), zero->IntModule.mulClamped(a), ())
    t->Assert.deepEqual(a->IntModule.mulUnsafe(zero), zero, ())
    t->Assert.deepEqual(a->IntModule.mulUnsafe(zero), zero->IntModule.mulUnsafe(a), ())
    t->Assert.deepEqual(a->IntModule.mul(one), Some(a), ())
    t->Assert.deepEqual(a->IntModule.mul(one), one->IntModule.mul(a), ())
    t->Assert.deepEqual(a->IntModule.mulExn(one), a, ())
    t->Assert.deepEqual(a->IntModule.mulExn(one), one->IntModule.mulExn(a), ())
    t->Assert.deepEqual(a->IntModule.mulClamped(one), a, ())
    t->Assert.deepEqual(a->IntModule.mulClamped(one), one->IntModule.mulClamped(a), ())
    t->Assert.deepEqual(a->IntModule.mulUnsafe(one), a, ())
    t->Assert.deepEqual(a->IntModule.mulUnsafe(one), one->IntModule.mulUnsafe(a), ())

    t->Assert.deepEqual(b->IntModule.mul(zero), Some(zero), ())
    t->Assert.deepEqual(b->IntModule.mul(zero), zero->IntModule.mul(b), ())
    t->Assert.deepEqual(b->IntModule.mulExn(zero), zero, ())
    t->Assert.deepEqual(b->IntModule.mulExn(zero), zero->IntModule.mulExn(b), ())
    t->Assert.deepEqual(b->IntModule.mulClamped(zero), zero, ())
    t->Assert.deepEqual(b->IntModule.mulClamped(zero), zero->IntModule.mulClamped(b), ())
    t->Assert.deepEqual(b->IntModule.mulUnsafe(zero), zero, ())
    t->Assert.deepEqual(b->IntModule.mulUnsafe(zero), zero->IntModule.mulUnsafe(b), ())
    t->Assert.deepEqual(b->IntModule.mul(one), Some(b), ())
    t->Assert.deepEqual(b->IntModule.mul(one), one->IntModule.mul(b), ())
    t->Assert.deepEqual(b->IntModule.mulExn(one), b, ())
    t->Assert.deepEqual(b->IntModule.mulExn(one), one->IntModule.mulExn(b), ())
    t->Assert.deepEqual(b->IntModule.mulClamped(one), b, ())
    t->Assert.deepEqual(b->IntModule.mulClamped(one), one->IntModule.mulClamped(b), ())
    t->Assert.deepEqual(b->IntModule.mulUnsafe(one), b, ())
    t->Assert.deepEqual(b->IntModule.mulUnsafe(one), one->IntModule.mulUnsafe(b), ())
  }

  testInRange(less, greater)
  testInRange(minValue, 0)
  testInRange(maxValue, 0)
  testInRange(minValue, 1)
  testInRange(maxValue, 1)
  testInRange(minValue + 1, 1)
  testInRange(maxValue - 1, 1)

  if isSigned {
    testInRange(-maxValue, 1)
    testInRange(maxValue, -1)
  }

  let testOutOfRange = (a, b, value) => {
    let a = fromIntExn(a)
    let b = fromIntExn(b)
    let value = fromIntExn(value)
    t->Assert.deepEqual(a->IntModule.mul(b), None, ())
    t->Assert.deepEqual(b->IntModule.mul(a), None, ())
    t->assertOverflow(() => a->IntModule.mulExn(b))
    t->assertOverflow(() => b->IntModule.mulExn(a))
    t->Assert.deepEqual(a->IntModule.mulClamped(b), value, ())
    t->Assert.deepEqual(b->IntModule.mulClamped(a), value, ())
  }

  testOutOfRange(maxValue, 2, maxValue)
  testOutOfRange(maxValue, value, maxValue)

  if isSigned {
    testOutOfRange(minValue, 2, minValue)
    testOutOfRange(minValue, value, minValue)
    testOutOfRange(minValue, -1, maxValue)
    testOutOfRange(maxValue, -2, minValue)
    testOutOfRange(minValue, -value, maxValue)
    testOutOfRange(maxValue, -value, minValue)
  }

  loop100Fn(() => {
    let a = randomValue()
    let b = randomValue()
    let result = a->Int.toFloat *. b->Int.toFloat
    if result < minValue->Int.toFloat {
      testOutOfRange(a, b, minValue)
    } else if result > maxValue->Int.toFloat {
      testOutOfRange(a, b, maxValue)
    } else {
      testInRange(a, b)
    }
  })
})

test(`test ${name} division`, t => {
  let testNotDividedByZero = (a, b) => {
    if isSigned {
      let negOne = fromIntExn(-1)

      if a !== minValue {
        let negA = fromIntExn(-a)
        let a = fromIntExn(a)
        t->Assert.deepEqual(a->IntModule.div(negOne), Some(negA), ())
        t->Assert.deepEqual(a->IntModule.divExn(negOne), negA, ())
        t->Assert.deepEqual(a->IntModule.divUnsafe(negOne), negA, ())
      }

      if b !== minValue {
        let negB = fromIntExn(-b)
        let b = fromIntExn(b)
        t->Assert.deepEqual(b->IntModule.div(negOne), Some(negB), ())
        t->Assert.deepEqual(b->IntModule.divExn(negOne), negB, ())
        t->Assert.deepEqual(b->IntModule.divUnsafe(negOne), negB, ())
      }
    }

    if a !== 0 {
      let result = fromIntExn(b / a)
      let a = fromIntExn(a)
      let b = fromIntExn(b)
      t->Assert.deepEqual(b->IntModule.div(a), Some(result), ())
      t->Assert.deepEqual(b->IntModule.divExn(a), result, ())
      t->Assert.deepEqual(b->IntModule.divUnsafe(a), result, ())
      t->Assert.deepEqual(a->IntModule.div(a), Some(one), ())
      t->Assert.deepEqual(a->IntModule.divExn(a), one, ())
      t->Assert.deepEqual(a->IntModule.divUnsafe(a), one, ())
      t->Assert.deepEqual(zero->IntModule.div(a), Some(zero), ())
      t->Assert.deepEqual(zero->IntModule.divExn(a), zero, ())
      t->Assert.deepEqual(zero->IntModule.divUnsafe(a), zero, ())
    }

    if b !== 0 {
      let result = fromIntExn(a / b)
      let a = fromIntExn(a)
      let b = fromIntExn(b)
      t->Assert.deepEqual(a->IntModule.div(b), Some(result), ())
      t->Assert.deepEqual(a->IntModule.divExn(b), result, ())
      t->Assert.deepEqual(a->IntModule.divUnsafe(b), result, ())
      t->Assert.deepEqual(b->IntModule.div(b), Some(one), ())
      t->Assert.deepEqual(b->IntModule.divExn(b), one, ())
      t->Assert.deepEqual(b->IntModule.divUnsafe(b), one, ())
      t->Assert.deepEqual(zero->IntModule.div(b), Some(zero), ())
      t->Assert.deepEqual(zero->IntModule.divExn(b), zero, ())
      t->Assert.deepEqual(zero->IntModule.divUnsafe(b), zero, ())
    }

    let a = fromIntExn(a)
    let b = fromIntExn(b)
    t->Assert.deepEqual(a->IntModule.div(one), Some(a), ())
    t->Assert.deepEqual(a->IntModule.divExn(one), a, ())
    t->Assert.deepEqual(a->IntModule.divUnsafe(one), a, ())
    t->Assert.deepEqual(b->IntModule.div(one), Some(b), ())
    t->Assert.deepEqual(b->IntModule.divExn(one), b, ())
    t->Assert.deepEqual(b->IntModule.divUnsafe(one), b, ())
  }

  testNotDividedByZero(0, value)
  testNotDividedByZero(negZero, value)
  testNotDividedByZero(less, greater)
  testNotDividedByZero(minValue, less)
  testNotDividedByZero(minValue, greater)
  testNotDividedByZero(maxValue, less)
  testNotDividedByZero(maxValue, greater)
  testNotDividedByZero(less, 2)
  testNotDividedByZero(greater, 2)
  testNotDividedByZero(minValue, 2)
  testNotDividedByZero(maxValue, 2)
  testNotDividedByZero(minValue + 1, 2)
  testNotDividedByZero(maxValue - 1, 2)

  if isSigned {
    testNotDividedByZero(less, -greater)
    testNotDividedByZero(greater, -less)
    testNotDividedByZero(-less, -greater)
    testNotDividedByZero(minValue, -less)
    testNotDividedByZero(minValue, -greater)
    testNotDividedByZero(maxValue, -less)
    testNotDividedByZero(maxValue, -greater)
    testNotDividedByZero(-maxValue, -less)
    testNotDividedByZero(-maxValue, -greater)
    testNotDividedByZero(less, -2)
    testNotDividedByZero(greater, -2)
    testNotDividedByZero(minValue, -2)
    testNotDividedByZero(maxValue, -2)
    testNotDividedByZero(minValue + 1, -2)
    testNotDividedByZero(maxValue - 1, -2)
  }

  let testDividedByZero = i => {
    let a = fromIntExn(i)
    t->Assert.deepEqual(a->IntModule.div(zero), None, ())
    t->assertDivisionByZero(() => a->IntModule.divExn(zero))
    let negZero = fromIntExn(negZero)
    t->Assert.deepEqual(a->IntModule.div(negZero), None, ())
    t->assertDivisionByZero(() => a->IntModule.divExn(negZero))
  }

  testFn(testDividedByZero)

  loop100Fn(() => {
    let a = randomValue()
    let b = randomValue()
    testNotDividedByZero(a, b)
    testDividedByZero(a)
    testDividedByZero(b)
  })
})

test(`test ${name} remainder`, t => {
  let testNotModByZero = (a, b) => {
    if a !== 0 {
      let result = fromIntExn(b->mod(a)->lor(0))
      let a = fromIntExn(a)
      let b = fromIntExn(b)
      t->Assert.deepEqual(b->IntModule.rem(a), Some(result), ())
      t->Assert.deepEqual(b->IntModule.remExn(a), result, ())
      t->Assert.deepEqual(b->IntModule.remUnsafe(a), result, ())
      t->Assert.deepEqual(zero->IntModule.rem(a), Some(zero), ())
      t->Assert.deepEqual(zero->IntModule.remExn(a), zero, ())
      t->Assert.deepEqual(zero->IntModule.remUnsafe(a), zero, ())
    }

    if b !== 0 {
      let result = fromIntExn(a->mod(b)->lor(0))
      let a = fromIntExn(a)
      let b = fromIntExn(b)
      t->Assert.deepEqual(a->IntModule.rem(b), Some(result), ())
      t->Assert.deepEqual(a->IntModule.remExn(b), result, ())
      t->Assert.deepEqual(a->IntModule.remUnsafe(b), result, ())
      t->Assert.deepEqual(zero->IntModule.rem(b), Some(zero), ())
      t->Assert.deepEqual(zero->IntModule.remExn(b), zero, ())
      t->Assert.deepEqual(zero->IntModule.remUnsafe(b), zero, ())
    }

    let a = fromIntExn(a)
    let b = fromIntExn(b)
    t->Assert.deepEqual(a->IntModule.rem(one), Some(zero), ())
    t->Assert.deepEqual(a->IntModule.remExn(one), zero, ())
    t->Assert.deepEqual(a->IntModule.remUnsafe(one), zero, ())
    t->Assert.deepEqual(b->IntModule.rem(one), Some(zero), ())
    t->Assert.deepEqual(b->IntModule.remExn(one), zero, ())
    t->Assert.deepEqual(b->IntModule.remUnsafe(one), zero, ())

    if isSigned {
      let negOne = fromIntExn(-1)
      t->Assert.deepEqual(a->IntModule.rem(negOne), Some(zero), ())
      t->Assert.deepEqual(a->IntModule.remExn(negOne), zero, ())
      t->Assert.deepEqual(a->IntModule.remUnsafe(negOne), zero, ())
      t->Assert.deepEqual(b->IntModule.rem(negOne), Some(zero), ())
      t->Assert.deepEqual(b->IntModule.remExn(negOne), zero, ())
      t->Assert.deepEqual(b->IntModule.remUnsafe(negOne), zero, ())
    }
  }

  testNotModByZero(less, greater)
  testNotModByZero(minValue, less)
  testNotModByZero(minValue, greater)
  testNotModByZero(maxValue, less)
  testNotModByZero(maxValue, greater)
  testNotModByZero(less, 2)
  testNotModByZero(greater, 2)
  testNotModByZero(minValue, 2)
  testNotModByZero(maxValue, 2)
  testNotModByZero(minValue + 1, 2)
  testNotModByZero(maxValue - 1, 2)

  if isSigned {
    testNotModByZero(less, -greater)
    testNotModByZero(greater, -less)
    testNotModByZero(-less, -greater)
    testNotModByZero(minValue, -less)
    testNotModByZero(minValue, -greater)
    testNotModByZero(maxValue, -less)
    testNotModByZero(maxValue, -greater)
    testNotModByZero(-maxValue, -less)
    testNotModByZero(-maxValue, -greater)
    testNotModByZero(less, -2)
    testNotModByZero(greater, -2)
    testNotModByZero(minValue, -2)
    testNotModByZero(maxValue, -2)
    testNotModByZero(minValue + 1, -2)
    testNotModByZero(maxValue - 1, -2)
  }

  let testModByZero = a => {
    let a = fromIntExn(a)
    t->Assert.deepEqual(a->IntModule.rem(zero), None, ())
    t->assertDivisionByZero(() => a->IntModule.remExn(zero))
    let negZero = fromIntExn(negZero)
    t->Assert.deepEqual(a->IntModule.rem(negZero), None, ())
    t->assertDivisionByZero(() => a->IntModule.remExn(negZero))
  }

  testFn(testModByZero)

  loop100Fn(() => {
    let a = randomValue()
    let b = randomValue()
    testNotModByZero(a, b)
    testModByZero(a)
    testModByZero(b)
  })
})

test(`test ${name} sum`, t => {
  let testInRange = arr => {
    let result = fromIntExn(arr->Array.reduce(0, (acc, v) => acc + v))
    let arr = arr->Array.mapU(i => fromIntExn(i))
    t->Assert.deepEqual(arr->IntModule.sum, Some(result), ())
    t->Assert.deepEqual(arr->IntModule.sumExn, result, ())
    t->Assert.deepEqual(arr->IntModule.sumUnsafe, result, ())
  }

  testInRange([less])
  testInRange([greater])
  testInRange([minValue])
  testInRange([maxValue])
  testInRange([minValue + 1])
  testInRange([maxValue - 1])
  testInRange([less, greater])
  testInRange([greater, less])
  testInRange([less, 0, value, 1, greater])
  testInRange([minValue, maxValue])

  t->Assert.deepEqual(IntModule.sum([]), None, ())
  t->assertInvalidArgument(() => IntModule.sumExn([]))

  let testOutOfRange = arr => {
    let arr = arr->Array.mapU(i => fromIntExn(i))
    t->Assert.deepEqual(arr->IntModule.sum, None, ())
    t->assertOverflow(() => arr->IntModule.sumExn)
  }

  testOutOfRange([maxValue, 1])
  testOutOfRange([1, maxValue])
  testOutOfRange([maxValue, value])
  testOutOfRange([value, maxValue])
  testOutOfRange([maxValue, greater - less, value])

  if isSigned {
    testOutOfRange([minValue, -1])
    testOutOfRange([-1, minValue])
    testOutOfRange([minValue, -value])
    testOutOfRange([-value, minValue])
    testOutOfRange([minValue, less - greater, -value])
  }

  loop100Fn(() => {
    let a = randomValue()
    let b = randomValue()
    testInRange([a])
    testInRange([b])

    let result = a->Int.toFloat +. b->Int.toFloat
    if result >= minValue->Int.toFloat && result <= maxValue->Int.toFloat {
      testInRange([a, b])
      testInRange([b, a])
    } else {
      testOutOfRange([a, b])
      testOutOfRange([b, a])
    }
  })
})

test(`test ${name} increase and decrease`, t => {
  let testIncInRange = i => {
    let result = fromIntExn(i + 1)
    let n = fromIntExn(i)
    t->Assert.deepEqual(n->IntModule.inc, Some(result), ())
    t->Assert.deepEqual(n->IntModule.incExn, result, ())
    t->Assert.deepEqual(n->IntModule.incUnsafe, result, ())
  }

  testIncInRange(0)
  testIncInRange(1)
  testIncInRange(value)
  testIncInRange(minValue)
  testIncInRange(minValue + 1)
  testIncInRange(maxValue - 1)
  loop100Fn(() => testIncInRange(randomValue()))

  if isSigned {
    testIncInRange(-value)
    testIncInRange(-maxValue)
  }

  t->Assert.deepEqual(IntModule.maxValue->IntModule.inc, None, ())
  t->assertOverflow(() => IntModule.maxValue->IntModule.incExn)

  let testDecInRange = i => {
    let result = fromIntExn(i - 1)
    let n = fromIntExn(i)
    t->Assert.deepEqual(n->IntModule.dec, Some(result), ())
    t->Assert.deepEqual(n->IntModule.decExn, result, ())
    t->Assert.deepEqual(n->IntModule.decUnsafe, result, ())
  }

  testDecInRange(1)
  testDecInRange(value)
  testDecInRange(minValue + 1)
  testDecInRange(maxValue)
  testDecInRange(maxValue - 1)
  loop100Fn(() => testDecInRange(randomInt(minValue->Int.toFloat +. 1.0, maxValue->Int.toFloat)))

  if isSigned {
    testDecInRange(0)
    testDecInRange(-value)
    testDecInRange(-maxValue)
  }

  t->Assert.deepEqual(IntModule.minValue->IntModule.dec, None, ())
  t->assertOverflow(() => IntModule.minValue->IntModule.decExn)
})

test(`test ${name} bitwise`, t => {
  let assertNot1 = (a, b) => {
    let a = fromIntExn(a)
    let b = fromIntExn(b)
    t->Assert.deepEqual(a->IntModule.lnot, b, ())
    t->Assert.deepEqual(b->IntModule.lnot, a, ())
  }

  assertNot1(value, valueNot)
  assertNot1(minValue, maxValue)
  assertNot1(minValue + 1, maxValue - 1)

  if isSigned {
    assertNot1(0, -1)
  }

  let assertNot2 = i => {
    let n = fromIntExn(i)
    t->Assert.deepEqual(n->IntModule.lnot->IntModule.lnot, n, ())
  }

  testFn(assertNot2)

  if isSigned {
    assertNot2(-1)
    assertNot2(-maxValue)
  }

  let assertAnd = (a, b, value) => {
    let a = fromIntExn(a)
    let b = fromIntExn(b)
    let value = fromIntExn(value)
    t->Assert.deepEqual(a->IntModule.land(a), a, ())
    t->Assert.deepEqual(b->IntModule.land(b), b, ())
    t->Assert.deepEqual(value->IntModule.land(value), value, ())
    t->Assert.deepEqual(a->IntModule.land(a->IntModule.lnot), zero, ())
    t->Assert.deepEqual(b->IntModule.land(b->IntModule.lnot), zero, ())
    t->Assert.deepEqual(value->IntModule.land(value->IntModule.lnot), zero, ())
    t->Assert.deepEqual(a->IntModule.land(b), value, ())
    t->Assert.deepEqual(b->IntModule.land(a), value, ())
  }

  assertAnd(value, valueNot, 0)
  assertAnd(minValue, maxValue, 0)
  assertAnd(minValue + 1, maxValue - 1, 0)
  assertAnd(less, greater, lessGreaterAnd)
  loop100Fn(() => {
    let value = randomValue()
    assertAnd(value, value, value)
  })

  if isSigned {
    assertAnd(0, -1, 0)
  }

  let assertOr = (a, b, value) => {
    let a = fromIntExn(a)
    let b = fromIntExn(b)
    let value = fromIntExn(value)
    let oneBits = isSigned ? fromIntExn(-1) : IntModule.maxValue
    t->Assert.deepEqual(a->IntModule.lor(a), a, ())
    t->Assert.deepEqual(b->IntModule.lor(b), b, ())
    t->Assert.deepEqual(value->IntModule.lor(value), value, ())
    t->Assert.deepEqual(a->IntModule.lor(a->IntModule.lnot), oneBits, ())
    t->Assert.deepEqual(b->IntModule.lor(b->IntModule.lnot), oneBits, ())
    t->Assert.deepEqual(value->IntModule.lor(value->IntModule.lnot), oneBits, ())
    t->Assert.deepEqual(a->IntModule.lor(b), value, ())
    t->Assert.deepEqual(b->IntModule.lor(a), value, ())
  }

  assertOr(less, greater, lessGreaterOr)
  loop100Fn(() => {
    let value = randomValue()
    assertOr(value, value, value)
  })

  if isSigned {
    assertOr(value, valueNot, -1)
    assertOr(minValue, maxValue, -1)
    assertOr(minValue + 1, maxValue - 1, -1)
    assertOr(0, -1, -1)
  } else {
    assertOr(value, valueNot, maxValue)
    assertOr(minValue, maxValue, maxValue)
    assertOr(minValue + 1, maxValue - 1, maxValue)
  }

  let assertXor = (a, b, value) => {
    let a = fromIntExn(a)
    let b = fromIntExn(b)
    let value = fromIntExn(value)
    let oneBits = isSigned ? fromIntExn(-1) : IntModule.maxValue
    t->Assert.deepEqual(a->IntModule.lxor(a), zero, ())
    t->Assert.deepEqual(b->IntModule.lxor(b), zero, ())
    t->Assert.deepEqual(value->IntModule.lxor(value), zero, ())
    t->Assert.deepEqual(a->IntModule.lxor(a->IntModule.lnot), oneBits, ())
    t->Assert.deepEqual(b->IntModule.lxor(b->IntModule.lnot), oneBits, ())
    t->Assert.deepEqual(value->IntModule.lxor(value->IntModule.lnot), oneBits, ())
    t->Assert.deepEqual(a->IntModule.lxor(b), value, ())
    t->Assert.deepEqual(b->IntModule.lxor(a), value, ())
  }

  assertXor(less, greater, lessGreaterXor)
  loop100Fn(() => {
    let value = randomValue()
    assertXor(value, value, 0)
  })

  if isSigned {
    assertXor(value, valueNot, -1)
    assertXor(minValue, maxValue, -1)
    assertXor(minValue + 1, maxValue - 1, -1)
    assertXor(0, -1, -1)
  } else {
    assertXor(value, valueNot, maxValue)
    assertXor(minValue, maxValue, maxValue)
    assertXor(minValue + 1, maxValue - 1, maxValue)
  }

  @warning("-27")
  let modBits = (i: int): int => %raw(`(i >>> 0) % bits`)

  {
    let value = fromIntExn(value)

    for i in -bits to 2 * bits {
      t->Assert.deepEqual(
        value->IntModule.lsl(i),
        fromIntExn(valueShiftLeft->Array.getExn(i->modBits)),
        (),
      )
    }
  }

  let mask = -1->lsr(32 - bits)

  let testUnsignedShiftRight = value => {
    let n = fromIntExn(value)
    for i in -bits to 2 * bits {
      if !is32Bits {
        let i_ = i->modBits
        t->Assert.deepEqual(
          n->IntModule.lsr(i),
          fromFloatExn(
            ((value < 0 && i_ !== 0 ? value->land(mask) : value)->Int.toFloat /.
              2.0 ** i_->Int.toFloat)->Js.Math.floor_float,
          ),
          (),
        )
      } else {
        t->Assert.deepEqual(n->IntModule.lsr(i), fromIntExn(value->lsr(i)), ())
      }
    }
  }

  testUnsignedShiftRight(value)
  testUnsignedShiftRight(valueNot)
  loop100Fn(() => testUnsignedShiftRight(randomValue()))

  let testShiftRight = value => {
    let n = fromIntExn(value)
    for i in -bits to 2 * bits {
      t->Assert.deepEqual(
        n->IntModule.asr(i),
        fromFloatExn((value->Int.toFloat /. 2.0 ** i->modBits->Int.toFloat)->Js.Math.floor_float),
        (),
      )
      if isSigned {
        t->Assert.deepEqual(
          n->IntModule.asr(i)->IntModule.lnot,
          n->IntModule.lnot->IntModule.asr(i),
          (),
        )
      }
    }
  }

  testShiftRight(value)
  testShiftRight(valueNot)
  loop100Fn(() => testShiftRight(randomValue()))

  {
    let value = fromIntExn(value)

    for i in -bits to 2 * bits {
      t->Assert.deepEqual(
        value->IntModule.rsl(i),
        fromIntExn(valueRotateShiftLeft->Array.getExn(i->modBits)),
        (),
      )
    }
  }

  let testRotateShiftLeft = value => {
    let n = fromIntExn(value)
    for i in -bits to 2 * bits {
      t->Assert.deepEqual(
        n->IntModule.rsl(i)->IntModule.lnot,
        n->IntModule.lnot->IntModule.rsl(i),
        (),
      )
    }
  }

  testRotateShiftLeft(value)
  testRotateShiftLeft(valueNot)
  loop100Fn(() => testRotateShiftLeft(randomValue()))

  let testRotateShiftRight = value => {
    let n = fromIntExn(value)
    for i in -bits to 2 * bits {
      t->Assert.deepEqual(n->IntModule.rsl(i), n->IntModule.rsr(bits - i), ())
      t->Assert.deepEqual(
        n->IntModule.rsr(i)->IntModule.lnot,
        n->IntModule.lnot->IntModule.rsr(i),
        (),
      )
    }
  }

  testRotateShiftRight(value)
  testRotateShiftRight(valueNot)
  loop100Fn(() => testRotateShiftRight(randomValue()))
})

test(`test ${name} integer math`, t => {
  let testClz = (n, result) => {
    t->Assert.deepEqual(n->IntModule.clz, result, ())
  }

  testClz(zero, bits)

  if isSigned {
    testClz(fromIntExn(-1), 0)
    testClz(IntModule.minValue, 0)
    testClz(IntModule.maxValue, 1)
  } else {
    testClz(IntModule.minValue, bits)
    testClz(IntModule.maxValue, 0)
  }

  for i in 0 to bits - 1 {
    testClz(one->IntModule.lsl(i), bits - i - 1)
  }

  t->Assert.deepEqual(
    fromIntExn(less)->IntModule.imul(fromIntExn(greater)),
    fromIntExn(less * greater),
    (),
  )

  let mask = -1->lsr(32 - bits)
  let j = 2.0 ** bits->Int.toFloat
  let k = 2.0 ** (bits - 1)->Int.toFloat
  let toU32 = i => i->land(mask)->intToUint32

  let testImul = (a, b) => {
    let m = (a->toU32 *. b->toU32)->mod_float(j)
    let result = isSigned && m >= k ? m -. j : m

    t->Assert.deepEqual(fromIntExn(a)->IntModule.imul(fromIntExn(b)), fromFloatExn(result), ())
    t->Assert.deepEqual(fromIntExn(b)->IntModule.imul(fromIntExn(a)), fromFloatExn(result), ())
  }

  if is32Bits {
    let n = 1000000.0
    loop100Fn(() => testImul(randomInt(0.0, n), randomInt(minValue->Int.toFloat, n)))

    loop100Fn(() => {
      let a = randomValue()
      let b = randomValue()
      t->Assert.deepEqual(
        fromIntExn(a)->IntModule.imul(fromIntExn(b)),
        fromIntExn(a->Js.Math.imul(b)),
        (),
      )
      t->Assert.deepEqual(
        fromIntExn(a)->IntModule.imul(fromIntExn(b)),
        fromIntExn(b)->IntModule.imul(fromIntExn(a)),
        (),
      )
    })
  } else {
    loop100Fn(() => testImul(randomValue(), randomValue()))
  }

  let testPowInRange = (a, b) => {
    let result = fromFloatExn(a->Int.toFloat ** b->Int.toFloat)
    let a = fromIntExn(a)
    let b = fromIntExn(b)

    t->Assert.deepEqual(IntModule.pow(~base=a, ~exp=b), Some(result), ())
    t->Assert.deepEqual(IntModule.powExn(~base=a, ~exp=b), result, ())
    t->Assert.deepEqual(IntModule.powUnsafe(~base=a, ~exp=b), result, ())

    t->Assert.deepEqual(IntModule.pow(~base=a, ~exp=zero), Some(one), ())
    t->Assert.deepEqual(IntModule.powExn(~base=a, ~exp=zero), one, ())
    t->Assert.deepEqual(IntModule.powUnsafe(~base=a, ~exp=zero), one, ())
    t->Assert.deepEqual(IntModule.pow(~base=b, ~exp=zero), Some(one), ())
    t->Assert.deepEqual(IntModule.powExn(~base=b, ~exp=zero), one, ())
    t->Assert.deepEqual(IntModule.powUnsafe(~base=b, ~exp=zero), one, ())

    t->Assert.deepEqual(IntModule.pow(~base=a, ~exp=one), Some(a), ())
    t->Assert.deepEqual(IntModule.powExn(~base=a, ~exp=one), a, ())
    t->Assert.deepEqual(IntModule.powUnsafe(~base=a, ~exp=one), a, ())
    t->Assert.deepEqual(IntModule.pow(~base=b, ~exp=one), Some(b), ())
    t->Assert.deepEqual(IntModule.powExn(~base=b, ~exp=one), b, ())
    t->Assert.deepEqual(IntModule.powUnsafe(~base=b, ~exp=one), b, ())
  }

  testPowInRange(0, 1)
  testPowInRange(1, 0)
  testPowInRange(value, 0)
  testPowInRange(minValue, 1)
  testPowInRange(maxValue, 1)

  let testPowOutOfRange = (a, b) => {
    let a = fromIntExn(a)
    let b = fromIntExn(b)

    t->Assert.deepEqual(IntModule.pow(~base=a, ~exp=b), None, ())
    t->assertOverflow(() => IntModule.powExn(~base=a, ~exp=b))
  }

  testPowOutOfRange(maxValue, 2)

  if isSigned {
    testPowOutOfRange(minValue, 2)
  } else {
    testPowInRange(minValue, value)
  }

  loop100Fn(() => {
    let a = randomValue()
    let b = randomPositiveSmallInt()
    let result = a->Int.toFloat ** b->Int.toFloat

    if result >= minValue->Int.toFloat && result <= maxValue->Int.toFloat {
      testPowInRange(a, b)
    } else {
      testPowOutOfRange(a, b)
    }
  })

  if isSigned {
    let testPowNotInteger = (a, b) => {
      let a = fromIntExn(a)
      let b = fromIntExn(b)

      t->Assert.deepEqual(IntModule.pow(~base=a, ~exp=b), None, ())
      t->assertInvalidArgument(() => IntModule.powExn(~base=a, ~exp=b))
    }

    testPowNotInteger(value, -1)
    testPowNotInteger(minValue, -1)
    testPowNotInteger(maxValue, -1)
  }

  loop100Fn(() => {
    let m = randomValue()
    let n = randomValue()
    let min = fromIntExn(m < n ? m : n)
    let max = fromIntExn(m > n ? m : n)

    if min !== max {
      loopFn(
        () => {
          let i = IntModule.random(min, max)
          t->Assert.isTrue(i->IntModule.ge(min), ())
          t->Assert.isTrue(i->IntModule.lt(max), ())
        },
        10,
      )
    }
  })
})
