open Ava
open Belt

open TestUtils

module IntModule = Number.Uint8

let minSafeInteger = ResNumber__Utils.minSafeInteger

let maxSafeInteger = ResNumber__Utils.maxSafeInteger

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

let min = 7

let max = 27

let minMaxAnd = 3

let minMaxOr = 31

let minMaxXor = 28

let randomValue = () => randomFloat(minValue->Int.toFloat, maxValue->Int.toFloat)->Js.Math.floor_int

let randomPositiveSmallValue = () => randomFloat(0.0, 16.0)->Js.Math.floor_int

let randomLargerInt = () =>
  randomFloat(maxValue->Int.toFloat +. 1.0, Js.Int.max->Int.toFloat)->Js.Math.floor_int

let randomSmallerInt = () =>
  randomFloat(Js.Int.min->Int.toFloat, minValue->Int.toFloat)->Js.Math.floor_int

let randomLargerFloat = () =>
  randomFloat(maxValue->Int.toFloat +. 1.0, maxSafeInteger)->Js.Math.floor_float

let randomSmallerFloat = () =>
  randomFloat(minSafeInteger, minValue->Int.toFloat -. 1.0)->Js.Math.floor_float

test(`test ${name} sign`, t => {
  t->Assert.deepEqual(IntModule.isSigned, isSigned, ())
  t->Assert.deepEqual(IntModule.isUnsigned, !isSigned, ())
})

test(`test ${name} bits`, t => t->Assert.deepEqual(IntModule.bits, bits, ()))

test(`test ${name} from int`, t => {
  let testInRange = i => {
    let n = IntModule.fromIntExn(i)
    t->Assert.deepEqual(IntModule.fromInt(i), Some(n), ())
    t->Assert.deepEqual(IntModule.fromIntExn(i), n, ())
    t->Assert.deepEqual(IntModule.fromIntClamped(i), n, ())
    t->Assert.deepEqual(IntModule.fromIntUnsafe(i), n, ())
  }

  testInRange(0)
  testInRange(1)
  testInRange(value)
  testInRange(minValue)
  testInRange(maxValue)
  testInRange(minValue + 1)
  testInRange(maxValue - 1)
  loop100Fn(() => testInRange(randomValue()))

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
      t->assertOverflow(() => IntModule.fromIntExn(i))
      t->Assert.deepEqual(IntModule.fromIntClamped(i), IntModule.fromIntExn(value), ())
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
    let n = IntModule.fromIntExn(i)
    t->Assert.deepEqual(n->IntModule.toInt, Some(i), ())
    t->Assert.deepEqual(n->IntModule.toIntExn, i, ())
    t->Assert.deepEqual(n->IntModule.toIntClamped, i, ())
    t->Assert.deepEqual(n->IntModule.toIntUnsafe, i, ())
  }

  testToInt(0)
  testToInt(1)
  testToInt(value)
  testToInt(minValue)
  testToInt(maxValue)
  testToInt(minValue + 1)
  testToInt(maxValue - 1)
  loop100Fn(() => testToInt(randomValue()))

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
    let n = IntModule.fromIntExn(i)
    t->Assert.deepEqual(IntModule.fromFloat(f), Some(n), ())
    t->Assert.deepEqual(IntModule.fromFloatExn(f), n, ())
    t->Assert.deepEqual(IntModule.fromFloatClamped(f), n, ())
    t->Assert.deepEqual(IntModule.fromFloatUnsafe(f), n, ())
  }

  testIsIntegerAndInRange(0)
  testIsIntegerAndInRange(1)
  testIsIntegerAndInRange(value)
  testIsIntegerAndInRange(minValue)
  testIsIntegerAndInRange(maxValue)
  testIsIntegerAndInRange(minValue + 1)
  testIsIntegerAndInRange(maxValue - 1)
  loop100Fn(() => testIsIntegerAndInRange(randomValue()))

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
    t->assertOverflow(() => IntModule.fromFloatExn(f))
    t->Assert.deepEqual(IntModule.fromFloatClamped(f), IntModule.fromIntExn(value), ())
  }

  testOutOfRange(minValue->Int.toFloat -. 1.0, minValue)
  testOutOfRange(maxValue->Int.toFloat +. 1.0, maxValue)
  testOutOfRange(minValue->Int.toFloat -. value->Int.toFloat, minValue)
  testOutOfRange(maxValue->Int.toFloat +. value->Int.toFloat, maxValue)
  loop100Fn(() => testOutOfRange(randomLargerFloat(), maxValue))
  loop100Fn(() => testOutOfRange(randomSmallerFloat(), minValue))

  let testIsNotInteger = (f, value) => {
    t->Assert.deepEqual(IntModule.fromFloat(f), None, ())
    t->assertInvalidArgument(() => IntModule.fromFloatExn(f))
    t->Assert.deepEqual(IntModule.fromFloatClamped(f), IntModule.fromIntExn(value), ())
  }

  let f = value->Int.toFloat +. 0.5
  testIsNotInteger(f, f->Float.toInt)

  if isSigned {
    let f = -.(value->Int.toFloat) -. 0.5
    testIsNotInteger(f, f->Float.toInt)
    testIsNotInteger(minValue->Int.toFloat +. 0.1, minValue + 1)
  } else {
    testIsNotInteger(minValue->Int.toFloat +. 0.1, minValue)
  }

  testIsNotInteger(minValue->Int.toFloat -. 0.1, minValue)
  testIsNotInteger(maxValue->Int.toFloat +. 0.1, maxValue)
  testIsNotInteger(maxValue->Int.toFloat -. 0.1, maxValue - 1)
  testIsNotInteger(minValue->Int.toFloat -. value->Int.toFloat -. 0.5, minValue)
  testIsNotInteger(maxValue->Int.toFloat +. value->Int.toFloat +. 0.5, maxValue)
})

test(`test ${name} to float`, t => {
  let testToFloat = i => {
    let f = i->Int.toFloat
    t->Assert.deepEqual(IntModule.fromFloatExn(f)->IntModule.toFloat, f, ())
  }

  testToFloat(value)
  loop100Fn(() => testToFloat(randomValue()))
})

test(`test ${name} from string`, t => {
  let testInRange = (i, s) => {
    let n = IntModule.fromIntExn(i)
    t->Assert.deepEqual(IntModule.fromString(s), Some(n), ())
    t->Assert.deepEqual(IntModule.fromStringExn(s), n, ())
  }

  testInRange(0, "0")
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
  }

  testOutOfRange(minValue->Int.toFloat -. 1.0)
  testOutOfRange(maxValue->Int.toFloat +. 1.0)
  testOutOfRange(minValue->Int.toFloat -. value->Int.toFloat)
  testOutOfRange(maxValue->Int.toFloat +. value->Int.toFloat)

  if !is32Bits {
    loop100Fn(() => testOutOfRange(randomLargerInt()->Int.toFloat))
    loop100Fn(() => testOutOfRange(randomSmallerInt()->Int.toFloat))
  }

  let s = "foo"
  t->Assert.deepEqual(IntModule.fromString(s), None, ())
  t->assertInvalidArgument(() => IntModule.fromStringExn(s))
})

test(`test ${name} to string`, t => {
  let testToString = i => {
    let s = i->Int.toString
    t->Assert.deepEqual(IntModule.fromStringExn(s)->IntModule.toString, s, ())
  }

  testToString(value)
  loop100Fn(() => testToString(randomValue()))
})

test(`test ${name} zero`, t => t->Assert.deepEqual(IntModule.zero, IntModule.fromIntExn(0), ()))

test(`test ${name} one`, t => t->Assert.deepEqual(IntModule.one, IntModule.fromIntExn(1), ()))

test(`test ${name} range`, t => {
  t->Assert.deepEqual(IntModule.minValue, IntModule.fromIntExn(minValue), ())
  t->Assert.deepEqual(IntModule.maxValue, IntModule.fromIntExn(maxValue), ())
})

test(`test ${name} comparison`, t => {
  let testCompare = (min, max) => {
    let min = IntModule.fromIntExn(min)
    let max = IntModule.fromIntExn(max)
    let value = IntModule.fromIntExn(value)
    let minValue = IntModule.fromIntExn(minValue)
    let maxValue = IntModule.fromIntExn(maxValue)

    t->Assert.deepEqual(IntModule.compare(min, max), Some(Less), ())
    t->Assert.deepEqual(IntModule.compare(max, min), Some(Greater), ())
    t->Assert.deepEqual(IntModule.compare(min, min), Some(Equal), ())
    t->Assert.deepEqual(IntModule.compare(max, max), Some(Equal), ())

    t->Assert.deepEqual(IntModule.compareExn(min, max), Less, ())
    t->Assert.deepEqual(IntModule.compareExn(max, min), Greater, ())
    t->Assert.deepEqual(IntModule.compareExn(min, min), Equal, ())
    t->Assert.deepEqual(IntModule.compareExn(max, max), Equal, ())

    t->Assert.isTrue(min->IntModule.eq(min), ())
    t->Assert.isTrue(max->IntModule.eq(max), ())
    t->Assert.isFalse(min->IntModule.eq(max), ())
    t->Assert.isFalse(max->IntModule.eq(min), ())

    t->Assert.isTrue(min->IntModule.ne(max), ())
    t->Assert.isTrue(max->IntModule.ne(min), ())
    t->Assert.isFalse(min->IntModule.ne(min), ())
    t->Assert.isFalse(max->IntModule.ne(max), ())

    t->Assert.isTrue(min->IntModule.lt(max), ())
    t->Assert.isFalse(max->IntModule.lt(min), ())
    t->Assert.isFalse(min->IntModule.lt(min), ())
    t->Assert.isFalse(max->IntModule.lt(max), ())

    t->Assert.isTrue(min->IntModule.le(max), ())
    t->Assert.isFalse(max->IntModule.le(min), ())
    t->Assert.isTrue(min->IntModule.le(min), ())
    t->Assert.isTrue(max->IntModule.le(max), ())

    t->Assert.isTrue(max->IntModule.gt(min), ())
    t->Assert.isFalse(min->IntModule.gt(max), ())
    t->Assert.isFalse(min->IntModule.gt(min), ())
    t->Assert.isFalse(max->IntModule.gt(max), ())

    t->Assert.isTrue(max->IntModule.ge(min), ())
    t->Assert.isFalse(min->IntModule.ge(max), ())
    t->Assert.isTrue(min->IntModule.ge(min), ())
    t->Assert.isTrue(max->IntModule.ge(max), ())

    t->Assert.deepEqual(IntModule.min(min, max), min, ())
    t->Assert.deepEqual(IntModule.min(max, min), min, ())
    t->Assert.deepEqual(IntModule.min(min, min), min, ())
    t->Assert.deepEqual(IntModule.min(max, max), max, ())

    t->Assert.deepEqual(IntModule.minMany([min]), Some(min), ())
    t->Assert.deepEqual(IntModule.minMany([min, max]), Some(min), ())
    t->Assert.deepEqual(IntModule.minMany([max, min]), Some(min), ())
    t->Assert.deepEqual(
      IntModule.minMany([min, value, max, minValue, maxValue]),
      Some(minValue),
      (),
    )
    t->Assert.deepEqual(IntModule.minManyExn([min]), min, ())
    t->Assert.deepEqual(IntModule.minManyExn([min, max]), min, ())
    t->Assert.deepEqual(IntModule.minManyExn([max, min]), min, ())
    t->Assert.deepEqual(IntModule.minManyExn([minValue, maxValue, min, value, max]), minValue, ())
    t->Assert.deepEqual(IntModule.minManyUnsafe([min]), min, ())
    t->Assert.deepEqual(IntModule.minManyUnsafe([min, max]), min, ())
    t->Assert.deepEqual(IntModule.minManyUnsafe([max, min]), min, ())
    t->Assert.deepEqual(
      IntModule.minManyUnsafe([min, minValue, maxValue, value, max]),
      minValue,
      (),
    )

    t->Assert.deepEqual(IntModule.max(min, max), max, ())
    t->Assert.deepEqual(IntModule.max(max, min), max, ())
    t->Assert.deepEqual(IntModule.max(min, min), min, ())
    t->Assert.deepEqual(IntModule.max(max, max), max, ())

    t->Assert.deepEqual(IntModule.maxMany([min]), Some(min), ())
    t->Assert.deepEqual(IntModule.maxMany([min, max]), Some(max), ())
    t->Assert.deepEqual(IntModule.maxMany([max, min]), Some(max), ())
    t->Assert.deepEqual(
      IntModule.maxMany([min, value, max, minValue, maxValue]),
      Some(maxValue),
      (),
    )
    t->Assert.deepEqual(IntModule.maxManyExn([min]), min, ())
    t->Assert.deepEqual(IntModule.maxManyExn([min, max]), max, ())
    t->Assert.deepEqual(IntModule.maxManyExn([max, min]), max, ())
    t->Assert.deepEqual(IntModule.maxManyExn([minValue, maxValue, min, value, max]), maxValue, ())
    t->Assert.deepEqual(IntModule.maxManyUnsafe([min]), min, ())
    t->Assert.deepEqual(IntModule.maxManyUnsafe([min, max]), max, ())
    t->Assert.deepEqual(IntModule.maxManyUnsafe([max, min]), max, ())
    t->Assert.deepEqual(
      IntModule.maxManyUnsafe([min, minValue, maxValue, value, max]),
      maxValue,
      (),
    )
  }

  testCompare(min, max)

  loop100Fn(() => {
    let min = randomValue()
    let max = Js.Math.random_int(min + 1, maxValue + 1)
    testCompare(min, max)
  })

  t->Assert.deepEqual(IntModule.minMany([]), None, ())
  t->assertInvalidArgument(() => IntModule.minManyExn([]))
  t->Assert.deepEqual(IntModule.maxMany([]), None, ())
  t->assertInvalidArgument(() => IntModule.maxManyExn([]))
})

test(`test ${name} addition`, t => {
  let testInRange = (a, b) => {
    let result = IntModule.fromIntExn(a + b)
    let a = IntModule.fromIntExn(a)
    let b = IntModule.fromIntExn(b)
    t->Assert.deepEqual(a->IntModule.add(b), Some(result), ())
    t->Assert.deepEqual(a->IntModule.add(b), b->IntModule.add(a), ())
    t->Assert.deepEqual(a->IntModule.addExn(b), result, ())
    t->Assert.deepEqual(a->IntModule.addExn(b), b->IntModule.addExn(a), ())
    t->Assert.deepEqual(a->IntModule.addClamped(b), result, ())
    t->Assert.deepEqual(a->IntModule.addClamped(b), b->IntModule.addClamped(a), ())
    t->Assert.deepEqual(a->IntModule.addUnsafe(b), result, ())
    t->Assert.deepEqual(a->IntModule.addUnsafe(b), b->IntModule.addUnsafe(a), ())
  }

  testInRange(min, max)
  testInRange(minValue, 0)
  testInRange(maxValue, 0)
  testInRange(minValue, 1)

  if isSigned {
    testInRange(maxValue, -1)
  }

  let testOutOfRange = (a, b, value) => {
    let a = IntModule.fromIntExn(a)
    let b = IntModule.fromIntExn(b)
    let value = IntModule.fromIntExn(value)
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
    let result = a + b
    if result < minValue {
      testOutOfRange(a, b, minValue)
    } else if result > maxValue {
      testOutOfRange(a, b, maxValue)
    } else {
      testInRange(a, b)
    }
    testInRange(a, 0)
    testInRange(b, 0)
    testInRange(a, 1)
    testInRange(b, 1)
  })
})

test(`test ${name} subtraction`, t => {
  let testInRange = (a, b) => {
    let result = IntModule.fromIntExn(a - b)
    let a = IntModule.fromIntExn(a)
    let b = IntModule.fromIntExn(b)
    t->Assert.deepEqual(a->IntModule.sub(b), Some(result), ())
    t->Assert.deepEqual(a->IntModule.subExn(b), result, ())
    t->Assert.deepEqual(a->IntModule.subClamped(b), result, ())
    t->Assert.deepEqual(a->IntModule.subUnsafe(b), result, ())

    t->Assert.deepEqual(a->IntModule.sub(a), Some(IntModule.zero), ())
    t->Assert.deepEqual(a->IntModule.subExn(a), IntModule.zero, ())
    t->Assert.deepEqual(a->IntModule.subClamped(a), IntModule.zero, ())
    t->Assert.deepEqual(a->IntModule.subUnsafe(a), IntModule.zero, ())

    t->Assert.deepEqual(b->IntModule.sub(b), Some(IntModule.zero), ())
    t->Assert.deepEqual(b->IntModule.subExn(b), IntModule.zero, ())
    t->Assert.deepEqual(b->IntModule.subClamped(b), IntModule.zero, ())
    t->Assert.deepEqual(b->IntModule.subUnsafe(b), IntModule.zero, ())
  }

  testInRange(max, min)
  testInRange(minValue, 0)
  testInRange(maxValue, 0)
  testInRange(maxValue, 1)

  if isSigned {
    testInRange(min, max)
    testInRange(minValue, -1)
    testInRange(0, maxValue)
  }

  let testOutOfRange = (a, b, value) => {
    let a = IntModule.fromIntExn(a)
    let b = IntModule.fromIntExn(b)
    t->Assert.deepEqual(a->IntModule.sub(b), None, ())
    t->assertOverflow(() => a->IntModule.subExn(b))
    t->Assert.deepEqual(a->IntModule.subClamped(b), IntModule.fromIntExn(value), ())
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

    let result = a - b
    if result < minValue {
      testOutOfRange(a, b, minValue)
    } else if result > maxValue {
      testOutOfRange(a, b, maxValue)
    } else {
      testInRange(a, b)
    }

    let result = b - a
    if result < minValue {
      testOutOfRange(b, a, minValue)
    } else if result > maxValue {
      testOutOfRange(b, a, maxValue)
    } else {
      testInRange(b, a)
    }
  })
})

test(`test ${name} multiplication`, t => {
  let testInRange = (a, b) => {
    let result = IntModule.fromIntExn(a * b)
    let a = IntModule.fromIntExn(a)
    let b = IntModule.fromIntExn(b)
    t->Assert.deepEqual(a->IntModule.mul(b), Some(result), ())
    t->Assert.deepEqual(a->IntModule.mul(b), b->IntModule.mul(a), ())
    t->Assert.deepEqual(a->IntModule.mulExn(b), result, ())
    t->Assert.deepEqual(a->IntModule.mulExn(b), b->IntModule.mulExn(a), ())
    t->Assert.deepEqual(a->IntModule.mulClamped(b), result, ())
    t->Assert.deepEqual(a->IntModule.mulClamped(b), b->IntModule.mulClamped(a), ())
    t->Assert.deepEqual(a->IntModule.mulUnsafe(b), result, ())
    t->Assert.deepEqual(a->IntModule.mulUnsafe(b), b->IntModule.mulUnsafe(a), ())
  }

  testInRange(min, max)
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
    let a = IntModule.fromIntExn(a)
    let b = IntModule.fromIntExn(b)
    let value = IntModule.fromIntExn(value)
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
    let result = a * b
    if result < minValue {
      testOutOfRange(a, b, minValue)
    } else if result > maxValue {
      testOutOfRange(a, b, maxValue)
    } else {
      testInRange(a, b)
    }
    testInRange(a, 0)
    testInRange(b, 0)
    testInRange(a, 1)
    testInRange(b, 1)
  })
})

test(`test ${name} division`, t => {
  let testNotDividedByZero = (a, b) => {
    if a !== 0 {
      let result = IntModule.fromIntExn(b / a)
      let a = IntModule.fromIntExn(a)
      let b = IntModule.fromIntExn(b)
      t->Assert.deepEqual(b->IntModule.div(a), Some(result), ())
      t->Assert.deepEqual(b->IntModule.divExn(a), result, ())
      t->Assert.deepEqual(b->IntModule.divUnsafe(a), result, ())
    }

    if b !== 0 {
      let result = IntModule.fromIntExn(a / b)
      let a = IntModule.fromIntExn(a)
      let b = IntModule.fromIntExn(b)
      t->Assert.deepEqual(a->IntModule.div(b), Some(result), ())
      t->Assert.deepEqual(a->IntModule.divExn(b), result, ())
      t->Assert.deepEqual(a->IntModule.divUnsafe(b), result, ())
    }
  }

  testNotDividedByZero(min, max)
  testNotDividedByZero(minValue, min)
  testNotDividedByZero(minValue, max)
  testNotDividedByZero(maxValue, min)
  testNotDividedByZero(maxValue, max)
  testNotDividedByZero(min, 1)
  testNotDividedByZero(max, 1)
  testNotDividedByZero(minValue, 1)
  testNotDividedByZero(maxValue, 1)
  testNotDividedByZero(minValue + 1, 1)
  testNotDividedByZero(maxValue - 1, 1)
  testNotDividedByZero(min, 2)
  testNotDividedByZero(max, 2)
  testNotDividedByZero(minValue, 2)
  testNotDividedByZero(maxValue, 2)
  testNotDividedByZero(minValue + 1, 2)
  testNotDividedByZero(maxValue - 1, 2)
  testNotDividedByZero(0, min)
  testNotDividedByZero(0, max)
  testNotDividedByZero(0, minValue)
  testNotDividedByZero(0, maxValue)
  testNotDividedByZero(0, minValue + 1)
  testNotDividedByZero(0, maxValue - 1)

  if isSigned {
    testNotDividedByZero(min, -max)
    testNotDividedByZero(max, -min)
    testNotDividedByZero(-min, -max)
    testNotDividedByZero(minValue, -min)
    testNotDividedByZero(minValue, -max)
    testNotDividedByZero(maxValue, -min)
    testNotDividedByZero(maxValue, -max)
    testNotDividedByZero(-maxValue, -min)
    testNotDividedByZero(-maxValue, -max)
    testNotDividedByZero(min, -1)
    testNotDividedByZero(max, -1)
    testNotDividedByZero(maxValue, -1)
    testNotDividedByZero(minValue + 1, -1)
    testNotDividedByZero(maxValue - 1, -1)
    testNotDividedByZero(min, -2)
    testNotDividedByZero(max, -2)
    testNotDividedByZero(minValue, -2)
    testNotDividedByZero(maxValue, -2)
    testNotDividedByZero(minValue + 1, -2)
    testNotDividedByZero(maxValue - 1, -2)
    testNotDividedByZero(0, -min)
    testNotDividedByZero(0, -max)
    testNotDividedByZero(0, -maxValue)
  }

  let testDividedByZero = a => {
    let a = IntModule.fromIntExn(a)
    t->Assert.deepEqual(a->IntModule.div(IntModule.zero), None, ())
    t->assertDivisionByZero(() => a->IntModule.divExn(IntModule.zero))
  }

  testDividedByZero(0)
  testDividedByZero(1)
  testDividedByZero(min)
  testDividedByZero(max)
  testDividedByZero(value)
  testDividedByZero(minValue)
  testDividedByZero(maxValue)
  testDividedByZero(minValue + 1)
  testDividedByZero(maxValue - 1)

  loop100Fn(() => {
    let a = randomValue()
    let b = randomValue()
    testNotDividedByZero(a, b)
    testDividedByZero(a)
    testDividedByZero(b)
  })
})

test(`test ${name} remainder`, t => {
  let testNotDividedByZero = (a, b) => {
    if a !== 0 {
      let result = IntModule.fromIntExn(b->mod(a)->lor(0))
      let a = IntModule.fromIntExn(a)
      let b = IntModule.fromIntExn(b)
      t->Assert.deepEqual(b->IntModule.rem(a), Some(result), ())
      t->Assert.deepEqual(b->IntModule.remExn(a), result, ())
      t->Assert.deepEqual(b->IntModule.remUnsafe(a), result, ())
    }
    if b !== 0 {
      let result = IntModule.fromIntExn(a->mod(b)->lor(0))
      let a = IntModule.fromIntExn(a)
      let b = IntModule.fromIntExn(b)
      t->Assert.deepEqual(a->IntModule.rem(b), Some(result), ())
      t->Assert.deepEqual(a->IntModule.remExn(b), result, ())
      t->Assert.deepEqual(a->IntModule.remUnsafe(b), result, ())
    }
  }

  testNotDividedByZero(min, max)
  testNotDividedByZero(minValue, min)
  testNotDividedByZero(minValue, max)
  testNotDividedByZero(maxValue, min)
  testNotDividedByZero(maxValue, max)
  testNotDividedByZero(min, 1)
  testNotDividedByZero(max, 1)
  testNotDividedByZero(minValue, 1)
  testNotDividedByZero(maxValue, 1)
  testNotDividedByZero(minValue + 1, 1)
  testNotDividedByZero(maxValue - 1, 1)
  testNotDividedByZero(min, 2)
  testNotDividedByZero(max, 2)
  testNotDividedByZero(minValue, 2)
  testNotDividedByZero(maxValue, 2)
  testNotDividedByZero(minValue + 1, 2)
  testNotDividedByZero(maxValue - 1, 2)
  testNotDividedByZero(0, min)
  testNotDividedByZero(0, max)
  testNotDividedByZero(0, minValue)
  testNotDividedByZero(0, maxValue)
  testNotDividedByZero(0, minValue + 1)
  testNotDividedByZero(0, maxValue - 1)

  if isSigned {
    testNotDividedByZero(min, -max)
    testNotDividedByZero(max, -min)
    testNotDividedByZero(-min, -max)
    testNotDividedByZero(minValue, -min)
    testNotDividedByZero(minValue, -max)
    testNotDividedByZero(maxValue, -min)
    testNotDividedByZero(maxValue, -max)
    testNotDividedByZero(-maxValue, -min)
    testNotDividedByZero(-maxValue, -max)
    testNotDividedByZero(min, -1)
    testNotDividedByZero(max, -1)
    testNotDividedByZero(maxValue, -1)
    testNotDividedByZero(minValue + 1, -1)
    testNotDividedByZero(maxValue - 1, -1)
    testNotDividedByZero(min, -2)
    testNotDividedByZero(max, -2)
    testNotDividedByZero(minValue, -2)
    testNotDividedByZero(maxValue, -2)
    testNotDividedByZero(minValue + 1, -2)
    testNotDividedByZero(maxValue - 1, -2)
    testNotDividedByZero(0, -min)
    testNotDividedByZero(0, -max)
    testNotDividedByZero(0, -maxValue)
  }

  let testDividedByZero = a => {
    let a = IntModule.fromIntExn(a)
    t->Assert.deepEqual(a->IntModule.rem(IntModule.zero), None, ())
    t->assertDivisionByZero(() => a->IntModule.remExn(IntModule.zero))
  }

  testDividedByZero(0)
  testDividedByZero(1)
  testDividedByZero(min)
  testDividedByZero(max)
  testDividedByZero(value)
  testDividedByZero(minValue)
  testDividedByZero(maxValue)
  testDividedByZero(minValue + 1)
  testDividedByZero(maxValue - 1)

  loop100Fn(() => {
    let a = randomValue()
    let b = randomValue()
    testNotDividedByZero(a, b)
    testDividedByZero(a)
    testDividedByZero(b)
  })
})

test(`test ${name} sum`, t => {
  let testInRange = arr => {
    let result = IntModule.fromIntExn(arr->Array.reduce(0, (acc, v) => acc + v))
    let arr = arr->Array.mapU(i => IntModule.fromIntExn(i))
    t->Assert.deepEqual(arr->IntModule.sum, Some(result), ())
    t->Assert.deepEqual(arr->IntModule.sumExn, result, ())
    t->Assert.deepEqual(arr->IntModule.sumUnsafe, result, ())
  }

  testInRange([min])
  testInRange([max])
  testInRange([minValue])
  testInRange([maxValue])
  testInRange([minValue + 1])
  testInRange([maxValue - 1])
  testInRange([min, max])
  testInRange([max, min])
  testInRange([min, value, max])
  testInRange([minValue, maxValue])

  t->Assert.deepEqual(IntModule.sum([]), None, ())
  t->assertInvalidArgument(() => IntModule.sumExn([]))

  let testOutOfRange = arr => {
    let arr = arr->Array.mapU(i => IntModule.fromIntExn(i))
    t->Assert.deepEqual(arr->IntModule.sum, None, ())
    t->assertOverflow(() => arr->IntModule.sumExn)
  }

  testOutOfRange([maxValue, 1])
  testOutOfRange([1, maxValue])
  testOutOfRange([maxValue, value])
  testOutOfRange([value, maxValue])
  testOutOfRange([maxValue, max - min, value])

  if isSigned {
    testOutOfRange([minValue, -1])
    testOutOfRange([-1, minValue])
    testOutOfRange([minValue, -value])
    testOutOfRange([-value, minValue])
    testOutOfRange([minValue, min - max, -value])
  }

  loop100Fn(() => {
    let a = randomValue()
    let b = randomValue()
    testInRange([a])
    testInRange([b])

    let result = a + b
    if result >= minValue && result <= maxValue {
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
    let result = IntModule.fromIntExn(i + 1)
    let n = IntModule.fromIntExn(i)
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
    let result = IntModule.fromIntExn(i - 1)
    let n = IntModule.fromIntExn(i)
    t->Assert.deepEqual(n->IntModule.dec, Some(result), ())
    t->Assert.deepEqual(n->IntModule.decExn, result, ())
    t->Assert.deepEqual(n->IntModule.decUnsafe, result, ())
  }

  testDecInRange(1)
  testDecInRange(value)
  testDecInRange(minValue + 1)
  testDecInRange(maxValue)
  testDecInRange(maxValue - 1)
  loop100Fn(() => testDecInRange(Js.Math.random_int(minValue + 1, maxValue)))

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
    let a = IntModule.fromIntExn(a)
    let b = IntModule.fromIntExn(b)
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
    let n = IntModule.fromIntExn(i)
    t->Assert.deepEqual(n->IntModule.lnot->IntModule.lnot, n, ())
  }

  assertNot2(0)
  assertNot2(1)
  assertNot2(value)
  assertNot2(minValue)
  assertNot2(maxValue)
  loop100Fn(() => assertNot2(randomValue()))

  if isSigned {
    assertNot2(-1)
    assertNot2(-maxValue)
  }

  let assertAnd = (a, b, value) => {
    let a = IntModule.fromIntExn(a)
    let b = IntModule.fromIntExn(b)
    let value = IntModule.fromIntExn(value)
    t->Assert.deepEqual(a->IntModule.land(a), a, ())
    t->Assert.deepEqual(b->IntModule.land(b), b, ())
    t->Assert.deepEqual(value->IntModule.land(value), value, ())
    t->Assert.deepEqual(a->IntModule.land(a->IntModule.lnot), IntModule.zero, ())
    t->Assert.deepEqual(b->IntModule.land(b->IntModule.lnot), IntModule.zero, ())
    t->Assert.deepEqual(value->IntModule.land(value->IntModule.lnot), IntModule.zero, ())
    t->Assert.deepEqual(a->IntModule.land(b), value, ())
    t->Assert.deepEqual(b->IntModule.land(a), value, ())
  }

  assertAnd(value, valueNot, 0)
  assertAnd(minValue, maxValue, 0)
  assertAnd(minValue + 1, maxValue - 1, 0)
  assertAnd(min, max, minMaxAnd)
  loop100Fn(() => {
    let value = randomValue()
    assertAnd(value, value, value)
  })

  if isSigned {
    assertAnd(0, -1, 0)
  }

  let assertOr = (a, b, value) => {
    let a = IntModule.fromIntExn(a)
    let b = IntModule.fromIntExn(b)
    let value = IntModule.fromIntExn(value)
    let oneBits = isSigned ? IntModule.fromIntExn(-1) : IntModule.maxValue
    t->Assert.deepEqual(a->IntModule.lor(a), a, ())
    t->Assert.deepEqual(b->IntModule.lor(b), b, ())
    t->Assert.deepEqual(value->IntModule.lor(value), value, ())
    t->Assert.deepEqual(a->IntModule.lor(a->IntModule.lnot), oneBits, ())
    t->Assert.deepEqual(b->IntModule.lor(b->IntModule.lnot), oneBits, ())
    t->Assert.deepEqual(value->IntModule.lor(value->IntModule.lnot), oneBits, ())
    t->Assert.deepEqual(a->IntModule.lor(b), value, ())
    t->Assert.deepEqual(b->IntModule.lor(a), value, ())
  }

  assertOr(min, max, minMaxOr)
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
    let a = IntModule.fromIntExn(a)
    let b = IntModule.fromIntExn(b)
    let value = IntModule.fromIntExn(value)
    let oneBits = isSigned ? IntModule.fromIntExn(-1) : IntModule.maxValue
    t->Assert.deepEqual(a->IntModule.lxor(a), IntModule.zero, ())
    t->Assert.deepEqual(b->IntModule.lxor(b), IntModule.zero, ())
    t->Assert.deepEqual(value->IntModule.lxor(value), IntModule.zero, ())
    t->Assert.deepEqual(a->IntModule.lxor(a->IntModule.lnot), oneBits, ())
    t->Assert.deepEqual(b->IntModule.lxor(b->IntModule.lnot), oneBits, ())
    t->Assert.deepEqual(value->IntModule.lxor(value->IntModule.lnot), oneBits, ())
    t->Assert.deepEqual(a->IntModule.lxor(b), value, ())
    t->Assert.deepEqual(b->IntModule.lxor(a), value, ())
  }

  assertXor(min, max, minMaxXor)
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
    let value = IntModule.fromIntExn(value)

    for i in -bits to 2 * bits {
      t->Assert.deepEqual(
        value->IntModule.lsl(i),
        IntModule.fromIntExn(valueShiftLeft->Array.getExn(i->modBits)),
        (),
      )
    }
  }

  let mask = -1->lsr(32 - bits)

  let testUnsignedShiftRight = value => {
    let n = IntModule.fromIntExn(value)
    for i in -bits to 2 * bits {
      if !is32Bits {
        let i_ = i->modBits
        t->Assert.deepEqual(
          n->IntModule.lsr(i),
          IntModule.fromFloatExn(
            ((value < 0 && i_ !== 0 ? value->land(mask) : value)->Int.toFloat /.
              2.0 ** i_->Int.toFloat)->Js.Math.floor_float,
          ),
          (),
        )
      } else {
        t->Assert.deepEqual(n->IntModule.lsr(i), IntModule.fromIntExn(value->lsr(i)), ())
      }
    }
  }

  testUnsignedShiftRight(value)
  testUnsignedShiftRight(valueNot)
  loop100Fn(() => testUnsignedShiftRight(randomValue()))

  let testShiftRight = value => {
    let n = IntModule.fromIntExn(value)
    for i in -bits to 2 * bits {
      t->Assert.deepEqual(
        n->IntModule.asr(i),
        IntModule.fromFloatExn(
          (value->Int.toFloat /. 2.0 ** i->modBits->Int.toFloat)->Js.Math.floor_float,
        ),
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
    let value = IntModule.fromIntExn(value)

    for i in -bits to 2 * bits {
      t->Assert.deepEqual(
        value->IntModule.rsl(i),
        IntModule.fromIntExn(valueRotateShiftLeft->Array.getExn(i->modBits)),
        (),
      )
    }
  }

  let testRotateShiftLeft = value => {
    let n = IntModule.fromIntExn(value)
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
    let n = IntModule.fromIntExn(value)
    for i in -bits to 2 * bits {
      t->Assert.deepEqual(n->IntModule.rsl(i), n->IntModule.rsr(8 - i), ())
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

  testClz(IntModule.zero, bits)

  if isSigned {
    testClz(IntModule.fromIntExn(-1), 0)
    testClz(IntModule.minValue, 0)
    testClz(IntModule.maxValue, 1)
  } else {
    testClz(IntModule.minValue, bits)
    testClz(IntModule.maxValue, 0)
  }

  for i in 0 to bits - 1 {
    testClz(IntModule.fromIntExn(1)->IntModule.lsl(i), bits - i - 1)
  }

  t->Assert.deepEqual(
    IntModule.fromIntExn(min)->IntModule.imul(IntModule.fromIntExn(max)),
    IntModule.fromIntExn(min * max),
    (),
  )

  if !is32Bits {
    let mask = -1->lsr(32 - bits)
    let j = 2.0 ** bits->Int.toFloat
    let k = 2.0 ** (bits - 1)->Int.toFloat
    let floatToIntUnsafe = ResNumber__Utils.floatToIntUnsafe
    let toMask = i => i->toUint32->floatToIntUnsafe->land(mask)->Int.toFloat

    loop100Fn(() => {
      let a = randomValue()
      let b = randomValue()
      let m = (a->toMask *. b->toMask)->mod_float(j)
      let result = m >= k ? m -. j : m

      t->Assert.deepEqual(
        IntModule.fromIntExn(a)->IntModule.imul(IntModule.fromIntExn(b)),
        IntModule.fromFloatExn(isSigned ? result : result->floatToIntUnsafe->toMask),
        (),
      )
    })
  }

  let testPowInRange = (a, b) => {
    let result = IntModule.fromFloatExn(a->Int.toFloat ** b->Int.toFloat)
    let a = IntModule.fromIntExn(a)
    let b = IntModule.fromIntExn(b)

    t->Assert.deepEqual(IntModule.pow(~base=a, ~exp=b), Some(result), ())
    t->Assert.deepEqual(IntModule.powExn(~base=a, ~exp=b), result, ())
    t->Assert.deepEqual(IntModule.powUnsafe(~base=a, ~exp=b), result, ())

    t->Assert.deepEqual(IntModule.pow(~base=a, ~exp=IntModule.zero), Some(IntModule.one), ())
    t->Assert.deepEqual(IntModule.powExn(~base=a, ~exp=IntModule.zero), IntModule.one, ())
    t->Assert.deepEqual(IntModule.powUnsafe(~base=a, ~exp=IntModule.zero), IntModule.one, ())
    t->Assert.deepEqual(IntModule.pow(~base=b, ~exp=IntModule.zero), Some(IntModule.one), ())
    t->Assert.deepEqual(IntModule.powExn(~base=b, ~exp=IntModule.zero), IntModule.one, ())
    t->Assert.deepEqual(IntModule.powUnsafe(~base=b, ~exp=IntModule.zero), IntModule.one, ())

    t->Assert.deepEqual(IntModule.pow(~base=a, ~exp=IntModule.one), Some(a), ())
    t->Assert.deepEqual(IntModule.powExn(~base=a, ~exp=IntModule.one), a, ())
    t->Assert.deepEqual(IntModule.powUnsafe(~base=a, ~exp=IntModule.one), a, ())
    t->Assert.deepEqual(IntModule.pow(~base=b, ~exp=IntModule.one), Some(b), ())
    t->Assert.deepEqual(IntModule.powExn(~base=b, ~exp=IntModule.one), b, ())
    t->Assert.deepEqual(IntModule.powUnsafe(~base=b, ~exp=IntModule.one), b, ())
  }

  testPowInRange(0, 1)
  testPowInRange(1, 0)
  testPowInRange(value, 0)
  testPowInRange(minValue, 1)
  testPowInRange(maxValue, 1)

  let testPowOutOfRange = (a, b) => {
    let a = IntModule.fromIntExn(a)
    let b = IntModule.fromIntExn(b)

    t->Assert.deepEqual(IntModule.pow(~base=a, ~exp=b), None, ())
    t->assertOverflow(() => IntModule.powExn(~base=a, ~exp=b))
  }

  testPowOutOfRange(maxValue, 2)
  testPowOutOfRange(2, maxValue)

  if isSigned {
    testPowOutOfRange(minValue, 2)
  }

  loop100Fn(() => {
    let a = randomValue()
    let b = randomPositiveSmallValue()
    let result = a->Int.toFloat ** b->Int.toFloat

    if result >= minValue->Int.toFloat && result <= maxValue->Int.toFloat {
      testPowInRange(a, b)
    } else {
      testPowOutOfRange(a, b)
    }
  })

  if isSigned {
    let testPowNotInteger = (a, b) => {
      let a = IntModule.fromIntExn(a)
      let b = IntModule.fromIntExn(b)

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
    let min = IntModule.fromIntExn(m < n ? m : n)
    let max = IntModule.fromIntExn(m > n ? m : n)

    if min !== max {
      loopFn(
        () => {
          let i = IntModule.random(min, max)
          t->Assert.isTrue(i->IntModule.ge(min) && i->IntModule.lt(max), ())
        },
        10,
      )
    }
  })
})
