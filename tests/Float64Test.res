open Ava
open Belt

open TestUtils

module FloatModule = Number.Float64

let name = "Float64"

let minValue = neg_infinity

let maxValue = infinity

let value = 12345678.9

let less = -56248.7954

let greater = 87934.1968

let minInt = Js.Int.min

let maxInt = Js.Int.max

let minIntFloat = minInt->Int.toFloat

let maxIntFloat = maxInt->Int.toFloat

let fromIntExn = FloatModule.fromIntExn

let fromFloatExn = FloatModule.fromFloatExn

let zero = FloatModule.zero

let negZero = fromFloatExn(-0.0)

let one = FloatModule.one

let negOne = fromIntExn(-1)

let randomValue = () => randomFloat(minIntFloat *. 2.0, maxIntFloat *. 2.0)

let randomInt32 = () => randomInt(minIntFloat, maxIntFloat)

let randomLargerInt = () => randomFloat(maxIntFloat, maxSafeInteger)->Js.Math.floor_float

let randomSmallerInt = () => randomFloat(minSafeInteger, minIntFloat)->Js.Math.floor_float

let randomFloatInInt32Range = () => randomFloat(minIntFloat, maxIntFloat)

let testFn = fn => {
  fn(0.0)
  fn(-0.0)
  fn(1.0)
  fn(-1.0)
  fn(value)
  fn(less)
  fn(greater)
  fn(minIntFloat)
  fn(maxIntFloat)
  fn(minValue)
  fn(maxValue)
  loop100Fn(() => fn(randomValue()))
}

let clampFloatToInt = f =>
  if f < minIntFloat {
    minInt
  } else if f > maxIntFloat {
    maxInt
  } else {
    f->Float.toInt
  }

test(`test ${name} from int`, t => {
  let testFromInt = i => {
    let n = fromIntExn(i)
    t->Assert.deepEqual(FloatModule.fromInt(i), Some(n), ())
    t->Assert.deepEqual(fromIntExn(i), n, ())
    t->Assert.deepEqual(FloatModule.fromIntClamped(i), n, ())
    t->Assert.deepEqual(FloatModule.fromIntUnsafe(i), n, ())
  }

  testFromInt(0)
  testFromInt(1)
  testFromInt(minInt)
  testFromInt(minInt + 1)
  testFromInt(maxInt)
  testFromInt(maxInt - 1)
  loop100Fn(() => testFromInt(randomInt32()))
})

test(`test ${name} to int`, t => {
  let testInRange = i => {
    let n = fromIntExn(i)
    t->Assert.deepEqual(n->FloatModule.toInt, Some(i), ())
    t->Assert.deepEqual(n->FloatModule.toIntExn, i, ())
    t->Assert.deepEqual(n->FloatModule.toIntClamped, i, ())
    t->Assert.deepEqual(n->FloatModule.toIntUnsafe, i, ())
  }

  testInRange(0)
  testInRange(1)
  testInRange(minInt)
  testInRange(minInt + 1)
  testInRange(maxInt)
  testInRange(maxInt - 1)
  loop100Fn(() => testInRange(randomInt32()))

  let testOutOfRange = (f, value) => {
    let n = fromFloatExn(f)
    t->Assert.deepEqual(n->FloatModule.toInt, None, ())
    t->assertOverflow(() => n->FloatModule.toIntExn)
    t->Assert.deepEqual(n->FloatModule.toIntClamped, value, ())
  }

  testOutOfRange(minIntFloat -. 1.0, minInt)
  testOutOfRange(maxIntFloat +. 1.0, maxInt)
  loop100Fn(() => testOutOfRange(randomSmallerInt(), minInt))
  loop100Fn(() => testOutOfRange(randomLargerInt(), maxInt))

  let testIsNotInteger = (f, value) => {
    let n = fromFloatExn(f)
    t->Assert.deepEqual(n->FloatModule.toInt, None, ())
    t->assertInvalidArgument(() => n->FloatModule.toIntExn)
    t->Assert.deepEqual(n->FloatModule.toIntClamped, value, ())
  }

  testIsNotInteger(value, value->Float.toInt)
  testIsNotInteger(0.1, 0)
  testIsNotInteger(1.5, 1)
  testIsNotInteger(-0.1, 0)
  testIsNotInteger(-1.5, -1)
  testIsNotInteger(minIntFloat -. 0.5, minInt)
  testIsNotInteger(maxIntFloat +. 0.5, maxInt)
  testIsNotInteger(minValue, minInt)
  testIsNotInteger(maxValue, maxInt)
  loop100Fn(() => {
    let f = randomFloatInInt32Range()
    if !(f->isInteger) {
      testIsNotInteger(f, f->Js.Math.unsafe_trunc)
    }
  })
})

test(`test ${name} from float`, t => {
  let testNotNaN = f => {
    let n = fromFloatExn(f)
    t->Assert.deepEqual(FloatModule.fromFloat(f), Some(n), ())
    t->Assert.deepEqual(fromFloatExn(f), n, ())
    t->Assert.deepEqual(FloatModule.fromFloatClamped(f), n, ())
    t->Assert.deepEqual(FloatModule.fromFloatUnsafe(f), n, ())
  }

  testFn(testNotNaN)

  t->Assert.deepEqual(FloatModule.fromFloat(nan), None, ())
  t->assertInvalidArgument(() => fromFloatExn(nan))
  t->Assert.deepEqual(FloatModule.fromFloatClamped(nan), zero, ())
})

test(`test ${name} to float`, t => {
  let testToFloat = f => {
    let n = fromFloatExn(f)
    t->Assert.deepEqual(n->FloatModule.toFloat, f, ())
  }

  testFn(testToFloat)
})

test(`test ${name} from string`, t => {
  let testNotNaN = (f, s) => {
    let n = fromFloatExn(f)
    t->Assert.deepEqual(FloatModule.fromString(s), Some(n), ())
    t->Assert.deepEqual(FloatModule.fromStringExn(s), n, ())
  }

  testNotNaN(0.0, "0")
  testNotNaN(-0.0, "-0")
  testNotNaN(1.0, "1")
  testNotNaN(value, value->Float.toString)
  testNotNaN(minIntFloat, minIntFloat->Float.toString)
  testNotNaN(maxIntFloat, maxIntFloat->Float.toString)
  testNotNaN(minValue, minValue->Float.toString)
  testNotNaN(maxValue, maxValue->Float.toString)
  loop100Fn(() => {
    let n = randomValue()
    testNotNaN(n, n->Float.toString)
  })

  let testIsNotFloat = s => {
    t->Assert.deepEqual(FloatModule.fromString(s), None, ())
    t->assertInvalidArgument(() => FloatModule.fromStringExn(s))
  }

  testIsNotFloat("NaN")
  testIsNotFloat("foo")
  testIsNotFloat("b1")
})

test(`test ${name} to string`, t => {
  let testToString = f => {
    let s = f->Float.toString
    t->Assert.deepEqual(fromFloatExn(f)->FloatModule.toString, s, ())
  }

  testFn(testToString)

  let testToExponential = f => {
    let s = f->Js.Float.toExponential
    let n = fromFloatExn(f)
    t->Assert.deepEqual(n->FloatModule.toExponential, s, ())

    for digits in 0 to 100 {
      let s = f->Js.Float.toExponentialWithPrecision(~digits)
      t->Assert.deepEqual(n->FloatModule.toExponentialWithPrecisionExn(~digits), s, ())
    }
    if f !== minValue && f !== maxValue {
      t->Assert.throws(() => n->FloatModule.toExponentialWithPrecisionExn(~digits=-1), ())
      t->Assert.throws(() => n->FloatModule.toExponentialWithPrecisionExn(~digits=101), ())
    }
  }

  testFn(testToExponential)

  let testToPrecision = f => {
    let s = f->Js.Float.toPrecision
    let n = fromFloatExn(f)
    t->Assert.deepEqual(n->FloatModule.toPrecision, s, ())

    for digits in 1 to 100 {
      let s = f->Js.Float.toPrecisionWithPrecision(~digits)
      t->Assert.deepEqual(n->FloatModule.toPrecisionWithPrecisionExn(~digits), s, ())
    }
    if f !== minValue && f !== maxValue {
      t->Assert.throws(() => n->FloatModule.toPrecisionWithPrecisionExn(~digits=0), ())
      t->Assert.throws(() => n->FloatModule.toPrecisionWithPrecisionExn(~digits=101), ())
    }
  }

  testFn(testToPrecision)

  let testToFixed = f => {
    let s = f->Js.Float.toFixed
    let n = fromFloatExn(f)
    t->Assert.deepEqual(n->FloatModule.toFixed, s, ())

    for digits in 0 to 100 {
      let s = f->Js.Float.toFixedWithPrecision(~digits)
      t->Assert.deepEqual(n->FloatModule.toFixedWithPrecisionExn(~digits), s, ())
    }
    t->Assert.throws(() => n->FloatModule.toFixedWithPrecisionExn(~digits=-1), ())
    t->Assert.throws(() => n->FloatModule.toFixedWithPrecisionExn(~digits=101), ())
  }

  testFn(testToFixed)
})

test(`test ${name} zero`, t => {
  t->Assert.deepEqual(zero, fromIntExn(0), ())
  t->Assert.deepEqual(zero, fromFloatExn(0.0), ())
  t->Assert.notDeepEqual(zero, fromFloatExn(-0.0), ())
  t->Assert.isTrue(zero->FloatModule.eq(fromFloatExn(0.0)), ())
})

test(`test ${name} one`, t => {
  t->Assert.deepEqual(one, fromIntExn(1), ())
  t->Assert.deepEqual(one, fromFloatExn(1.0), ())
})

test(`test ${name} range`, t => {
  t->Assert.deepEqual(FloatModule.minValue, fromFloatExn(minValue), ())
  t->Assert.deepEqual(FloatModule.maxValue, fromFloatExn(maxValue), ())
})

test(`test ${name} comparison`, t => {
  let testCompare = (less, greater) => {
    let less = fromFloatExn(less)
    let greater = fromFloatExn(greater)
    let value = fromFloatExn(value)
    let minValue = fromFloatExn(minValue)
    let maxValue = fromFloatExn(maxValue)

    t->Assert.deepEqual(FloatModule.compare(less, greater), Some(Less), ())
    t->Assert.deepEqual(FloatModule.compare(greater, less), Some(Greater), ())
    t->Assert.deepEqual(FloatModule.compare(less, less), Some(Equal), ())
    t->Assert.deepEqual(FloatModule.compare(greater, greater), Some(Equal), ())

    t->Assert.deepEqual(FloatModule.compareExn(less, greater), Less, ())
    t->Assert.deepEqual(FloatModule.compareExn(greater, less), Greater, ())
    t->Assert.deepEqual(FloatModule.compareExn(less, less), Equal, ())
    t->Assert.deepEqual(FloatModule.compareExn(greater, greater), Equal, ())

    t->Assert.isTrue(less->FloatModule.eq(less), ())
    t->Assert.isTrue(greater->FloatModule.eq(greater), ())
    t->Assert.isFalse(less->FloatModule.eq(greater), ())
    t->Assert.isFalse(greater->FloatModule.eq(less), ())

    t->Assert.isTrue(less->FloatModule.ne(greater), ())
    t->Assert.isTrue(greater->FloatModule.ne(less), ())
    t->Assert.isFalse(less->FloatModule.ne(less), ())
    t->Assert.isFalse(greater->FloatModule.ne(greater), ())

    t->Assert.isTrue(less->FloatModule.lt(greater), ())
    t->Assert.isFalse(greater->FloatModule.lt(less), ())
    t->Assert.isFalse(less->FloatModule.lt(less), ())
    t->Assert.isFalse(greater->FloatModule.lt(greater), ())

    t->Assert.isTrue(less->FloatModule.le(greater), ())
    t->Assert.isFalse(greater->FloatModule.le(less), ())
    t->Assert.isTrue(less->FloatModule.le(less), ())
    t->Assert.isTrue(greater->FloatModule.le(greater), ())

    t->Assert.isTrue(greater->FloatModule.gt(less), ())
    t->Assert.isFalse(less->FloatModule.gt(greater), ())
    t->Assert.isFalse(less->FloatModule.gt(less), ())
    t->Assert.isFalse(greater->FloatModule.gt(greater), ())

    t->Assert.isTrue(greater->FloatModule.ge(less), ())
    t->Assert.isFalse(less->FloatModule.ge(greater), ())
    t->Assert.isTrue(less->FloatModule.ge(less), ())
    t->Assert.isTrue(greater->FloatModule.ge(greater), ())

    t->Assert.deepEqual(FloatModule.min(less, greater), less, ())
    t->Assert.deepEqual(FloatModule.min(greater, less), less, ())
    t->Assert.deepEqual(FloatModule.min(less, less), less, ())
    t->Assert.deepEqual(FloatModule.min(greater, greater), greater, ())

    t->Assert.deepEqual(FloatModule.minMany([less]), Some(less), ())
    t->Assert.deepEqual(FloatModule.minMany([less, greater]), Some(less), ())
    t->Assert.deepEqual(FloatModule.minMany([greater, less]), Some(less), ())
    t->Assert.deepEqual(
      FloatModule.minMany([less, value, greater, minValue, maxValue]),
      Some(minValue),
      (),
    )
    t->Assert.deepEqual(FloatModule.minManyExn([less]), less, ())
    t->Assert.deepEqual(FloatModule.minManyExn([less, greater]), less, ())
    t->Assert.deepEqual(FloatModule.minManyExn([greater, less]), less, ())
    t->Assert.deepEqual(
      FloatModule.minManyExn([minValue, maxValue, less, value, greater]),
      minValue,
      (),
    )
    t->Assert.deepEqual(FloatModule.minManyUnsafe([less]), less, ())
    t->Assert.deepEqual(FloatModule.minManyUnsafe([less, greater]), less, ())
    t->Assert.deepEqual(FloatModule.minManyUnsafe([greater, less]), less, ())
    t->Assert.deepEqual(
      FloatModule.minManyUnsafe([less, minValue, maxValue, value, greater]),
      minValue,
      (),
    )

    t->Assert.deepEqual(FloatModule.max(less, greater), greater, ())
    t->Assert.deepEqual(FloatModule.max(greater, less), greater, ())
    t->Assert.deepEqual(FloatModule.max(less, less), less, ())
    t->Assert.deepEqual(FloatModule.max(greater, greater), greater, ())

    t->Assert.deepEqual(FloatModule.maxMany([less]), Some(less), ())
    t->Assert.deepEqual(FloatModule.maxMany([less, greater]), Some(greater), ())
    t->Assert.deepEqual(FloatModule.maxMany([greater, less]), Some(greater), ())
    t->Assert.deepEqual(
      FloatModule.maxMany([less, value, greater, minValue, maxValue]),
      Some(maxValue),
      (),
    )
    t->Assert.deepEqual(FloatModule.maxManyExn([less]), less, ())
    t->Assert.deepEqual(FloatModule.maxManyExn([less, greater]), greater, ())
    t->Assert.deepEqual(FloatModule.maxManyExn([greater, less]), greater, ())
    t->Assert.deepEqual(
      FloatModule.maxManyExn([minValue, maxValue, less, value, greater]),
      maxValue,
      (),
    )
    t->Assert.deepEqual(FloatModule.maxManyUnsafe([less]), less, ())
    t->Assert.deepEqual(FloatModule.maxManyUnsafe([less, greater]), greater, ())
    t->Assert.deepEqual(FloatModule.maxManyUnsafe([greater, less]), greater, ())
    t->Assert.deepEqual(
      FloatModule.maxManyUnsafe([less, minValue, maxValue, value, greater]),
      maxValue,
      (),
    )
  }

  testCompare(less, greater)
  loop100Fn(() => {
    let less = randomValue()
    let greater = randomFloat(less +. 1.0, maxIntFloat *. 3.0)
    testCompare(less, greater)
  })

  t->Assert.isTrue(zero->FloatModule.eq(negZero), ())
  t->Assert.isTrue(zero->FloatModule.le(negZero), ())
  t->Assert.isTrue(zero->FloatModule.ge(negZero), ())
  t->Assert.isFalse(zero->FloatModule.lt(negZero), ())
  t->Assert.isFalse(zero->FloatModule.gt(negZero), ())

  t->Assert.deepEqual(FloatModule.minMany([]), None, ())
  t->assertInvalidArgument(() => FloatModule.minManyExn([]))
  t->Assert.deepEqual(FloatModule.maxMany([]), None, ())
  t->assertInvalidArgument(() => FloatModule.maxManyExn([]))
})

test(`test ${name} addition`, t => {
  let testNotNaN = (a, b) => {
    if a->isFinite {
      let negA = fromFloatExn(-.a)
      let a = fromFloatExn(a)
      t->Assert.deepEqual(a->FloatModule.add(negA), Some(zero), ())
      t->Assert.deepEqual(a->FloatModule.add(negA), negA->FloatModule.add(a), ())
      t->Assert.deepEqual(a->FloatModule.addExn(negA), zero, ())
      t->Assert.deepEqual(a->FloatModule.addExn(negA), negA->FloatModule.addExn(a), ())
      t->Assert.deepEqual(a->FloatModule.addClamped(negA), zero, ())
      t->Assert.deepEqual(a->FloatModule.addClamped(negA), negA->FloatModule.addClamped(a), ())
      t->Assert.deepEqual(a->FloatModule.addUnsafe(negA), zero, ())
      t->Assert.deepEqual(a->FloatModule.addUnsafe(negA), negA->FloatModule.addUnsafe(a), ())
    }

    if b->isFinite {
      let negB = fromFloatExn(-.b)
      let b = fromFloatExn(b)
      t->Assert.deepEqual(b->FloatModule.add(negB), Some(zero), ())
      t->Assert.deepEqual(b->FloatModule.add(negB), negB->FloatModule.add(b), ())
      t->Assert.deepEqual(b->FloatModule.addExn(negB), zero, ())
      t->Assert.deepEqual(b->FloatModule.addExn(negB), negB->FloatModule.addExn(b), ())
      t->Assert.deepEqual(b->FloatModule.addClamped(negB), zero, ())
      t->Assert.deepEqual(b->FloatModule.addClamped(negB), negB->FloatModule.addClamped(b), ())
      t->Assert.deepEqual(b->FloatModule.addUnsafe(negB), zero, ())
      t->Assert.deepEqual(b->FloatModule.addUnsafe(negB), negB->FloatModule.addUnsafe(b), ())
    }

    let result = fromFloatExn(a +. b)
    let a = fromFloatExn(a)
    let b = fromFloatExn(b)

    t->Assert.deepEqual(a->FloatModule.add(b), Some(result), ())
    t->Assert.deepEqual(a->FloatModule.add(b), b->FloatModule.add(a), ())
    t->Assert.deepEqual(a->FloatModule.addExn(b), result, ())
    t->Assert.deepEqual(a->FloatModule.addExn(b), b->FloatModule.addExn(a), ())
    t->Assert.deepEqual(a->FloatModule.addClamped(b), result, ())
    t->Assert.deepEqual(a->FloatModule.addClamped(b), b->FloatModule.addClamped(a), ())
    t->Assert.deepEqual(a->FloatModule.addUnsafe(b), result, ())
    t->Assert.deepEqual(a->FloatModule.addUnsafe(b), b->FloatModule.addUnsafe(a), ())

    let result = fromFloatExn(a->FloatModule.toFloat +. 0.0)
    t->Assert.deepEqual(a->FloatModule.add(zero), Some(result), ())
    t->Assert.deepEqual(a->FloatModule.add(zero), zero->FloatModule.add(a), ())
    t->Assert.deepEqual(a->FloatModule.addExn(zero), result, ())
    t->Assert.deepEqual(a->FloatModule.addExn(zero), zero->FloatModule.addExn(a), ())
    t->Assert.deepEqual(a->FloatModule.addClamped(zero), result, ())
    t->Assert.deepEqual(a->FloatModule.addClamped(zero), zero->FloatModule.addClamped(a), ())
    t->Assert.deepEqual(a->FloatModule.addUnsafe(zero), result, ())
    t->Assert.deepEqual(a->FloatModule.addUnsafe(zero), zero->FloatModule.addUnsafe(a), ())

    let result = fromFloatExn(b->FloatModule.toFloat +. 0.0)
    t->Assert.deepEqual(b->FloatModule.add(zero), Some(result), ())
    t->Assert.deepEqual(b->FloatModule.add(zero), zero->FloatModule.add(b), ())
    t->Assert.deepEqual(b->FloatModule.addExn(zero), result, ())
    t->Assert.deepEqual(b->FloatModule.addExn(zero), zero->FloatModule.addExn(b), ())
    t->Assert.deepEqual(b->FloatModule.addClamped(zero), result, ())
    t->Assert.deepEqual(b->FloatModule.addClamped(zero), zero->FloatModule.addClamped(b), ())
    t->Assert.deepEqual(b->FloatModule.addUnsafe(zero), result, ())
    t->Assert.deepEqual(b->FloatModule.addUnsafe(zero), zero->FloatModule.addUnsafe(b), ())
  }

  testNotNaN(0.0, -0.0)
  testNotNaN(less, greater)
  testNotNaN(minIntFloat, -1.0)
  testNotNaN(maxIntFloat, 1.0)
  testNotNaN(minValue, 0.0)
  testNotNaN(minValue, -0.0)
  testNotNaN(minValue, 1.0)
  testNotNaN(minValue, -1.0)
  testNotNaN(minValue, minValue)
  testNotNaN(maxValue, 0.0)
  testNotNaN(maxValue, -0.0)
  testNotNaN(maxValue, 1.0)
  testNotNaN(maxValue, -1.0)
  testNotNaN(maxValue, maxValue)
  loop100Fn(() => {
    let a = randomValue()
    let b = randomValue()
    testNotNaN(a, b)
  })

  let testNaN = (a, b) => {
    let a = fromFloatExn(a)
    let b = fromFloatExn(b)
    t->Assert.deepEqual(a->FloatModule.add(b), None, ())
    t->Assert.deepEqual(b->FloatModule.add(a), None, ())
    t->assertInvalidArgument(() => a->FloatModule.addExn(b))
    t->assertInvalidArgument(() => b->FloatModule.addExn(a))
    t->Assert.deepEqual(a->FloatModule.addClamped(b), zero, ())
    t->Assert.deepEqual(b->FloatModule.addClamped(a), zero, ())
  }

  testNaN(maxValue, minValue)
})

test(`test ${name} subtraction`, t => {
  let testNotNaN = (a, b) => {
    if a->isFinite {
      let a = fromFloatExn(a)
      t->Assert.deepEqual(a->FloatModule.sub(a), Some(zero), ())
      t->Assert.deepEqual(a->FloatModule.subExn(a), zero, ())
      t->Assert.deepEqual(a->FloatModule.subClamped(a), zero, ())
      t->Assert.deepEqual(a->FloatModule.subUnsafe(a), zero, ())
    }

    if b->isFinite {
      let b = fromFloatExn(b)
      t->Assert.deepEqual(b->FloatModule.sub(b), Some(zero), ())
      t->Assert.deepEqual(b->FloatModule.subExn(b), zero, ())
      t->Assert.deepEqual(b->FloatModule.subClamped(b), zero, ())
      t->Assert.deepEqual(b->FloatModule.subUnsafe(b), zero, ())
    }

    let result = fromFloatExn(a -. b)
    let a = fromFloatExn(a)
    let b = fromFloatExn(b)
    t->Assert.deepEqual(a->FloatModule.sub(b), Some(result), ())
    t->Assert.deepEqual(a->FloatModule.subExn(b), result, ())
    t->Assert.deepEqual(a->FloatModule.subClamped(b), result, ())
    t->Assert.deepEqual(a->FloatModule.subUnsafe(b), result, ())

    t->Assert.deepEqual(a->FloatModule.sub(zero), Some(a), ())
    t->Assert.deepEqual(a->FloatModule.subExn(zero), a, ())
    t->Assert.deepEqual(a->FloatModule.subClamped(zero), a, ())
    t->Assert.deepEqual(a->FloatModule.subUnsafe(zero), a, ())

    t->Assert.deepEqual(b->FloatModule.sub(zero), Some(b), ())
    t->Assert.deepEqual(b->FloatModule.subExn(zero), b, ())
    t->Assert.deepEqual(b->FloatModule.subClamped(zero), b, ())
    t->Assert.deepEqual(b->FloatModule.subUnsafe(zero), b, ())
  }

  testNotNaN(0.0, -0.0)
  testNotNaN(less, greater)
  testNotNaN(greater, less)
  testNotNaN(minIntFloat, 1.0)
  testNotNaN(maxIntFloat, -1.0)
  testNotNaN(minValue, 0.0)
  testNotNaN(minValue, -0.0)
  testNotNaN(minValue, 1.0)
  testNotNaN(minValue, -1.0)
  testNotNaN(minValue, value)
  testNotNaN(maxValue, 0.0)
  testNotNaN(maxValue, -0.0)
  testNotNaN(maxValue, 1.0)
  testNotNaN(maxValue, -1.0)
  testNotNaN(maxValue, value)
  testNotNaN(minValue, maxValue)
  testNotNaN(maxValue, minValue)
  testNotNaN(0.0, minValue)
  testNotNaN(-0.0, minValue)
  testNotNaN(1.0, minValue)
  testNotNaN(-1.0, minValue)
  testNotNaN(value, minValue)
  testNotNaN(0.0, maxValue)
  testNotNaN(-0.0, maxValue)
  testNotNaN(1.0, maxValue)
  testNotNaN(-1.0, maxValue)
  testNotNaN(value, maxValue)
  loop100Fn(() => {
    let a = randomValue()
    let b = randomValue()
    testNotNaN(a, b)
  })

  let testNaN = (a, b) => {
    let a = fromFloatExn(a)
    let b = fromFloatExn(b)
    t->Assert.deepEqual(a->FloatModule.sub(b), None, ())
    t->assertInvalidArgument(() => a->FloatModule.subExn(b))
    t->Assert.deepEqual(a->FloatModule.subClamped(b), zero, ())
  }

  testNaN(minValue, minValue)
  testNaN(maxValue, maxValue)
})

test(`test ${name} multiplication`, t => {
  let testNotNaN = (a, b) => {
    if a->isFinite {
      let result = fromFloatExn(a *. 0.0)
      let a = fromFloatExn(a)
      t->Assert.deepEqual(a->FloatModule.mul(zero), Some(result), ())
      t->Assert.deepEqual(a->FloatModule.mul(zero), zero->FloatModule.mul(a), ())
      t->Assert.deepEqual(a->FloatModule.mulExn(zero), result, ())
      t->Assert.deepEqual(a->FloatModule.mulExn(zero), zero->FloatModule.mulExn(a), ())
      t->Assert.deepEqual(a->FloatModule.mulClamped(zero), result, ())
      t->Assert.deepEqual(a->FloatModule.mulClamped(zero), zero->FloatModule.mulClamped(a), ())
      t->Assert.deepEqual(a->FloatModule.mulUnsafe(zero), result, ())
      t->Assert.deepEqual(a->FloatModule.mulUnsafe(zero), zero->FloatModule.mulUnsafe(a), ())
    }

    if b->isFinite {
      let result = fromFloatExn(b *. 0.0)
      let b = fromFloatExn(b)
      t->Assert.deepEqual(b->FloatModule.mul(zero), Some(result), ())
      t->Assert.deepEqual(b->FloatModule.mul(zero), zero->FloatModule.mul(b), ())
      t->Assert.deepEqual(b->FloatModule.mulExn(zero), result, ())
      t->Assert.deepEqual(b->FloatModule.mulExn(zero), zero->FloatModule.mulExn(b), ())
      t->Assert.deepEqual(b->FloatModule.mulClamped(zero), result, ())
      t->Assert.deepEqual(b->FloatModule.mulClamped(zero), zero->FloatModule.mulClamped(b), ())
      t->Assert.deepEqual(b->FloatModule.mulUnsafe(zero), result, ())
      t->Assert.deepEqual(b->FloatModule.mulUnsafe(zero), zero->FloatModule.mulUnsafe(b), ())
    }

    let result = fromFloatExn(a *. b)
    let a = fromFloatExn(a)
    let b = fromFloatExn(b)
    t->Assert.deepEqual(a->FloatModule.mul(b), Some(result), ())
    t->Assert.deepEqual(a->FloatModule.mul(b), b->FloatModule.mul(a), ())
    t->Assert.deepEqual(a->FloatModule.mulExn(b), result, ())
    t->Assert.deepEqual(a->FloatModule.mulExn(b), b->FloatModule.mulExn(a), ())
    t->Assert.deepEqual(a->FloatModule.mulClamped(b), result, ())
    t->Assert.deepEqual(a->FloatModule.mulClamped(b), b->FloatModule.mulClamped(a), ())
    t->Assert.deepEqual(a->FloatModule.mulUnsafe(b), result, ())
    t->Assert.deepEqual(a->FloatModule.mulUnsafe(b), b->FloatModule.mulUnsafe(a), ())

    t->Assert.deepEqual(a->FloatModule.mul(one), Some(a), ())
    t->Assert.deepEqual(a->FloatModule.mul(one), one->FloatModule.mul(a), ())
    t->Assert.deepEqual(a->FloatModule.mulExn(one), a, ())
    t->Assert.deepEqual(a->FloatModule.mulExn(one), one->FloatModule.mulExn(a), ())
    t->Assert.deepEqual(a->FloatModule.mulClamped(one), a, ())
    t->Assert.deepEqual(a->FloatModule.mulClamped(one), one->FloatModule.mulClamped(a), ())
    t->Assert.deepEqual(a->FloatModule.mulUnsafe(one), a, ())
    t->Assert.deepEqual(a->FloatModule.mulUnsafe(one), one->FloatModule.mulUnsafe(a), ())

    t->Assert.deepEqual(b->FloatModule.mul(one), Some(b), ())
    t->Assert.deepEqual(b->FloatModule.mul(one), one->FloatModule.mul(b), ())
    t->Assert.deepEqual(b->FloatModule.mulExn(one), b, ())
    t->Assert.deepEqual(b->FloatModule.mulExn(one), one->FloatModule.mulExn(b), ())
    t->Assert.deepEqual(b->FloatModule.mulClamped(one), b, ())
    t->Assert.deepEqual(b->FloatModule.mulClamped(one), one->FloatModule.mulClamped(b), ())
    t->Assert.deepEqual(b->FloatModule.mulUnsafe(one), b, ())
    t->Assert.deepEqual(b->FloatModule.mulUnsafe(one), one->FloatModule.mulUnsafe(b), ())
  }

  testNotNaN(0.0, -1.0)
  testNotNaN(-0.0, 1.0)
  testNotNaN(less, greater)
  testNotNaN(minIntFloat, value)
  testNotNaN(maxIntFloat, value)
  testNotNaN(minValue, value)
  testNotNaN(minValue, -.value)
  testNotNaN(maxValue, value)
  testNotNaN(maxValue, -.value)
  testNotNaN(minValue, maxValue)
  loop100Fn(() => {
    let a = randomValue()
    let b = randomValue()
    testNotNaN(a, b)
  })

  let testNaN = (a, b) => {
    let a = fromFloatExn(a)
    let b = fromFloatExn(b)
    t->Assert.deepEqual(a->FloatModule.mul(b), None, ())
    t->Assert.deepEqual(b->FloatModule.mul(a), None, ())
    t->assertInvalidArgument(() => a->FloatModule.mulExn(b))
    t->assertInvalidArgument(() => b->FloatModule.mulExn(a))
    t->Assert.deepEqual(a->FloatModule.mulClamped(b), zero, ())
    t->Assert.deepEqual(b->FloatModule.mulClamped(a), zero, ())
  }

  testNaN(minValue, 0.0)
  testNaN(maxValue, 0.0)
})

test(`test ${name} division`, t => {
  let testNotNaN = (a, b) => {
    if a !== 0.0 {
      let result = fromFloatExn(b /. a)
      let a = fromFloatExn(a)
      let b = fromFloatExn(b)
      t->Assert.deepEqual(b->FloatModule.div(a), Some(result), ())
      t->Assert.deepEqual(b->FloatModule.divExn(a), result, ())
      t->Assert.deepEqual(b->FloatModule.divUnsafe(a), result, ())

      if a->FloatModule.isFinite {
        t->Assert.deepEqual(a->FloatModule.div(a), Some(one), ())
        t->Assert.deepEqual(a->FloatModule.divExn(a), one, ())
        t->Assert.deepEqual(a->FloatModule.divUnsafe(a), one, ())
      }

      let result = fromFloatExn(0.0 /. a->FloatModule.toFloat)
      t->Assert.deepEqual(zero->FloatModule.div(a), Some(result), ())
      t->Assert.deepEqual(zero->FloatModule.divExn(a), result, ())
      t->Assert.deepEqual(zero->FloatModule.divUnsafe(a), result, ())

      let result = fromFloatExn(-0.0 /. a->FloatModule.toFloat)
      t->Assert.deepEqual(negZero->FloatModule.div(a), Some(result), ())
      t->Assert.deepEqual(negZero->FloatModule.divExn(a), result, ())
      t->Assert.deepEqual(negZero->FloatModule.divUnsafe(a), result, ())

      let result = fromFloatExn(a->FloatModule.toFloat /. 0.0)
      t->Assert.deepEqual(a->FloatModule.div(zero), Some(result), ())
      t->Assert.deepEqual(a->FloatModule.divExn(zero), result, ())
      t->Assert.deepEqual(a->FloatModule.divUnsafe(zero), result, ())

      let result = fromFloatExn(a->FloatModule.toFloat /. -0.0)

      t->Assert.deepEqual(a->FloatModule.div(negZero), Some(result), ())
      t->Assert.deepEqual(a->FloatModule.divExn(negZero), result, ())
      t->Assert.deepEqual(a->FloatModule.divUnsafe(negZero), result, ())
    }

    if b !== 0.0 {
      let result = fromFloatExn(a /. b)
      let a = fromFloatExn(a)
      let b = fromFloatExn(b)
      t->Assert.deepEqual(a->FloatModule.div(b), Some(result), ())
      t->Assert.deepEqual(a->FloatModule.divExn(b), result, ())
      t->Assert.deepEqual(a->FloatModule.divUnsafe(b), result, ())

      if b->FloatModule.isFinite {
        t->Assert.deepEqual(b->FloatModule.div(b), Some(one), ())
        t->Assert.deepEqual(b->FloatModule.divExn(b), one, ())
        t->Assert.deepEqual(b->FloatModule.divUnsafe(b), one, ())
      }

      let result = fromFloatExn(0.0 /. b->FloatModule.toFloat)
      t->Assert.deepEqual(zero->FloatModule.div(b), Some(result), ())
      t->Assert.deepEqual(zero->FloatModule.divExn(b), result, ())
      t->Assert.deepEqual(zero->FloatModule.divUnsafe(b), result, ())

      let result = fromFloatExn(-0.0 /. b->FloatModule.toFloat)
      t->Assert.deepEqual(negZero->FloatModule.div(b), Some(result), ())
      t->Assert.deepEqual(negZero->FloatModule.divExn(b), result, ())
      t->Assert.deepEqual(negZero->FloatModule.divUnsafe(b), result, ())

      let result = fromFloatExn(b->FloatModule.toFloat /. 0.0)
      t->Assert.deepEqual(b->FloatModule.div(zero), Some(result), ())
      t->Assert.deepEqual(b->FloatModule.divExn(zero), result, ())
      t->Assert.deepEqual(b->FloatModule.divUnsafe(zero), result, ())

      let result = fromFloatExn(b->FloatModule.toFloat /. -0.0)
      t->Assert.deepEqual(b->FloatModule.div(negZero), Some(result), ())
      t->Assert.deepEqual(b->FloatModule.divExn(negZero), result, ())
      t->Assert.deepEqual(b->FloatModule.divUnsafe(negZero), result, ())
    }

    let negA = fromFloatExn(-.a)
    let negB = fromFloatExn(-.b)
    let a = fromFloatExn(a)
    let b = fromFloatExn(b)
    t->Assert.deepEqual(a->FloatModule.div(one), Some(a), ())
    t->Assert.deepEqual(a->FloatModule.divExn(one), a, ())
    t->Assert.deepEqual(a->FloatModule.divUnsafe(one), a, ())
    t->Assert.deepEqual(b->FloatModule.div(one), Some(b), ())
    t->Assert.deepEqual(b->FloatModule.divExn(one), b, ())
    t->Assert.deepEqual(b->FloatModule.divUnsafe(one), b, ())

    t->Assert.deepEqual(a->FloatModule.div(negOne), Some(negA), ())
    t->Assert.deepEqual(a->FloatModule.divExn(negOne), negA, ())
    t->Assert.deepEqual(a->FloatModule.divUnsafe(negOne), negA, ())
    t->Assert.deepEqual(b->FloatModule.div(negOne), Some(negB), ())
    t->Assert.deepEqual(b->FloatModule.divExn(negOne), negB, ())
    t->Assert.deepEqual(b->FloatModule.divUnsafe(negOne), negB, ())
  }

  testNotNaN(less, greater)
  testNotNaN(minIntFloat, maxIntFloat)
  testNotNaN(minIntFloat, less)
  testNotNaN(maxIntFloat, greater)
  testNotNaN(minValue, less)
  testNotNaN(minValue, greater)
  testNotNaN(maxValue, less)
  testNotNaN(maxValue, greater)
  loop100Fn(() => {
    let a = randomValue()
    let b = randomValue()
    testNotNaN(a, b)
  })

  let testNaN = (a, b) => {
    let a = fromFloatExn(a)
    let b = fromFloatExn(b)
    t->Assert.deepEqual(a->FloatModule.div(b), None, ())
    t->assertInvalidArgument(() => a->FloatModule.divExn(b))
  }

  testNaN(minValue, minValue)
  testNaN(minValue, maxValue)
  testNaN(maxValue, maxValue)
  testNaN(maxValue, minValue)
  testNaN(0.0, 0.0)
  testNaN(0.0, -0.0)
  testNaN(-0.0, 0.0)
  testNaN(-0.0, -0.0)
})

test(`test ${name} remainder`, t => {
  let testNotNaN = (a, b) => {
    if a !== 0.0 {
      let result = fromFloatExn(b->mod_float(a))
      let a = fromFloatExn(a)
      let b = fromFloatExn(b)
      t->Assert.deepEqual(b->FloatModule.rem(a), Some(result), ())
      t->Assert.deepEqual(b->FloatModule.remExn(a), result, ())
      t->Assert.deepEqual(b->FloatModule.remUnsafe(a), result, ())
      t->Assert.deepEqual(zero->FloatModule.rem(a), Some(zero), ())
      t->Assert.deepEqual(zero->FloatModule.remExn(a), zero, ())
      t->Assert.deepEqual(zero->FloatModule.remUnsafe(a), zero, ())
    }

    if b !== 0.0 {
      let result = fromFloatExn(a->mod_float(b))
      let a = fromFloatExn(a)
      let b = fromFloatExn(b)
      t->Assert.deepEqual(a->FloatModule.rem(b), Some(result), ())
      t->Assert.deepEqual(a->FloatModule.remExn(b), result, ())
      t->Assert.deepEqual(a->FloatModule.remUnsafe(b), result, ())
      t->Assert.deepEqual(zero->FloatModule.rem(b), Some(zero), ())
      t->Assert.deepEqual(zero->FloatModule.remExn(b), zero, ())
      t->Assert.deepEqual(zero->FloatModule.remUnsafe(b), zero, ())
    }
  }

  testNotNaN(less, greater)
  testNotNaN(minIntFloat, maxIntFloat)
  testNotNaN(minIntFloat, less)
  testNotNaN(maxIntFloat, greater)

  let testModByZero = f => {
    let a = fromFloatExn(f)
    t->Assert.deepEqual(a->FloatModule.rem(zero), None, ())
    t->assertDivisionByZero(() => a->FloatModule.remExn(zero))
    t->Assert.deepEqual(a->FloatModule.rem(negZero), None, ())
    t->assertDivisionByZero(() => a->FloatModule.remExn(negZero))
  }

  testFn(testModByZero)

  loop100Fn(() => {
    let a = randomValue()
    let b = randomValue()
    testNotNaN(a, b)
    testModByZero(a)
    testModByZero(b)
  })

  let testNaN = (a, b) => {
    let a = fromFloatExn(a)
    let b = fromFloatExn(b)
    t->Assert.deepEqual(a->FloatModule.rem(b), None, ())
    t->assertInvalidArgument(() => a->FloatModule.remExn(b))
  }

  testNaN(minValue, 1.0)
  testNaN(minValue, value)
  testNaN(minValue, minIntFloat)
  testNaN(minValue, maxIntFloat)
  testNaN(minValue, maxValue)
  testNaN(maxValue, 1.0)
  testNaN(maxValue, value)
  testNaN(maxValue, minIntFloat)
  testNaN(maxValue, maxIntFloat)
  testNaN(maxValue, minValue)
})

test(`test ${name} sum`, t => {
  let testNotNaN = arr => {
    let result = fromFloatExn(arr->Array.reduce(0.0, (acc, v) => acc +. v))
    let arr = arr->Array.mapU(f => fromFloatExn(f))
    t->Assert.deepEqual(arr->FloatModule.sum, Some(result), ())
    t->Assert.deepEqual(arr->FloatModule.sumExn, result, ())
    t->Assert.deepEqual(arr->FloatModule.sumUnsafe, result, ())
  }

  testNotNaN([less])
  testNotNaN([greater])
  testNotNaN([minIntFloat])
  testNotNaN([maxIntFloat])
  testNotNaN([minValue])
  testNotNaN([maxValue])
  testNotNaN([less, greater])
  testNotNaN([greater, less])
  testNotNaN([less, minIntFloat, 0.0, value, maxIntFloat, 1.0, greater, -0.0])
  loop100Fn(() => {
    let a = randomValue()
    let b = randomValue()
    testNotNaN([a])
    testNotNaN([b])
    testNotNaN([a, b])
    testNotNaN([b, a])
    testNotNaN([a, value, maxIntFloat, minIntFloat, b])
  })

  let testNaN = arr => {
    let arr = arr->Array.mapU(i => fromFloatExn(i))
    t->Assert.deepEqual(arr->FloatModule.sum, None, ())
    t->assertInvalidArgument(() => arr->FloatModule.sumExn)
  }

  testNaN([minValue, maxValue])
  testNaN([value, minValue, maxValue])
  testNaN([minValue, maxValue, value])

  t->Assert.deepEqual(FloatModule.sum([]), None, ())
  t->assertInvalidArgument(() => FloatModule.sumExn([]))
})

test(`test ${name} signed math`, t => {
  let testNeg = f => {
    let a = fromFloatExn(f)
    let b = fromFloatExn(-.f)
    t->Assert.deepEqual(a->FloatModule.neg, Some(b), ())
    t->Assert.deepEqual(a->FloatModule.negExn, b, ())
    t->Assert.deepEqual(a->FloatModule.negUnsafe, b, ())
    t->Assert.deepEqual(b->FloatModule.neg, Some(a), ())
    t->Assert.deepEqual(b->FloatModule.negExn, a, ())
    t->Assert.deepEqual(b->FloatModule.negUnsafe, a, ())
  }

  testFn(testNeg)

  let testAbs = f => {
    let a = fromFloatExn(f)
    let b = fromFloatExn(f->Js.Math.abs_float)
    t->Assert.deepEqual(a->FloatModule.abs, Some(b), ())
    t->Assert.deepEqual(a->FloatModule.absExn, b, ())
    t->Assert.deepEqual(a->FloatModule.absUnsafe, b, ())
    t->Assert.deepEqual(b->FloatModule.abs, Some(b), ())
    t->Assert.deepEqual(b->FloatModule.absExn, b, ())
    t->Assert.deepEqual(b->FloatModule.absUnsafe, b, ())
  }

  testFn(testAbs)
  testAbs(-.value)
  testAbs(-.maxIntFloat)

  let testSign = f => {
    let test_ = f => {
      let n = fromFloatExn(f)
      let result = if f < 0.0 {
        Number.Negative
      } else if f > 0.0 {
        Number.Positive
      } else {
        Number.Zero
      }
      let raw = f->Js.Math.sign_float
      t->Assert.deepEqual(n->FloatModule.sign, Some(result), ())
      t->Assert.deepEqual(n->FloatModule.signExn, result, ())
      t->Assert.deepEqual(n->FloatModule.signRaw, raw, ())
    }

    test_(f)
    test_(-.f)
  }

  testFn(testSign)
})

test(`test ${name} float math`, t => {
  module Math = Js.Math

  let testAcos = f => {
    let n = fromFloatExn(f)
    if f >= -1.0 && f <= 1.0 {
      let result = fromFloatExn(Math.acos(f))
      t->Assert.deepEqual(FloatModule.acos(n), Some(result), ())
      t->Assert.deepEqual(FloatModule.acosExn(n), result, ())
      t->Assert.deepEqual(FloatModule.acosUnsafe(n), result, ())
    } else {
      t->Assert.deepEqual(FloatModule.acos(n), None, ())
      t->assertInvalidArgument(() => FloatModule.acosExn(n))
    }
  }

  testFn(testAcos)
  loop100Fn(() => testAcos(randomFloat(-1.0, 1.0)))

  let testAcosh = f => {
    let n = fromFloatExn(f)
    if f >= 1.0 {
      let result = fromFloatExn(Math.acosh(f))
      t->Assert.deepEqual(FloatModule.acosh(n), Some(result), ())
      t->Assert.deepEqual(FloatModule.acoshExn(n), result, ())
      t->Assert.deepEqual(FloatModule.acoshUnsafe(n), result, ())
    } else {
      t->Assert.deepEqual(FloatModule.acosh(n), None, ())
      t->assertInvalidArgument(() => FloatModule.acoshExn(n))
    }
  }

  testFn(testAcosh)
  loop100Fn(() => testAcosh(randomFloat(1.0, maxIntFloat *. 2.0)))

  let testAsin = f => {
    let n = fromFloatExn(f)
    if f >= -1.0 && f <= 1.0 {
      let result = fromFloatExn(Math.asin(f))
      t->Assert.deepEqual(FloatModule.asin(n), Some(result), ())
      t->Assert.deepEqual(FloatModule.asinExn(n), result, ())
      t->Assert.deepEqual(FloatModule.asinUnsafe(n), result, ())
    } else {
      t->Assert.deepEqual(FloatModule.asin(n), None, ())
      t->assertInvalidArgument(() => FloatModule.asinExn(n))
    }
  }

  testFn(testAsin)
  loop100Fn(() => testAsin(randomFloat(-1.0, 1.0)))

  let testAsinh = f => {
    t->Assert.deepEqual(FloatModule.asinh(fromFloatExn(f)), fromFloatExn(Math.asinh(f)), ())
    t->Assert.deepEqual(FloatModule.asinh(fromFloatExn(-.f)), fromFloatExn(Math.asinh(-.f)), ())
  }

  testFn(testAsinh)

  let testAtan = f => {
    t->Assert.deepEqual(FloatModule.atan(fromFloatExn(f)), fromFloatExn(Math.atan(f)), ())
    t->Assert.deepEqual(FloatModule.atan(fromFloatExn(-.f)), fromFloatExn(Math.atan(-.f)), ())
  }

  testFn(testAtan)

  let testAtan2 = (y, x) => {
    t->Assert.deepEqual(
      FloatModule.atan2(~y=fromFloatExn(y), ~x=fromFloatExn(x)),
      fromFloatExn(Math.atan2(~y, ~x, ())),
      (),
    )
    t->Assert.deepEqual(
      FloatModule.atan2(~y=fromFloatExn(-.y), ~x=fromFloatExn(x)),
      fromFloatExn(Math.atan2(~y=-.y, ~x, ())),
      (),
    )
    t->Assert.deepEqual(
      FloatModule.atan2(~y=fromFloatExn(y), ~x=fromFloatExn(-.x)),
      fromFloatExn(Math.atan2(~y, ~x=-.x, ())),
      (),
    )
    t->Assert.deepEqual(
      FloatModule.atan2(~y=fromFloatExn(-.y), ~x=fromFloatExn(-.x)),
      fromFloatExn(Math.atan2(~y=-.y, ~x=-.x, ())),
      (),
    )

    t->Assert.deepEqual(
      FloatModule.atan2(~y=fromFloatExn(x), ~x=fromFloatExn(y)),
      fromFloatExn(Math.atan2(~y=x, ~x=y, ())),
      (),
    )
    t->Assert.deepEqual(
      FloatModule.atan2(~y=fromFloatExn(-.x), ~x=fromFloatExn(y)),
      fromFloatExn(Math.atan2(~y=-.x, ~x=y, ())),
      (),
    )
    t->Assert.deepEqual(
      FloatModule.atan2(~y=fromFloatExn(x), ~x=fromFloatExn(-.y)),
      fromFloatExn(Math.atan2(~y=x, ~x=-.y, ())),
      (),
    )
    t->Assert.deepEqual(
      FloatModule.atan2(~y=fromFloatExn(-.x), ~x=fromFloatExn(-.y)),
      fromFloatExn(Math.atan2(~y=-.x, ~x=-.y, ())),
      (),
    )
  }

  testAtan2(0.0, -0.0)
  testAtan2(0.0, value)
  testAtan2(0.0, minValue)
  testAtan2(0.0, maxValue)
  testAtan2(less, greater)
  testAtan2(value, minValue)
  testAtan2(value, maxValue)
  testAtan2(minValue, maxValue)
  loop100Fn(() => testAtan2(randomValue(), randomValue()))

  let testAtanh = f => {
    let n = fromFloatExn(f)
    if f >= -1.0 && f <= 1.0 {
      let result = fromFloatExn(Math.atanh(f))
      t->Assert.deepEqual(FloatModule.atanh(n), Some(result), ())
      t->Assert.deepEqual(FloatModule.atanhExn(n), result, ())
      t->Assert.deepEqual(FloatModule.atanhUnsafe(n), result, ())
    } else {
      t->Assert.deepEqual(FloatModule.atanh(n), None, ())
      t->assertInvalidArgument(() => FloatModule.atanhExn(n))
    }
  }

  testFn(testAtanh)
  loop100Fn(() => testAtanh(randomFloat(-1.0, 1.0)))

  let testCbrt = f => {
    t->Assert.deepEqual(FloatModule.cbrt(fromFloatExn(f)), fromFloatExn(Math.cbrt(f)), ())
    t->Assert.deepEqual(FloatModule.cbrt(fromFloatExn(-.f)), fromFloatExn(Math.cbrt(-.f)), ())
  }

  testFn(testCbrt)

  let testCeil = f => {
    let test_ = f => {
      let n = fromFloatExn(f)
      let result = f->Math.ceil_float
      t->Assert.deepEqual(n->FloatModule.ceil, fromFloatExn(result), ())
      if f > minIntFloat -. 1.0 && f <= maxIntFloat {
        let result = result->Float.toInt
        t->Assert.deepEqual(n->FloatModule.ceilInt, Some(result), ())
        t->Assert.deepEqual(n->FloatModule.ceilIntExn, result, ())
        t->Assert.deepEqual(n->FloatModule.ceilIntClamped, result, ())
        t->Assert.deepEqual(n->FloatModule.ceilIntUnsafe, result, ())
      } else {
        t->Assert.deepEqual(n->FloatModule.ceilInt, None, ())
        t->assertOverflow(() => n->FloatModule.ceilIntExn)
        t->Assert.deepEqual(n->FloatModule.ceilIntClamped, f->clampFloatToInt, ())
      }
    }

    test_(f)
    test_(-.f)
  }

  testFn(testCeil)
  testCeil(minIntFloat -. 0.1)
  testCeil(minIntFloat -. 0.9)
  testCeil(minIntFloat -. 1.0)
  loop100Fn(() => testCeil(randomFloatInInt32Range()))

  let testCos = f => {
    let n = fromFloatExn(f)
    if f->isFinite {
      let result = fromFloatExn(Math.cos(f))
      t->Assert.deepEqual(FloatModule.cos(n), Some(result), ())
      t->Assert.deepEqual(FloatModule.cosExn(n), result, ())
      t->Assert.deepEqual(FloatModule.cosUnsafe(n), result, ())
    } else {
      t->Assert.deepEqual(FloatModule.cos(n), None, ())
      t->assertInvalidArgument(() => FloatModule.cosExn(n))
    }
  }

  testFn(testCos)

  let testCosh = f => {
    t->Assert.deepEqual(FloatModule.cosh(fromFloatExn(f)), fromFloatExn(Math.cosh(f)), ())
    t->Assert.deepEqual(FloatModule.cosh(fromFloatExn(-.f)), fromFloatExn(Math.cosh(-.f)), ())
  }

  testFn(testCosh)

  let testExp = f => {
    t->Assert.deepEqual(FloatModule.exp(fromFloatExn(f)), fromFloatExn(Math.exp(f)), ())
    t->Assert.deepEqual(FloatModule.exp(fromFloatExn(-.f)), fromFloatExn(Math.exp(-.f)), ())
  }

  testFn(testExp)

  let testExpm1 = f => {
    t->Assert.deepEqual(FloatModule.expm1(fromFloatExn(f)), fromFloatExn(Math.expm1(f)), ())
    t->Assert.deepEqual(FloatModule.expm1(fromFloatExn(-.f)), fromFloatExn(Math.expm1(-.f)), ())
  }

  testFn(testExpm1)

  let testFloor = f => {
    let test_ = f => {
      let n = fromFloatExn(f)
      let result = f->Math.floor_float
      t->Assert.deepEqual(n->FloatModule.floor, fromFloatExn(result), ())
      if f >= minIntFloat && f < maxIntFloat +. 1.0 {
        let result = result->Float.toInt
        t->Assert.deepEqual(n->FloatModule.floorInt, Some(result), ())
        t->Assert.deepEqual(n->FloatModule.floorIntExn, result, ())
        t->Assert.deepEqual(n->FloatModule.floorIntClamped, result, ())
        t->Assert.deepEqual(n->FloatModule.floorIntUnsafe, result, ())
      } else {
        t->Assert.deepEqual(n->FloatModule.floorInt, None, ())
        t->assertOverflow(() => n->FloatModule.floorIntExn)
        t->Assert.deepEqual(n->FloatModule.floorIntClamped, f->clampFloatToInt, ())
      }
    }

    test_(f)
    test_(-.f)
  }

  testFn(testFloor)
  testFloor(maxIntFloat +. 0.1)
  testFloor(maxIntFloat +. 0.9)
  testFloor(maxIntFloat +. 1.0)
  loop100Fn(() => testFloor(randomFloatInInt32Range()))

  let testFround = f => {
    t->Assert.deepEqual(FloatModule.fround(fromFloatExn(f)), fromFloatExn(Math.fround(f)), ())
    t->Assert.deepEqual(FloatModule.fround(fromFloatExn(-.f)), fromFloatExn(Math.fround(-.f)), ())
  }

  testFn(testFround)

  let testHypot = (a, b) => {
    t->Assert.deepEqual(
      FloatModule.hypot(fromFloatExn(a), fromFloatExn(b)),
      fromFloatExn(Math.hypot(a, b)),
      (),
    )
    t->Assert.deepEqual(
      FloatModule.hypot(fromFloatExn(-.a), fromFloatExn(b)),
      fromFloatExn(Math.hypot(-.a, b)),
      (),
    )
    t->Assert.deepEqual(
      FloatModule.hypot(fromFloatExn(a), fromFloatExn(-.b)),
      fromFloatExn(Math.hypot(a, -.b)),
      (),
    )
    t->Assert.deepEqual(
      FloatModule.hypot(fromFloatExn(-.a), fromFloatExn(-.b)),
      fromFloatExn(Math.hypot(-.a, -.b)),
      (),
    )

    t->Assert.deepEqual(
      FloatModule.hypot(fromFloatExn(b), fromFloatExn(a)),
      FloatModule.hypot(fromFloatExn(a), fromFloatExn(b)),
      (),
    )
    t->Assert.deepEqual(
      FloatModule.hypot(fromFloatExn(-.b), fromFloatExn(a)),
      FloatModule.hypot(fromFloatExn(a), fromFloatExn(-.b)),
      (),
    )
    t->Assert.deepEqual(
      FloatModule.hypot(fromFloatExn(b), fromFloatExn(-.a)),
      FloatModule.hypot(fromFloatExn(-.a), fromFloatExn(b)),
      (),
    )
    t->Assert.deepEqual(
      FloatModule.hypot(fromFloatExn(-.b), fromFloatExn(-.a)),
      FloatModule.hypot(fromFloatExn(-.a), fromFloatExn(-.b)),
      (),
    )
  }

  testHypot(0.0, -0.0)
  testHypot(0.0, value)
  testHypot(0.0, minValue)
  testHypot(0.0, maxValue)
  testHypot(less, greater)
  testHypot(value, minValue)
  testHypot(value, maxValue)
  testHypot(minValue, maxValue)
  loop100Fn(() => testHypot(randomValue(), randomValue()))

  let testHypotMany = arr =>
    t->Assert.deepEqual(
      FloatModule.hypotMany(arr->Array.mapU(f => fromFloatExn(f))),
      fromFloatExn(Math.hypotMany(arr)),
      (),
    )

  testHypotMany([])
  testHypotMany([0.0])
  testHypotMany([-0.0])
  testHypotMany([1.0])
  testHypotMany([-1.0])
  testHypotMany([value])
  testHypotMany([less])
  testHypotMany([greater])
  testHypotMany([minIntFloat])
  testHypotMany([maxIntFloat])
  testHypotMany([minValue])
  testHypotMany([maxValue])
  testHypotMany([greater, less])
  testHypotMany([value, less, greater])
  testHypotMany([value, minValue, maxIntFloat])
  testHypotMany([value, maxValue, minIntFloat])
  testHypotMany([value, less, greater, -1.0, 0.0, -0.0, 1.0])
  loop100Fn(() =>
    testHypotMany([randomValue(), randomValue(), randomValue(), randomValue(), randomValue()])
  )

  let testLog = f => {
    let n = fromFloatExn(f)
    if f >= 0.0 {
      let result = fromFloatExn(Math.log(f))
      t->Assert.deepEqual(FloatModule.log(n), Some(result), ())
      t->Assert.deepEqual(FloatModule.logExn(n), result, ())
      t->Assert.deepEqual(FloatModule.logUnsafe(n), result, ())
    } else {
      t->Assert.deepEqual(FloatModule.log(n), None, ())
      t->assertInvalidArgument(() => FloatModule.logExn(n))
    }
  }

  testFn(testLog)
  loop100Fn(() => testLog(randomFloat(0.0, maxIntFloat *. 2.0)))

  let testLog10 = f => {
    let n = fromFloatExn(f)
    if f >= 0.0 {
      let result = fromFloatExn(Math.log10(f))
      t->Assert.deepEqual(FloatModule.log10(n), Some(result), ())
      t->Assert.deepEqual(FloatModule.log10Exn(n), result, ())
      t->Assert.deepEqual(FloatModule.log10Unsafe(n), result, ())
    } else {
      t->Assert.deepEqual(FloatModule.log10(n), None, ())
      t->assertInvalidArgument(() => FloatModule.log10Exn(n))
    }
  }

  testFn(testLog10)
  loop100Fn(() => testLog10(randomFloat(0.0, maxIntFloat *. 2.0)))

  let testLog1p = f => {
    let n = fromFloatExn(f)
    if f >= -1.0 {
      let result = fromFloatExn(Math.log1p(f))
      t->Assert.deepEqual(FloatModule.log1p(n), Some(result), ())
      t->Assert.deepEqual(FloatModule.log1pExn(n), result, ())
      t->Assert.deepEqual(FloatModule.log1pUnsafe(n), result, ())
    } else {
      t->Assert.deepEqual(FloatModule.log1p(n), None, ())
      t->assertInvalidArgument(() => FloatModule.log1pExn(n))
    }
  }

  testFn(testLog1p)
  loop100Fn(() => testLog1p(randomFloat(-1.0, maxIntFloat *. 2.0)))

  let testLog2 = f => {
    let n = fromFloatExn(f)
    if f >= 0.0 {
      let result = fromFloatExn(Math.log2(f))
      t->Assert.deepEqual(FloatModule.log2(n), Some(result), ())
      t->Assert.deepEqual(FloatModule.log2Exn(n), result, ())
      t->Assert.deepEqual(FloatModule.log2Unsafe(n), result, ())
    } else {
      t->Assert.deepEqual(FloatModule.log2(n), None, ())
      t->assertInvalidArgument(() => FloatModule.log2Exn(n))
    }
  }

  testFn(testLog2)
  loop100Fn(() => testLog2(randomFloat(0.0, maxIntFloat *. 2.0)))

  let testPowNotNaN = (a, b) => {
    if a >= 0.0 || b->isInteger {
      let result = fromFloatExn(Math.pow_float(~base=a, ~exp=b))
      let base = fromFloatExn(a)
      let exp = fromFloatExn(b)
      t->Assert.deepEqual(FloatModule.pow(~base, ~exp), Some(result), ())
      t->Assert.deepEqual(FloatModule.powExn(~base, ~exp), result, ())
      t->Assert.deepEqual(FloatModule.powUnsafe(~base, ~exp), result, ())
    }

    if b >= 0.0 || a->isInteger {
      let result = fromFloatExn(Math.pow_float(~base=b, ~exp=a))
      let base = fromFloatExn(b)
      let exp = fromFloatExn(a)
      t->Assert.deepEqual(FloatModule.pow(~base, ~exp), Some(result), ())
      t->Assert.deepEqual(FloatModule.powExn(~base, ~exp), result, ())
      t->Assert.deepEqual(FloatModule.powUnsafe(~base, ~exp), result, ())
    }

    let base = fromFloatExn(a)
    t->Assert.deepEqual(FloatModule.pow(~base, ~exp=zero), Some(one), ())
    t->Assert.deepEqual(FloatModule.powExn(~base, ~exp=zero), one, ())
    t->Assert.deepEqual(FloatModule.powUnsafe(~base, ~exp=zero), one, ())
    t->Assert.deepEqual(FloatModule.pow(~base, ~exp=one), Some(base), ())
    t->Assert.deepEqual(FloatModule.powExn(~base, ~exp=one), base, ())
    t->Assert.deepEqual(FloatModule.powUnsafe(~base, ~exp=one), base, ())

    let result = fromFloatExn(1.0 /. a)
    t->Assert.deepEqual(FloatModule.pow(~base, ~exp=negOne), Some(result), ())
    t->Assert.deepEqual(FloatModule.powExn(~base, ~exp=negOne), result, ())
    t->Assert.deepEqual(FloatModule.powUnsafe(~base, ~exp=negOne), result, ())

    let base = fromFloatExn(b)
    t->Assert.deepEqual(FloatModule.pow(~base, ~exp=zero), Some(one), ())
    t->Assert.deepEqual(FloatModule.powExn(~base, ~exp=zero), one, ())
    t->Assert.deepEqual(FloatModule.powUnsafe(~base, ~exp=zero), one, ())
    t->Assert.deepEqual(FloatModule.pow(~base, ~exp=one), Some(base), ())
    t->Assert.deepEqual(FloatModule.powExn(~base, ~exp=one), base, ())
    t->Assert.deepEqual(FloatModule.powUnsafe(~base, ~exp=one), base, ())

    let result = fromFloatExn(1.0 /. b)
    t->Assert.deepEqual(FloatModule.pow(~base, ~exp=negOne), Some(result), ())
    t->Assert.deepEqual(FloatModule.powExn(~base, ~exp=negOne), result, ())
    t->Assert.deepEqual(FloatModule.powUnsafe(~base, ~exp=negOne), result, ())
  }

  testPowNotNaN(0.0, -0.0)
  testPowNotNaN(1.0, -1.0)
  testPowNotNaN(0.0, value)
  testPowNotNaN(1.0, value)
  testPowNotNaN(value, minIntFloat)
  testPowNotNaN(value, maxIntFloat)
  testPowNotNaN(value, minValue)
  testPowNotNaN(value, maxValue)
  testPowNotNaN(minValue, maxValue)
  loop100Fn(() => testPowNotNaN(randomValue(), randomValue()))

  let testPowNaN = (base, exp) => {
    t->Assert.deepEqual(FloatModule.pow(~base, ~exp), None, ())
    t->assertInvalidArgument(() => FloatModule.powExn(~base, ~exp))
  }

  loop100Fn(() => testPowNaN(fromFloatExn(randomValue()), FloatModule.fromFloatUnsafe(nan)))
  loop100Fn(() => {
    let exp = randomValue()
    if exp !== 0.0 {
      testPowNaN(FloatModule.fromFloatUnsafe(nan), fromFloatExn(exp))
    }
  })
  testPowNaN(one, FloatModule.minValue)
  testPowNaN(one, FloatModule.maxValue)
  testPowNaN(negOne, FloatModule.minValue)
  testPowNaN(negOne, FloatModule.maxValue)
  loop100Fn(() => {
    let exp = randomValue()
    if !(exp->isInteger) {
      testPowNaN(fromFloatExn(randomFloat(minIntFloat *. 2.0, 0.0)), fromFloatExn(exp))
    }
  })

  let testRandom = () => {
    let value = FloatModule.random()
    t->Assert.isTrue(value->FloatModule.ge(zero), ())
    t->Assert.isTrue(value->FloatModule.lt(one), ())

    let m = randomValue()
    let n = randomValue()
    let min = fromFloatExn(m < n ? m : n)
    let max = fromFloatExn(m > n ? m : n)

    if min !== max {
      loopFn(() => {
        let i = FloatModule.randomRange(min, max)
        t->Assert.isTrue(i->FloatModule.ge(min), ())
        t->Assert.isTrue(i->FloatModule.lt(max), ())
      }, 10)
    }
  }

  loop100Fn(() => testRandom())

  let testRound = f => {
    let test_ = f => {
      let n = fromFloatExn(f)
      let result = f->Math.round
      t->Assert.deepEqual(n->FloatModule.round, fromFloatExn(result), ())
      if f >= minIntFloat -. 0.5 && f < maxIntFloat +. 0.5 {
        let result = result->Float.toInt
        t->Assert.deepEqual(n->FloatModule.roundInt, Some(result), ())
        t->Assert.deepEqual(n->FloatModule.roundIntExn, result, ())
        t->Assert.deepEqual(n->FloatModule.roundIntClamped, result, ())
        t->Assert.deepEqual(n->FloatModule.roundIntUnsafe, result, ())
      } else {
        t->Assert.deepEqual(n->FloatModule.roundInt, None, ())
        t->assertOverflow(() => n->FloatModule.roundIntExn)
        t->Assert.deepEqual(n->FloatModule.roundIntClamped, f->clampFloatToInt, ())
      }
    }

    test_(f)
    test_(-.f)
  }

  testFn(testRound)
  testRound(minIntFloat -. 0.1)
  testRound(minIntFloat -. 0.5)
  testRound(minIntFloat -. 0.6)
  testRound(maxIntFloat +. 0.1)
  testRound(maxIntFloat +. 0.5)
  testRound(maxIntFloat +. 0.6)
  loop100Fn(() => testRound(randomFloatInInt32Range()))

  let testSin = f => {
    let n = fromFloatExn(f)
    if f->isFinite {
      let result = fromFloatExn(Math.sin(f))
      t->Assert.deepEqual(FloatModule.sin(n), Some(result), ())
      t->Assert.deepEqual(FloatModule.sinExn(n), result, ())
      t->Assert.deepEqual(FloatModule.sinUnsafe(n), result, ())
    } else {
      t->Assert.deepEqual(FloatModule.sin(n), None, ())
      t->assertInvalidArgument(() => FloatModule.sinExn(n))
    }
  }

  testFn(testSin)

  let testSinh = f => {
    t->Assert.deepEqual(FloatModule.sinh(fromFloatExn(f)), fromFloatExn(Math.sinh(f)), ())
    t->Assert.deepEqual(FloatModule.sinh(fromFloatExn(-.f)), fromFloatExn(Math.sinh(-.f)), ())
  }

  testFn(testSinh)

  let testSqrt = f => {
    let n = fromFloatExn(f)
    if f >= 0.0 {
      let result = fromFloatExn(Math.sqrt(f))
      t->Assert.deepEqual(FloatModule.sqrt(n), Some(result), ())
      t->Assert.deepEqual(FloatModule.sqrtExn(n), result, ())
      t->Assert.deepEqual(FloatModule.sqrtUnsafe(n), result, ())
    } else {
      t->Assert.deepEqual(FloatModule.sqrt(n), None, ())
      t->assertInvalidArgument(() => FloatModule.sqrtExn(n))
    }
  }

  testFn(testSqrt)
  loop100Fn(() => testSqrt(randomFloat(0.0, maxIntFloat *. 2.0)))

  let testTan = f => {
    let n = fromFloatExn(f)
    if f->isFinite {
      let result = fromFloatExn(Math.tan(f))
      t->Assert.deepEqual(FloatModule.tan(n), Some(result), ())
      t->Assert.deepEqual(FloatModule.tanExn(n), result, ())
      t->Assert.deepEqual(FloatModule.tanUnsafe(n), result, ())
    } else {
      t->Assert.deepEqual(FloatModule.tan(n), None, ())
      t->assertInvalidArgument(() => FloatModule.tanExn(n))
    }
  }

  testFn(testTan)

  let testTanh = f => {
    t->Assert.deepEqual(FloatModule.tanh(fromFloatExn(f)), fromFloatExn(Math.tanh(f)), ())
    t->Assert.deepEqual(FloatModule.tanh(fromFloatExn(-.f)), fromFloatExn(Math.tanh(-.f)), ())
  }

  testFn(testTanh)

  let testTrunc = f => {
    let test_ = f => {
      let n = fromFloatExn(f)
      let result = f->Math.trunc
      t->Assert.deepEqual(n->FloatModule.trunc, fromFloatExn(result), ())
      if f > minIntFloat -. 1.0 && f < maxIntFloat +. 1.0 {
        let result = result->Float.toInt
        t->Assert.deepEqual(n->FloatModule.truncInt, Some(result), ())
        t->Assert.deepEqual(n->FloatModule.truncIntExn, result, ())
        t->Assert.deepEqual(n->FloatModule.truncIntClamped, result, ())
        t->Assert.deepEqual(n->FloatModule.truncIntUnsafe, result, ())
      } else {
        t->Assert.deepEqual(n->FloatModule.truncInt, None, ())
        t->assertOverflow(() => n->FloatModule.truncIntExn)
        t->Assert.deepEqual(n->FloatModule.truncIntClamped, f->clampFloatToInt, ())
      }
    }

    test_(f)
    test_(-.f)
  }

  testFn(testTrunc)
  testTrunc(minIntFloat -. 0.1)
  testTrunc(minIntFloat -. 0.5)
  testTrunc(minIntFloat -. 0.9)
  testTrunc(minIntFloat -. 1.0)
  testTrunc(maxIntFloat +. 0.1)
  testTrunc(maxIntFloat +. 0.5)
  testTrunc(maxIntFloat +. 0.9)
  testTrunc(maxIntFloat +. 1.0)
  loop100Fn(() => testTrunc(randomFloatInInt32Range()))
})

test(`test ${name} float extra`, t => {
  let testIsFinite = (f, result) =>
    t->Assert.deepEqual(fromFloatExn(f)->FloatModule.isFinite, result, ())

  testIsFinite(0.0, true)
  testIsFinite(-0.0, true)
  testIsFinite(1.0, true)
  testIsFinite(-1.0, true)
  testIsFinite(1e55, true)
  testIsFinite(-1e55, true)
  testIsFinite(value, true)
  testIsFinite(minIntFloat, true)
  testIsFinite(maxIntFloat, true)
  testIsFinite(minValue, false)
  testIsFinite(maxValue, false)
  loop100Fn(() => testIsFinite(randomValue(), true))
  t->Assert.deepEqual(FloatModule.fromFloatUnsafe(nan)->FloatModule.isFinite, false, ())

  let testIsInteger = (f, result) =>
    t->Assert.deepEqual(fromFloatExn(f)->FloatModule.isInteger, result, ())

  testIsInteger(0.0, true)
  testIsInteger(-0.0, true)
  testIsInteger(1.0, true)
  testIsInteger(-1.0, true)
  testIsInteger(1e55, true)
  testIsInteger(-1e55, true)
  testIsInteger(value, false)
  testIsInteger(minIntFloat, true)
  testIsInteger(maxIntFloat, true)
  testIsInteger(minValue, false)
  testIsInteger(maxValue, false)
  loop100Fn(() => testIsInteger(randomInt32()->Int.toFloat, true))
  loop100Fn(() => testIsInteger(randomSmallerInt(), true))
  loop100Fn(() => testIsInteger(randomLargerInt(), true))

  let testIsNotNaN = f => t->Assert.deepEqual(fromFloatExn(f)->FloatModule.isNaN, false, ())

  testFn(testIsNotNaN)
  t->Assert.deepEqual(FloatModule.fromFloatUnsafe(nan)->FloatModule.isNaN, true, ())

  let testIsSafeInteger = (f, result) =>
    t->Assert.deepEqual(fromFloatExn(f)->FloatModule.isSafeInteger, result, ())

  testIsSafeInteger(0.0, true)
  testIsSafeInteger(-0.0, true)
  testIsSafeInteger(1.0, true)
  testIsSafeInteger(-1.0, true)
  testIsSafeInteger(1e55, false)
  testIsSafeInteger(-1e55, false)
  testIsSafeInteger(value, false)
  testIsSafeInteger(minIntFloat, true)
  testIsSafeInteger(maxIntFloat, true)
  testIsSafeInteger(minValue, false)
  testIsSafeInteger(maxValue, false)
  loop100Fn(() => testIsSafeInteger(randomInt32()->Int.toFloat, true))
})
