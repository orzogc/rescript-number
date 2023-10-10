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

let randomValue = () => randomFloat(minIntFloat *. 2.0, maxIntFloat *. 2.0)

let randomInt32 = () => randomInt(minIntFloat, maxIntFloat)

let randomLargerInt = () => randomFloat(maxIntFloat, maxSafeInteger)->Js.Math.floor_float

let randomSmallerInt = () => randomFloat(minSafeInteger, minIntFloat)->Js.Math.floor_float

let randomFloatInInt32Range = () => randomFloat(minIntFloat, maxIntFloat)

test(`test ${name} from int`, t => {
  let testFromInt = i => {
    let n = FloatModule.fromIntExn(i)
    t->Assert.deepEqual(FloatModule.fromInt(i), Some(n), ())
    t->Assert.deepEqual(FloatModule.fromIntExn(i), n, ())
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
    let n = FloatModule.fromIntExn(i)
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
    let n = FloatModule.fromFloatExn(f)
    t->Assert.deepEqual(n->FloatModule.toInt, None, ())
    t->assertOverflow(() => n->FloatModule.toIntExn)
    t->Assert.deepEqual(n->FloatModule.toIntClamped, value, ())
  }

  testOutOfRange(minIntFloat -. 1.0, minInt)
  testOutOfRange(maxIntFloat +. 1.0, maxInt)
  loop100Fn(() => testOutOfRange(randomSmallerInt(), minInt))
  loop100Fn(() => testOutOfRange(randomLargerInt(), maxInt))

  let testIsNotInteger = (f, value) => {
    let n = FloatModule.fromFloatExn(f)
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
  let testNotNan = f => {
    let n = FloatModule.fromFloatExn(f)
    t->Assert.deepEqual(FloatModule.fromFloat(f), Some(n), ())
    t->Assert.deepEqual(FloatModule.fromFloatExn(f), n, ())
    t->Assert.deepEqual(FloatModule.fromFloatClamped(f), n, ())
    t->Assert.deepEqual(FloatModule.fromFloatUnsafe(f), n, ())
  }

  testNotNan(0.0)
  testNotNan(-0.0)
  testNotNan(1.0)
  testNotNan(value)
  testNotNan(minIntFloat)
  testNotNan(maxIntFloat)
  testNotNan(minValue)
  testNotNan(maxValue)
  loop100Fn(() => testNotNan(randomValue()))

  t->Assert.deepEqual(FloatModule.fromFloat(nan), None, ())
  t->assertInvalidArgument(() => FloatModule.fromFloatExn(nan))
  t->Assert.deepEqual(FloatModule.fromFloatClamped(nan), FloatModule.zero, ())
})

test(`test ${name} to float`, t => {
  let testToFloat = f => {
    let n = FloatModule.fromFloatExn(f)
    t->Assert.deepEqual(n->FloatModule.toFloat, f, ())
  }

  testToFloat(0.0)
  testToFloat(-0.0)
  testToFloat(1.0)
  testToFloat(value)
  testToFloat(minIntFloat)
  testToFloat(maxIntFloat)
  testToFloat(minValue)
  testToFloat(maxValue)
  loop100Fn(() => testToFloat(randomValue()))
})

test(`test ${name} from string`, t => {
  let testNotNan = (f, s) => {
    let n = FloatModule.fromFloatExn(f)
    t->Assert.deepEqual(FloatModule.fromString(s), Some(n), ())
    t->Assert.deepEqual(FloatModule.fromStringExn(s), n, ())
  }

  testNotNan(0.0, "0")
  testNotNan(-0.0, "-0")
  testNotNan(1.0, "1")
  testNotNan(value, value->Float.toString)
  testNotNan(minIntFloat, minIntFloat->Float.toString)
  testNotNan(maxIntFloat, maxIntFloat->Float.toString)
  testNotNan(minValue, minValue->Float.toString)
  testNotNan(maxValue, maxValue->Float.toString)
  loop100Fn(() => {
    let n = randomValue()
    testNotNan(n, n->Float.toString)
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
    t->Assert.deepEqual(FloatModule.fromFloatExn(f)->FloatModule.toString, s, ())
  }

  testToString(0.0)
  testToString(-0.0)
  testToString(1.0)
  testToString(value)
  testToString(minIntFloat)
  testToString(maxIntFloat)
  testToString(minValue)
  testToString(maxValue)
  loop100Fn(() => testToString(randomValue()))
})

test(`test ${name} zero`, t => {
  t->Assert.deepEqual(FloatModule.zero, FloatModule.fromIntExn(0), ())
  t->Assert.deepEqual(FloatModule.zero, FloatModule.fromFloatExn(0.0), ())
  t->Assert.notDeepEqual(FloatModule.zero, FloatModule.fromFloatExn(-0.0), ())
  t->Assert.isTrue(FloatModule.zero->FloatModule.eq(FloatModule.fromFloatExn(0.0)), ())
})

test(`test ${name} one`, t => {
  t->Assert.deepEqual(FloatModule.one, FloatModule.fromIntExn(1), ())
  t->Assert.deepEqual(FloatModule.one, FloatModule.fromFloatExn(1.0), ())
})

test(`test ${name} range`, t => {
  t->Assert.deepEqual(FloatModule.minValue, FloatModule.fromFloatExn(minValue), ())
  t->Assert.deepEqual(FloatModule.maxValue, FloatModule.fromFloatExn(maxValue), ())
})

test(`test ${name} comparison`, t => {
  let testCompare = (less, greater) => {
    let less = FloatModule.fromFloatExn(less)
    let greater = FloatModule.fromFloatExn(greater)
    let value = FloatModule.fromFloatExn(value)
    let minValue = FloatModule.fromFloatExn(minValue)
    let maxValue = FloatModule.fromFloatExn(maxValue)

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

  let zero = FloatModule.fromFloatExn(0.0)
  let negZero = FloatModule.fromFloatExn(-0.0)
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
  let testNotNan = (a, b) => {
    if a->isFinite {
      let negA = FloatModule.fromFloatExn(-.a)
      let a = FloatModule.fromFloatExn(a)
      t->Assert.deepEqual(a->FloatModule.add(negA), Some(FloatModule.zero), ())
      t->Assert.deepEqual(a->FloatModule.add(negA), negA->FloatModule.add(a), ())
      t->Assert.deepEqual(a->FloatModule.addExn(negA), FloatModule.zero, ())
      t->Assert.deepEqual(a->FloatModule.addExn(negA), negA->FloatModule.addExn(a), ())
      t->Assert.deepEqual(a->FloatModule.addClamped(negA), FloatModule.zero, ())
      t->Assert.deepEqual(a->FloatModule.addClamped(negA), negA->FloatModule.addClamped(a), ())
      t->Assert.deepEqual(a->FloatModule.addUnsafe(negA), FloatModule.zero, ())
      t->Assert.deepEqual(a->FloatModule.addUnsafe(negA), negA->FloatModule.addUnsafe(a), ())
    }

    if b->isFinite {
      let negB = FloatModule.fromFloatExn(-.b)
      let b = FloatModule.fromFloatExn(b)
      t->Assert.deepEqual(b->FloatModule.add(negB), Some(FloatModule.zero), ())
      t->Assert.deepEqual(b->FloatModule.add(negB), negB->FloatModule.add(b), ())
      t->Assert.deepEqual(b->FloatModule.addExn(negB), FloatModule.zero, ())
      t->Assert.deepEqual(b->FloatModule.addExn(negB), negB->FloatModule.addExn(b), ())
      t->Assert.deepEqual(b->FloatModule.addClamped(negB), FloatModule.zero, ())
      t->Assert.deepEqual(b->FloatModule.addClamped(negB), negB->FloatModule.addClamped(b), ())
      t->Assert.deepEqual(b->FloatModule.addUnsafe(negB), FloatModule.zero, ())
      t->Assert.deepEqual(b->FloatModule.addUnsafe(negB), negB->FloatModule.addUnsafe(b), ())
    }

    let result = FloatModule.fromFloatExn(a +. b)
    let a = FloatModule.fromFloatExn(a)
    let b = FloatModule.fromFloatExn(b)

    t->Assert.deepEqual(a->FloatModule.add(b), Some(result), ())
    t->Assert.deepEqual(a->FloatModule.add(b), b->FloatModule.add(a), ())
    t->Assert.deepEqual(a->FloatModule.addExn(b), result, ())
    t->Assert.deepEqual(a->FloatModule.addExn(b), b->FloatModule.addExn(a), ())
    t->Assert.deepEqual(a->FloatModule.addClamped(b), result, ())
    t->Assert.deepEqual(a->FloatModule.addClamped(b), b->FloatModule.addClamped(a), ())
    t->Assert.deepEqual(a->FloatModule.addUnsafe(b), result, ())
    t->Assert.deepEqual(a->FloatModule.addUnsafe(b), b->FloatModule.addUnsafe(a), ())

    let result = FloatModule.fromFloatExn(a->FloatModule.toFloat +. 0.0)
    t->Assert.deepEqual(a->FloatModule.add(FloatModule.zero), Some(result), ())
    t->Assert.deepEqual(
      a->FloatModule.add(FloatModule.zero),
      FloatModule.zero->FloatModule.add(a),
      (),
    )
    t->Assert.deepEqual(a->FloatModule.addExn(FloatModule.zero), result, ())
    t->Assert.deepEqual(
      a->FloatModule.addExn(FloatModule.zero),
      FloatModule.zero->FloatModule.addExn(a),
      (),
    )
    t->Assert.deepEqual(a->FloatModule.addClamped(FloatModule.zero), result, ())
    t->Assert.deepEqual(
      a->FloatModule.addClamped(FloatModule.zero),
      FloatModule.zero->FloatModule.addClamped(a),
      (),
    )
    t->Assert.deepEqual(a->FloatModule.addUnsafe(FloatModule.zero), result, ())
    t->Assert.deepEqual(
      a->FloatModule.addUnsafe(FloatModule.zero),
      FloatModule.zero->FloatModule.addUnsafe(a),
      (),
    )

    let result = FloatModule.fromFloatExn(b->FloatModule.toFloat +. 0.0)
    t->Assert.deepEqual(b->FloatModule.add(FloatModule.zero), Some(result), ())
    t->Assert.deepEqual(
      b->FloatModule.add(FloatModule.zero),
      FloatModule.zero->FloatModule.add(b),
      (),
    )
    t->Assert.deepEqual(b->FloatModule.addExn(FloatModule.zero), result, ())
    t->Assert.deepEqual(
      b->FloatModule.addExn(FloatModule.zero),
      FloatModule.zero->FloatModule.addExn(b),
      (),
    )
    t->Assert.deepEqual(b->FloatModule.addClamped(FloatModule.zero), result, ())
    t->Assert.deepEqual(
      b->FloatModule.addClamped(FloatModule.zero),
      FloatModule.zero->FloatModule.addClamped(b),
      (),
    )
    t->Assert.deepEqual(b->FloatModule.addUnsafe(FloatModule.zero), result, ())
    t->Assert.deepEqual(
      b->FloatModule.addUnsafe(FloatModule.zero),
      FloatModule.zero->FloatModule.addUnsafe(b),
      (),
    )
  }

  testNotNan(0.0, -0.0)
  testNotNan(less, greater)
  testNotNan(minIntFloat, -1.0)
  testNotNan(maxIntFloat, 1.0)
  testNotNan(minValue, 0.0)
  testNotNan(minValue, -0.0)
  testNotNan(minValue, 1.0)
  testNotNan(minValue, -1.0)
  testNotNan(minValue, minValue)
  testNotNan(maxValue, 0.0)
  testNotNan(maxValue, -0.0)
  testNotNan(maxValue, 1.0)
  testNotNan(maxValue, -1.0)
  testNotNan(maxValue, maxValue)
  loop100Fn(() => {
    let a = randomValue()
    let b = randomValue()
    testNotNan(a, b)
  })

  let testNan = (a, b) => {
    let a = FloatModule.fromFloatExn(a)
    let b = FloatModule.fromFloatExn(b)
    t->Assert.deepEqual(a->FloatModule.add(b), None, ())
    t->Assert.deepEqual(b->FloatModule.add(a), None, ())
    t->assertInvalidArgument(() => a->FloatModule.addExn(b))
    t->assertInvalidArgument(() => b->FloatModule.addExn(a))
    t->Assert.deepEqual(a->FloatModule.addClamped(b), FloatModule.zero, ())
    t->Assert.deepEqual(b->FloatModule.addClamped(a), FloatModule.zero, ())
  }

  testNan(maxValue, minValue)
})

test(`test ${name} subtraction`, t => {
  let testNotNan = (a, b) => {
    if a->isFinite {
      let a = FloatModule.fromFloatExn(a)
      t->Assert.deepEqual(a->FloatModule.sub(a), Some(FloatModule.zero), ())
      t->Assert.deepEqual(a->FloatModule.subExn(a), FloatModule.zero, ())
      t->Assert.deepEqual(a->FloatModule.subClamped(a), FloatModule.zero, ())
      t->Assert.deepEqual(a->FloatModule.subUnsafe(a), FloatModule.zero, ())
    }

    if b->isFinite {
      let b = FloatModule.fromFloatExn(b)
      t->Assert.deepEqual(b->FloatModule.sub(b), Some(FloatModule.zero), ())
      t->Assert.deepEqual(b->FloatModule.subExn(b), FloatModule.zero, ())
      t->Assert.deepEqual(b->FloatModule.subClamped(b), FloatModule.zero, ())
      t->Assert.deepEqual(b->FloatModule.subUnsafe(b), FloatModule.zero, ())
    }

    let result = FloatModule.fromFloatExn(a -. b)
    let a = FloatModule.fromFloatExn(a)
    let b = FloatModule.fromFloatExn(b)
    t->Assert.deepEqual(a->FloatModule.sub(b), Some(result), ())
    t->Assert.deepEqual(a->FloatModule.subExn(b), result, ())
    t->Assert.deepEqual(a->FloatModule.subClamped(b), result, ())
    t->Assert.deepEqual(a->FloatModule.subUnsafe(b), result, ())

    t->Assert.deepEqual(a->FloatModule.sub(FloatModule.zero), Some(a), ())
    t->Assert.deepEqual(a->FloatModule.subExn(FloatModule.zero), a, ())
    t->Assert.deepEqual(a->FloatModule.subClamped(FloatModule.zero), a, ())
    t->Assert.deepEqual(a->FloatModule.subUnsafe(FloatModule.zero), a, ())

    t->Assert.deepEqual(b->FloatModule.sub(FloatModule.zero), Some(b), ())
    t->Assert.deepEqual(b->FloatModule.subExn(FloatModule.zero), b, ())
    t->Assert.deepEqual(b->FloatModule.subClamped(FloatModule.zero), b, ())
    t->Assert.deepEqual(b->FloatModule.subUnsafe(FloatModule.zero), b, ())
  }

  testNotNan(0.0, -0.0)
  testNotNan(less, greater)
  testNotNan(greater, less)
  testNotNan(minIntFloat, 1.0)
  testNotNan(maxIntFloat, -1.0)
  testNotNan(minValue, 0.0)
  testNotNan(minValue, -0.0)
  testNotNan(minValue, 1.0)
  testNotNan(minValue, -1.0)
  testNotNan(minValue, value)
  testNotNan(maxValue, 0.0)
  testNotNan(maxValue, -0.0)
  testNotNan(maxValue, 1.0)
  testNotNan(maxValue, -1.0)
  testNotNan(maxValue, value)
  testNotNan(minValue, maxValue)
  testNotNan(maxValue, minValue)
  testNotNan(0.0, minValue)
  testNotNan(-0.0, minValue)
  testNotNan(1.0, minValue)
  testNotNan(-1.0, minValue)
  testNotNan(value, minValue)
  testNotNan(0.0, maxValue)
  testNotNan(-0.0, maxValue)
  testNotNan(1.0, maxValue)
  testNotNan(-1.0, maxValue)
  testNotNan(value, maxValue)
  loop100Fn(() => {
    let a = randomValue()
    let b = randomValue()
    testNotNan(a, b)
  })

  let testNan = (a, b) => {
    let a = FloatModule.fromFloatExn(a)
    let b = FloatModule.fromFloatExn(b)
    t->Assert.deepEqual(a->FloatModule.sub(b), None, ())
    t->assertInvalidArgument(() => a->FloatModule.subExn(b))
    t->Assert.deepEqual(a->FloatModule.subClamped(b), FloatModule.zero, ())
  }

  testNan(minValue, minValue)
  testNan(maxValue, maxValue)
})

test(`test ${name} multiplication`, t => {
  let testNotNan = (a, b) => {
    if a->isFinite {
      let result = FloatModule.fromFloatExn(a *. 0.0)
      let a = FloatModule.fromFloatExn(a)
      t->Assert.deepEqual(a->FloatModule.mul(FloatModule.zero), Some(result), ())
      t->Assert.deepEqual(
        a->FloatModule.mul(FloatModule.zero),
        FloatModule.zero->FloatModule.mul(a),
        (),
      )
      t->Assert.deepEqual(a->FloatModule.mulExn(FloatModule.zero), result, ())
      t->Assert.deepEqual(
        a->FloatModule.mulExn(FloatModule.zero),
        FloatModule.zero->FloatModule.mulExn(a),
        (),
      )
      t->Assert.deepEqual(a->FloatModule.mulClamped(FloatModule.zero), result, ())
      t->Assert.deepEqual(
        a->FloatModule.mulClamped(FloatModule.zero),
        FloatModule.zero->FloatModule.mulClamped(a),
        (),
      )
      t->Assert.deepEqual(a->FloatModule.mulUnsafe(FloatModule.zero), result, ())
      t->Assert.deepEqual(
        a->FloatModule.mulUnsafe(FloatModule.zero),
        FloatModule.zero->FloatModule.mulUnsafe(a),
        (),
      )
    }

    if b->isFinite {
      let result = FloatModule.fromFloatExn(b *. 0.0)
      let b = FloatModule.fromFloatExn(b)
      t->Assert.deepEqual(b->FloatModule.mul(FloatModule.zero), Some(result), ())
      t->Assert.deepEqual(
        b->FloatModule.mul(FloatModule.zero),
        FloatModule.zero->FloatModule.mul(b),
        (),
      )
      t->Assert.deepEqual(b->FloatModule.mulExn(FloatModule.zero), result, ())
      t->Assert.deepEqual(
        b->FloatModule.mulExn(FloatModule.zero),
        FloatModule.zero->FloatModule.mulExn(b),
        (),
      )
      t->Assert.deepEqual(b->FloatModule.mulClamped(FloatModule.zero), result, ())
      t->Assert.deepEqual(
        b->FloatModule.mulClamped(FloatModule.zero),
        FloatModule.zero->FloatModule.mulClamped(b),
        (),
      )
      t->Assert.deepEqual(b->FloatModule.mulUnsafe(FloatModule.zero), result, ())
      t->Assert.deepEqual(
        b->FloatModule.mulUnsafe(FloatModule.zero),
        FloatModule.zero->FloatModule.mulUnsafe(b),
        (),
      )
    }

    let result = FloatModule.fromFloatExn(a *. b)
    let a = FloatModule.fromFloatExn(a)
    let b = FloatModule.fromFloatExn(b)
    t->Assert.deepEqual(a->FloatModule.mul(b), Some(result), ())
    t->Assert.deepEqual(a->FloatModule.mul(b), b->FloatModule.mul(a), ())
    t->Assert.deepEqual(a->FloatModule.mulExn(b), result, ())
    t->Assert.deepEqual(a->FloatModule.mulExn(b), b->FloatModule.mulExn(a), ())
    t->Assert.deepEqual(a->FloatModule.mulClamped(b), result, ())
    t->Assert.deepEqual(a->FloatModule.mulClamped(b), b->FloatModule.mulClamped(a), ())
    t->Assert.deepEqual(a->FloatModule.mulUnsafe(b), result, ())
    t->Assert.deepEqual(a->FloatModule.mulUnsafe(b), b->FloatModule.mulUnsafe(a), ())

    t->Assert.deepEqual(a->FloatModule.mul(FloatModule.one), Some(a), ())
    t->Assert.deepEqual(
      a->FloatModule.mul(FloatModule.one),
      FloatModule.one->FloatModule.mul(a),
      (),
    )
    t->Assert.deepEqual(a->FloatModule.mulExn(FloatModule.one), a, ())
    t->Assert.deepEqual(
      a->FloatModule.mulExn(FloatModule.one),
      FloatModule.one->FloatModule.mulExn(a),
      (),
    )
    t->Assert.deepEqual(a->FloatModule.mulClamped(FloatModule.one), a, ())
    t->Assert.deepEqual(
      a->FloatModule.mulClamped(FloatModule.one),
      FloatModule.one->FloatModule.mulClamped(a),
      (),
    )
    t->Assert.deepEqual(a->FloatModule.mulUnsafe(FloatModule.one), a, ())
    t->Assert.deepEqual(
      a->FloatModule.mulUnsafe(FloatModule.one),
      FloatModule.one->FloatModule.mulUnsafe(a),
      (),
    )

    t->Assert.deepEqual(b->FloatModule.mul(FloatModule.one), Some(b), ())
    t->Assert.deepEqual(
      b->FloatModule.mul(FloatModule.one),
      FloatModule.one->FloatModule.mul(b),
      (),
    )
    t->Assert.deepEqual(b->FloatModule.mulExn(FloatModule.one), b, ())
    t->Assert.deepEqual(
      b->FloatModule.mulExn(FloatModule.one),
      FloatModule.one->FloatModule.mulExn(b),
      (),
    )
    t->Assert.deepEqual(b->FloatModule.mulClamped(FloatModule.one), b, ())
    t->Assert.deepEqual(
      b->FloatModule.mulClamped(FloatModule.one),
      FloatModule.one->FloatModule.mulClamped(b),
      (),
    )
    t->Assert.deepEqual(b->FloatModule.mulUnsafe(FloatModule.one), b, ())
    t->Assert.deepEqual(
      b->FloatModule.mulUnsafe(FloatModule.one),
      FloatModule.one->FloatModule.mulUnsafe(b),
      (),
    )
  }

  testNotNan(0.0, -1.0)
  testNotNan(-0.0, 1.0)
  testNotNan(less, greater)
  testNotNan(minIntFloat, value)
  testNotNan(maxIntFloat, value)
  testNotNan(minValue, value)
  testNotNan(minValue, -.value)
  testNotNan(maxValue, value)
  testNotNan(maxValue, -.value)
  testNotNan(minValue, maxValue)
  loop100Fn(() => {
    let a = randomValue()
    let b = randomValue()
    testNotNan(a, b)
  })

  let testNan = (a, b) => {
    let a = FloatModule.fromFloatExn(a)
    let b = FloatModule.fromFloatExn(b)
    t->Assert.deepEqual(a->FloatModule.mul(b), None, ())
    t->Assert.deepEqual(b->FloatModule.mul(a), None, ())
    t->assertInvalidArgument(() => a->FloatModule.mulExn(b))
    t->assertInvalidArgument(() => b->FloatModule.mulExn(a))
    t->Assert.deepEqual(a->FloatModule.mulClamped(b), FloatModule.zero, ())
    t->Assert.deepEqual(b->FloatModule.mulClamped(a), FloatModule.zero, ())
  }

  testNan(minValue, 0.0)
  testNan(maxValue, 0.0)
})

test(`test ${name} division`, t => {
  let testNotNan = (a, b) => {
    if a !== 0.0 {
      let result = FloatModule.fromFloatExn(b /. a)
      let a = FloatModule.fromFloatExn(a)
      let b = FloatModule.fromFloatExn(b)
      t->Assert.deepEqual(b->FloatModule.div(a), Some(result), ())
      t->Assert.deepEqual(b->FloatModule.divExn(a), result, ())
      t->Assert.deepEqual(b->FloatModule.divUnsafe(a), result, ())

      let result = FloatModule.fromFloatExn(0.0 /. a->FloatModule.toFloat)
      t->Assert.deepEqual(FloatModule.zero->FloatModule.div(a), Some(result), ())
      t->Assert.deepEqual(FloatModule.zero->FloatModule.divExn(a), result, ())
      t->Assert.deepEqual(FloatModule.zero->FloatModule.divUnsafe(a), result, ())
    }

    if b !== 0.0 {
      let result = FloatModule.fromFloatExn(a /. b)
      let a = FloatModule.fromFloatExn(a)
      let b = FloatModule.fromFloatExn(b)
      t->Assert.deepEqual(a->FloatModule.div(b), Some(result), ())
      t->Assert.deepEqual(a->FloatModule.divExn(b), result, ())
      t->Assert.deepEqual(a->FloatModule.divUnsafe(b), result, ())

      let result = FloatModule.fromFloatExn(0.0 /. b->FloatModule.toFloat)
      t->Assert.deepEqual(FloatModule.zero->FloatModule.div(b), Some(result), ())
      t->Assert.deepEqual(FloatModule.zero->FloatModule.divExn(b), result, ())
      t->Assert.deepEqual(FloatModule.zero->FloatModule.divUnsafe(b), result, ())
    }

    let negA = FloatModule.fromFloatExn(-.a)
    let negB = FloatModule.fromFloatExn(-.b)
    let a = FloatModule.fromFloatExn(a)
    let b = FloatModule.fromFloatExn(b)
    t->Assert.deepEqual(a->FloatModule.div(FloatModule.one), Some(a), ())
    t->Assert.deepEqual(a->FloatModule.divExn(FloatModule.one), a, ())
    t->Assert.deepEqual(a->FloatModule.divUnsafe(FloatModule.one), a, ())
    t->Assert.deepEqual(b->FloatModule.div(FloatModule.one), Some(b), ())
    t->Assert.deepEqual(b->FloatModule.divExn(FloatModule.one), b, ())
    t->Assert.deepEqual(b->FloatModule.divUnsafe(FloatModule.one), b, ())

    let negOne = FloatModule.fromIntExn(-1)
    t->Assert.deepEqual(a->FloatModule.div(negOne), Some(negA), ())
    t->Assert.deepEqual(a->FloatModule.divExn(negOne), negA, ())
    t->Assert.deepEqual(a->FloatModule.divUnsafe(negOne), negA, ())
    t->Assert.deepEqual(b->FloatModule.div(negOne), Some(negB), ())
    t->Assert.deepEqual(b->FloatModule.divExn(negOne), negB, ())
    t->Assert.deepEqual(b->FloatModule.divUnsafe(negOne), negB, ())
  }

  testNotNan(less, greater)
  testNotNan(minIntFloat, maxIntFloat)
  testNotNan(minIntFloat, less)
  testNotNan(maxIntFloat, greater)
  testNotNan(minValue, less)
  testNotNan(minValue, greater)
  testNotNan(maxValue, less)
  testNotNan(maxValue, greater)

  let testDividedByZero = f => {
    let a = FloatModule.fromFloatExn(f)
    t->Assert.deepEqual(a->FloatModule.div(FloatModule.zero), None, ())
    t->assertDivisionByZero(() => a->FloatModule.divExn(FloatModule.zero))
    let negZero = FloatModule.fromFloatExn(-0.0)
    t->Assert.deepEqual(a->FloatModule.div(negZero), None, ())
    t->assertDivisionByZero(() => a->FloatModule.divExn(negZero))
  }

  testDividedByZero(0.0)
  testDividedByZero(-0.0)
  testDividedByZero(1.0)
  testDividedByZero(less)
  testDividedByZero(greater)
  testDividedByZero(value)
  testDividedByZero(minIntFloat)
  testDividedByZero(maxIntFloat)
  testDividedByZero(minValue)
  testDividedByZero(maxValue)

  loop100Fn(() => {
    let a = randomValue()
    let b = randomValue()
    testNotNan(a, b)
    testDividedByZero(a)
    testDividedByZero(b)
  })

  let testNan = (a, b) => {
    let a = FloatModule.fromFloatExn(a)
    let b = FloatModule.fromFloatExn(b)
    t->Assert.deepEqual(a->FloatModule.div(b), None, ())
    t->assertInvalidArgument(() => a->FloatModule.divExn(b))
  }

  testNan(minValue, maxValue)
  testNan(maxValue, minValue)
})

test(`test ${name} remainder`, t => {
  let testNotNan = (a, b) => {
    if a !== 0.0 {
      let result = FloatModule.fromFloatExn(b->mod_float(a))
      let a = FloatModule.fromFloatExn(a)
      let b = FloatModule.fromFloatExn(b)
      t->Assert.deepEqual(b->FloatModule.rem(a), Some(result), ())
      t->Assert.deepEqual(b->FloatModule.remExn(a), result, ())
      t->Assert.deepEqual(b->FloatModule.remUnsafe(a), result, ())
      t->Assert.deepEqual(FloatModule.zero->FloatModule.rem(a), Some(FloatModule.zero), ())
      t->Assert.deepEqual(FloatModule.zero->FloatModule.remExn(a), FloatModule.zero, ())
      t->Assert.deepEqual(FloatModule.zero->FloatModule.remUnsafe(a), FloatModule.zero, ())
    }

    if b !== 0.0 {
      let result = FloatModule.fromFloatExn(a->mod_float(b))
      let a = FloatModule.fromFloatExn(a)
      let b = FloatModule.fromFloatExn(b)
      t->Assert.deepEqual(a->FloatModule.rem(b), Some(result), ())
      t->Assert.deepEqual(a->FloatModule.remExn(b), result, ())
      t->Assert.deepEqual(a->FloatModule.remUnsafe(b), result, ())
      t->Assert.deepEqual(FloatModule.zero->FloatModule.rem(b), Some(FloatModule.zero), ())
      t->Assert.deepEqual(FloatModule.zero->FloatModule.remExn(b), FloatModule.zero, ())
      t->Assert.deepEqual(FloatModule.zero->FloatModule.remUnsafe(b), FloatModule.zero, ())
    }
  }

  testNotNan(less, greater)
  testNotNan(minIntFloat, maxIntFloat)
  testNotNan(minIntFloat, less)
  testNotNan(maxIntFloat, greater)

  let testModByZero = f => {
    let a = FloatModule.fromFloatExn(f)
    t->Assert.deepEqual(a->FloatModule.rem(FloatModule.zero), None, ())
    t->assertDivisionByZero(() => a->FloatModule.remExn(FloatModule.zero))
    let negZero = FloatModule.fromFloatExn(-0.0)
    t->Assert.deepEqual(a->FloatModule.rem(negZero), None, ())
    t->assertDivisionByZero(() => a->FloatModule.remExn(negZero))
  }

  testModByZero(0.0)
  testModByZero(-0.0)
  testModByZero(1.0)
  testModByZero(less)
  testModByZero(greater)
  testModByZero(value)
  testModByZero(minIntFloat)
  testModByZero(maxIntFloat)
  testModByZero(minValue)
  testModByZero(maxValue)

  loop100Fn(() => {
    let a = randomValue()
    let b = randomValue()
    testNotNan(a, b)
    testModByZero(a)
    testModByZero(b)
  })

  let testNan = (a, b) => {
    let a = FloatModule.fromFloatExn(a)
    let b = FloatModule.fromFloatExn(b)
    t->Assert.deepEqual(a->FloatModule.rem(b), None, ())
    t->assertInvalidArgument(() => a->FloatModule.remExn(b))
  }

  testNan(minValue, 1.0)
  testNan(minValue, value)
  testNan(minValue, minIntFloat)
  testNan(minValue, maxIntFloat)
  testNan(minValue, maxValue)
  testNan(maxValue, 1.0)
  testNan(maxValue, value)
  testNan(maxValue, minIntFloat)
  testNan(maxValue, maxIntFloat)
  testNan(maxValue, minValue)
})

test(`test ${name} sum`, t => {
  let testNotNan = arr => {
    let result = FloatModule.fromFloatExn(arr->Array.reduce(0.0, (acc, v) => acc +. v))
    let arr = arr->Array.mapU(f => FloatModule.fromFloatExn(f))
    t->Assert.deepEqual(arr->FloatModule.sum, Some(result), ())
    t->Assert.deepEqual(arr->FloatModule.sumExn, result, ())
    t->Assert.deepEqual(arr->FloatModule.sumUnsafe, result, ())
  }

  testNotNan([less])
  testNotNan([greater])
  testNotNan([minIntFloat])
  testNotNan([maxIntFloat])
  testNotNan([minValue])
  testNotNan([maxValue])
  testNotNan([less, greater])
  testNotNan([greater, less])
  testNotNan([less, minIntFloat, value, maxIntFloat, greater])
  loop100Fn(() => {
    let a = randomValue()
    let b = randomValue()
    testNotNan([a])
    testNotNan([b])
    testNotNan([a, b])
    testNotNan([b, a])
    testNotNan([a, value, maxIntFloat, minIntFloat, b])
  })

  let testNan = arr => {
    let arr = arr->Array.mapU(i => FloatModule.fromFloatExn(i))
    t->Assert.deepEqual(arr->FloatModule.sum, None, ())
    t->assertInvalidArgument(() => arr->FloatModule.sumExn)
  }

  testNan([minValue, maxValue])
  testNan([value, minValue, maxValue])
  testNan([minValue, maxValue, value])
})
