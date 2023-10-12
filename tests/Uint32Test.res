open Ava
open Belt

open TestUtils

module IntModule = Number.Uint32

let name = "Uint32"

let isSigned = false

let bits = 32

let minValue = 0.0

let maxValue = 4294967295.0

let value = 2000000000.0

let valueNot = 2294967295.0

let valueShiftLeft = [
  value,
  4000000000.0,
  3705032704.0,
  3115098112.0,
  1935228928.0,
  3870457856.0,
  3445948416.0,
  2596929536.0,
  898891776.0,
  1797783552.0,
  3595567104.0,
  2896166912.0,
  1497366528.0,
  2994733056.0,
  1694498816.0,
  3388997632.0,
  2483027968.0,
  671088640.0,
  1342177280.0,
  2684354560.0,
  1073741824.0,
  2147483648.0,
  0.0,
  0.0,
  0.0,
  0.0,
  0.0,
  0.0,
  0.0,
  0.0,
  0.0,
  0.0,
]

let valueRotateShiftLeft = [
  value,
  4000000000.0,
  3705032705.0,
  3115098115.0,
  1935228935.0,
  3870457870.0,
  3445948445.0,
  2596929595.0,
  898891895.0,
  1797783790.0,
  3595567580.0,
  2896167865.0,
  1497368435.0,
  2994736870.0,
  1694506445.0,
  3389012890.0,
  2483058485.0,
  671149675.0,
  1342299350.0,
  2684598700.0,
  1074230105.0,
  2148460210.0,
  1953125.0,
  3906250.0,
  7812500.0,
  15625000.0,
  31250000.0,
  62500000.0,
  125000000.0,
  250000000.0,
  500000000.0,
  1000000000.0,
]

let less = 27852.0

let greater = 56331.0

let lessGreaterAnd = 19464.0

let lessGreaterOr = 64719.0

let lessGreaterXor = 45255.0

let minInt = Js.Int.min

let maxInt = Js.Int.max

@warning("-27")
let toUint32 = (f: float): float => %raw(`f >>> 0`)

let randomValue = () => randomFloat(minValue, maxValue)->Js.Math.floor_float

let randomLargerFloat = () => randomFloat(maxInt->Int.toFloat +. 1.0, maxValue)->Js.Math.floor_float

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
  testInRange(value->Float.toInt)
  testInRange(maxInt)
  testInRange(maxInt - 1)
  loop100Fn(() => testInRange(randomPositiveInt()))

  let testOutOfRange = i => {
    t->Assert.deepEqual(IntModule.fromInt(i), None, ())
    t->assertOverflow(() => IntModule.fromIntExn(i))
    t->Assert.deepEqual(IntModule.fromIntClamped(i), IntModule.minValue, ())
  }

  testOutOfRange(-1)
  testOutOfRange(-.value->Float.toInt)
  testOutOfRange(minInt)
  testOutOfRange(minInt + 1)
  loop100Fn(() => testOutOfRange(randomNegativeInt()))
})

test(`test ${name} to int`, t => {
  let testInRange = i => {
    let n = IntModule.fromIntExn(i)
    t->Assert.deepEqual(n->IntModule.toInt, Some(i), ())
    t->Assert.deepEqual(n->IntModule.toIntExn, i, ())
    t->Assert.deepEqual(n->IntModule.toIntClamped, i, ())
    t->Assert.deepEqual(n->IntModule.toIntUnsafe, i, ())
  }

  testInRange(0)
  testInRange(1)
  testInRange(value->Float.toInt)
  testInRange(maxInt)
  testInRange(maxInt - 1)
  loop100Fn(() => testInRange(randomPositiveInt()))

  let testOutOfRange = f => {
    let n = IntModule.fromFloatExn(f)
    t->Assert.deepEqual(n->IntModule.toInt, None, ())
    t->assertOverflow(() => n->IntModule.toIntExn)
    t->Assert.deepEqual(n->IntModule.toIntClamped, maxInt, ())
  }

  testOutOfRange(maxInt->Int.toFloat +. 1.0)
  testOutOfRange(maxValue)
  loop100Fn(() => testOutOfRange(randomLargerFloat()))
})

test(`test ${name} from float`, t => {
  let testIsIntegerAndInRange = f => {
    let n = IntModule.fromFloatExn(f)
    t->Assert.deepEqual(IntModule.fromFloat(f), Some(n), ())
    t->Assert.deepEqual(IntModule.fromFloatExn(f), n, ())
    t->Assert.deepEqual(IntModule.fromFloatClamped(f), n, ())
    t->Assert.deepEqual(IntModule.fromFloatUnsafe(f), n, ())
  }

  testIsIntegerAndInRange(0.0)
  testIsIntegerAndInRange(-0.0)
  testIsIntegerAndInRange(1.0)
  testIsIntegerAndInRange(value)
  testIsIntegerAndInRange(minValue)
  testIsIntegerAndInRange(maxValue)
  testIsIntegerAndInRange(minValue +. 1.0)
  testIsIntegerAndInRange(maxValue -. 1.0)
  loop100Fn(() => testIsIntegerAndInRange(randomValue()))

  let testOutOfRange = (f, value) => {
    t->Assert.deepEqual(IntModule.fromFloat(f), None, ())
    t->assertOverflow(() => IntModule.fromFloatExn(f))
    t->Assert.deepEqual(IntModule.fromFloatClamped(f), IntModule.fromFloatExn(value), ())
  }

  testOutOfRange(minValue -. 1.0, minValue)
  testOutOfRange(maxValue +. 1.0, maxValue)
  testOutOfRange(minValue -. value, minValue)
  testOutOfRange(maxValue +. value, maxValue)
  loop100Fn(() =>
    testOutOfRange(randomFloat(maxValue +. 1.0, maxSafeInteger)->Js.Math.floor_float, maxValue)
  )
  loop100Fn(() =>
    testOutOfRange(randomFloat(minSafeInteger, minValue -. 1.0)->Js.Math.floor_float, minValue)
  )

  let testIsNotInteger = (f, value) => {
    t->Assert.deepEqual(IntModule.fromFloat(f), None, ())
    t->assertInvalidArgument(() => IntModule.fromFloatExn(f))
    t->Assert.deepEqual(IntModule.fromFloatClamped(f), IntModule.fromFloatExn(value), ())
  }

  testIsNotInteger(value +. 0.5, value)
  testIsNotInteger(nan, 0.0)
  testIsNotInteger(neg_infinity, minValue)
  testIsNotInteger(infinity, maxValue)
  testIsNotInteger(minValue +. 0.1, minValue)
  testIsNotInteger(minValue -. 0.1, minValue)
  testIsNotInteger(maxValue +. 0.1, maxValue)
  testIsNotInteger(maxValue -. 0.1, maxValue -. 1.0)
  testIsNotInteger(minValue -. value -. 0.5, minValue)
  testIsNotInteger(maxValue +. value +. 0.5, maxValue)
})

test(`test ${name} to float`, t => {
  let testToFloat = f => {
    t->Assert.deepEqual(IntModule.fromFloatExn(f)->IntModule.toFloat, f, ())
  }

  testToFloat(value)
  loop100Fn(() => testToFloat(randomValue()))
})

test(`test ${name} from string`, t => {
  let testInRange = (f, s) => {
    let n = IntModule.fromFloatExn(f)
    t->Assert.deepEqual(IntModule.fromString(s), Some(n), ())
    t->Assert.deepEqual(IntModule.fromStringExn(s), n, ())

    for radix in 2 to 36 {
      let s = f->Js.Float.toStringWithRadix(~radix)
      t->Assert.deepEqual(IntModule.fromStringWithRadix(s, ~radix), Some(n), ())
      t->Assert.deepEqual(IntModule.fromStringWithRadixExn(s, ~radix), n, ())
    }
    t->assertInvalidArgument(() => IntModule.fromStringWithRadixExn(s, ~radix=1))
    t->assertInvalidArgument(() => IntModule.fromStringWithRadixExn(s, ~radix=37))
  }

  testInRange(0.0, "0")
  testInRange(0.0, "-0")
  testInRange(1.0, "1")
  testInRange(value, value->Float.toString)
  testInRange(value, (value +. 0.5)->Float.toString)
  testInRange(minValue, minValue->Float.toString)
  testInRange(maxValue, maxValue->Float.toString)
  testInRange(minValue +. 1.0, (minValue +. 1.0)->Float.toString)
  testInRange(maxValue -. 1.0, (maxValue -. 1.0)->Float.toString)
  loop100Fn(() => {
    let value = randomValue()
    testInRange(value, value->Float.toString)
  })

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

  testOutOfRange(minValue -. 1.0)
  testOutOfRange(maxValue +. 1.0)
  testOutOfRange(minValue -. value)
  testOutOfRange(maxValue +. value)
  loop100Fn(() => testOutOfRange(randomNegativeInt()->Int.toFloat))

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
  let testFn = fn => {
    fn(0.0)
    fn(1.0)
    fn(value)
    fn(minValue)
    fn(minValue +. 1.0)
    fn(maxValue)
    fn(maxValue -. 1.0)
    loop100Fn(() => fn(randomValue()))
  }

  let testToString = f => {
    let s = f->Float.toString
    let n = IntModule.fromFloatExn(f)
    t->Assert.deepEqual(n->IntModule.toString, s, ())

    for radix in 2 to 36 {
      let s = f->Js.Float.toStringWithRadix(~radix)
      t->Assert.deepEqual(n->IntModule.toStringWithRadixExn(~radix), s, ())
    }
    t->Assert.throws(() => n->IntModule.toStringWithRadixExn(~radix=1), ())
    t->Assert.throws(() => n->IntModule.toStringWithRadixExn(~radix=37), ())
  }

  testFn(testToString)

  let testToExponential = f => {
    let s = f->Js.Float.toExponential
    let n = IntModule.fromFloatExn(f)
    t->Assert.deepEqual(n->IntModule.toExponential, s, ())

    for digits in 0 to 100 {
      let s = f->Js.Float.toExponentialWithPrecision(~digits)
      t->Assert.deepEqual(n->IntModule.toExponentialWithPrecisionExn(~digits), s, ())
    }
    t->Assert.throws(() => n->IntModule.toExponentialWithPrecisionExn(~digits=-1), ())
    t->Assert.throws(() => n->IntModule.toExponentialWithPrecisionExn(~digits=101), ())
  }

  testFn(testToExponential)

  let testToPrecision = f => {
    let s = f->Js.Float.toPrecision
    let n = IntModule.fromFloatExn(f)
    t->Assert.deepEqual(n->IntModule.toPrecision, s, ())

    for digits in 1 to 100 {
      let s = f->Js.Float.toPrecisionWithPrecision(~digits)
      t->Assert.deepEqual(n->IntModule.toPrecisionWithPrecisionExn(~digits), s, ())
    }
    t->Assert.throws(() => n->IntModule.toPrecisionWithPrecisionExn(~digits=0), ())
    t->Assert.throws(() => n->IntModule.toPrecisionWithPrecisionExn(~digits=101), ())
  }

  testFn(testToPrecision)
})

test(`test ${name} zero`, t => {
  t->Assert.deepEqual(IntModule.zero, IntModule.fromIntExn(0), ())
  t->Assert.deepEqual(IntModule.zero, IntModule.fromFloatExn(0.0), ())
  t->Assert.deepEqual(IntModule.zero, IntModule.fromFloatExn(-0.0), ())
})

test(`test ${name} one`, t => {
  t->Assert.deepEqual(IntModule.one, IntModule.fromIntExn(1), ())
  t->Assert.deepEqual(IntModule.one, IntModule.fromFloatExn(1.0), ())
})

test(`test ${name} range`, t => {
  t->Assert.deepEqual(IntModule.minValue, IntModule.fromIntExn(0), ())
  t->Assert.deepEqual(IntModule.minValue, IntModule.fromFloatExn(minValue), ())
  t->Assert.deepEqual(IntModule.maxValue, IntModule.fromFloatExn(maxValue), ())
})

test(`test ${name} comparison`, t => {
  let testCompare = (less, greater) => {
    let less = IntModule.fromFloatExn(less)
    let greater = IntModule.fromFloatExn(greater)
    let value = IntModule.fromFloatExn(value)
    let minValue = IntModule.fromFloatExn(minValue)
    let maxValue = IntModule.fromFloatExn(maxValue)

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
    let greater = randomFloat(less +. 1.0, maxValue +. 1.0)->Js.Math.floor_float
    testCompare(less, greater)
  })

  let zero = IntModule.fromFloatExn(0.0)
  let negZero = IntModule.fromFloatExn(-0.0)
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
    let result = IntModule.fromFloatExn(a +. b)
    let a = IntModule.fromFloatExn(a)
    let b = IntModule.fromFloatExn(b)
    t->Assert.deepEqual(a->IntModule.add(b), Some(result), ())
    t->Assert.deepEqual(a->IntModule.add(b), b->IntModule.add(a), ())
    t->Assert.deepEqual(a->IntModule.addExn(b), result, ())
    t->Assert.deepEqual(a->IntModule.addExn(b), b->IntModule.addExn(a), ())
    t->Assert.deepEqual(a->IntModule.addClamped(b), result, ())
    t->Assert.deepEqual(a->IntModule.addClamped(b), b->IntModule.addClamped(a), ())
    t->Assert.deepEqual(a->IntModule.addUnsafe(b), result, ())
    t->Assert.deepEqual(a->IntModule.addUnsafe(b), b->IntModule.addUnsafe(a), ())

    t->Assert.deepEqual(a->IntModule.add(IntModule.zero), Some(a), ())
    t->Assert.deepEqual(a->IntModule.add(IntModule.zero), IntModule.zero->IntModule.add(a), ())
    t->Assert.deepEqual(a->IntModule.addExn(IntModule.zero), a, ())
    t->Assert.deepEqual(
      a->IntModule.addExn(IntModule.zero),
      IntModule.zero->IntModule.addExn(a),
      (),
    )
    t->Assert.deepEqual(a->IntModule.addClamped(IntModule.zero), a, ())
    t->Assert.deepEqual(
      a->IntModule.addClamped(IntModule.zero),
      IntModule.zero->IntModule.addClamped(a),
      (),
    )
    t->Assert.deepEqual(a->IntModule.addUnsafe(IntModule.zero), a, ())
    t->Assert.deepEqual(
      a->IntModule.addUnsafe(IntModule.zero),
      IntModule.zero->IntModule.addUnsafe(a),
      (),
    )

    t->Assert.deepEqual(b->IntModule.add(IntModule.zero), Some(b), ())
    t->Assert.deepEqual(b->IntModule.add(IntModule.zero), IntModule.zero->IntModule.add(b), ())
    t->Assert.deepEqual(b->IntModule.addExn(IntModule.zero), b, ())
    t->Assert.deepEqual(
      b->IntModule.addExn(IntModule.zero),
      IntModule.zero->IntModule.addExn(b),
      (),
    )
    t->Assert.deepEqual(b->IntModule.addClamped(IntModule.zero), b, ())
    t->Assert.deepEqual(
      b->IntModule.addClamped(IntModule.zero),
      IntModule.zero->IntModule.addClamped(b),
      (),
    )
    t->Assert.deepEqual(b->IntModule.addUnsafe(IntModule.zero), b, ())
    t->Assert.deepEqual(
      b->IntModule.addUnsafe(IntModule.zero),
      IntModule.zero->IntModule.addUnsafe(b),
      (),
    )
  }

  testInRange(less, greater)
  testInRange(minValue, 0.0)
  testInRange(maxValue, 0.0)
  testInRange(minValue, 1.0)

  let testOutOfRange = (a, b, value) => {
    let a = IntModule.fromFloatExn(a)
    let b = IntModule.fromFloatExn(b)
    let value = IntModule.fromFloatExn(value)
    t->Assert.deepEqual(a->IntModule.add(b), None, ())
    t->Assert.deepEqual(b->IntModule.add(a), None, ())
    t->assertOverflow(() => a->IntModule.addExn(b))
    t->assertOverflow(() => b->IntModule.addExn(a))
    t->Assert.deepEqual(a->IntModule.addClamped(b), value, ())
    t->Assert.deepEqual(b->IntModule.addClamped(a), value, ())
  }

  testOutOfRange(maxValue, 1.0, maxValue)
  testOutOfRange(maxValue, value, maxValue)

  loop100Fn(() => {
    let a = randomValue()
    let b = randomValue()
    let result = a +. b
    if result < minValue {
      testOutOfRange(a, b, minValue)
    } else if result > maxValue {
      testOutOfRange(a, b, maxValue)
    } else {
      testInRange(a, b)
    }
    testInRange(a, 1.0)
    testInRange(b, 1.0)
  })
})

test(`test ${name} subtraction`, t => {
  let testInRange = (a, b) => {
    let result = IntModule.fromFloatExn(a -. b)
    let a = IntModule.fromFloatExn(a)
    let b = IntModule.fromFloatExn(b)
    t->Assert.deepEqual(a->IntModule.sub(b), Some(result), ())
    t->Assert.deepEqual(a->IntModule.subExn(b), result, ())
    t->Assert.deepEqual(a->IntModule.subClamped(b), result, ())
    t->Assert.deepEqual(a->IntModule.subUnsafe(b), result, ())
    t->Assert.deepEqual(a->IntModule.sub(result), Some(b), ())
    t->Assert.deepEqual(a->IntModule.subExn(result), b, ())
    t->Assert.deepEqual(a->IntModule.subClamped(result), b, ())
    t->Assert.deepEqual(a->IntModule.subUnsafe(result), b, ())

    t->Assert.deepEqual(a->IntModule.sub(a), Some(IntModule.zero), ())
    t->Assert.deepEqual(a->IntModule.subExn(a), IntModule.zero, ())
    t->Assert.deepEqual(a->IntModule.subClamped(a), IntModule.zero, ())
    t->Assert.deepEqual(a->IntModule.subUnsafe(a), IntModule.zero, ())
    t->Assert.deepEqual(a->IntModule.sub(IntModule.zero), Some(a), ())
    t->Assert.deepEqual(a->IntModule.subExn(IntModule.zero), a, ())
    t->Assert.deepEqual(a->IntModule.subClamped(IntModule.zero), a, ())
    t->Assert.deepEqual(a->IntModule.subUnsafe(IntModule.zero), a, ())

    t->Assert.deepEqual(b->IntModule.sub(b), Some(IntModule.zero), ())
    t->Assert.deepEqual(b->IntModule.subExn(b), IntModule.zero, ())
    t->Assert.deepEqual(b->IntModule.subClamped(b), IntModule.zero, ())
    t->Assert.deepEqual(b->IntModule.subUnsafe(b), IntModule.zero, ())
    t->Assert.deepEqual(b->IntModule.sub(IntModule.zero), Some(b), ())
    t->Assert.deepEqual(b->IntModule.subExn(IntModule.zero), b, ())
    t->Assert.deepEqual(b->IntModule.subClamped(IntModule.zero), b, ())
    t->Assert.deepEqual(b->IntModule.subUnsafe(IntModule.zero), b, ())
  }

  testInRange(greater, less)
  testInRange(minValue, 0.0)
  testInRange(maxValue, 0.0)
  testInRange(maxValue, 1.0)

  let testOutOfRange = (a, b, value) => {
    let a = IntModule.fromFloatExn(a)
    let b = IntModule.fromFloatExn(b)
    t->Assert.deepEqual(a->IntModule.sub(b), None, ())
    t->assertOverflow(() => a->IntModule.subExn(b))
    t->Assert.deepEqual(a->IntModule.subClamped(b), IntModule.fromFloatExn(value), ())
  }

  testOutOfRange(minValue, 1.0, minValue)
  testOutOfRange(minValue, value, minValue)

  loop100Fn(() => {
    let a = randomValue()
    let b = randomValue()

    let result = a -. b
    if result < minValue {
      testOutOfRange(a, b, minValue)
    } else if result > maxValue {
      testOutOfRange(a, b, maxValue)
    } else {
      testInRange(a, b)
    }

    let result = b -. a
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
    let result = IntModule.fromFloatExn(a *. b)
    let a = IntModule.fromFloatExn(a)
    let b = IntModule.fromFloatExn(b)
    t->Assert.deepEqual(a->IntModule.mul(b), Some(result), ())
    t->Assert.deepEqual(a->IntModule.mul(b), b->IntModule.mul(a), ())
    t->Assert.deepEqual(a->IntModule.mulExn(b), result, ())
    t->Assert.deepEqual(a->IntModule.mulExn(b), b->IntModule.mulExn(a), ())
    t->Assert.deepEqual(a->IntModule.mulClamped(b), result, ())
    t->Assert.deepEqual(a->IntModule.mulClamped(b), b->IntModule.mulClamped(a), ())
    t->Assert.deepEqual(a->IntModule.mulUnsafe(b), result, ())
    t->Assert.deepEqual(a->IntModule.mulUnsafe(b), b->IntModule.mulUnsafe(a), ())

    t->Assert.deepEqual(a->IntModule.mul(IntModule.zero), Some(IntModule.zero), ())
    t->Assert.deepEqual(a->IntModule.mul(IntModule.zero), IntModule.zero->IntModule.mul(a), ())
    t->Assert.deepEqual(a->IntModule.mulExn(IntModule.zero), IntModule.zero, ())
    t->Assert.deepEqual(
      a->IntModule.mulExn(IntModule.zero),
      IntModule.zero->IntModule.mulExn(a),
      (),
    )
    t->Assert.deepEqual(a->IntModule.mulClamped(IntModule.zero), IntModule.zero, ())
    t->Assert.deepEqual(
      a->IntModule.mulClamped(IntModule.zero),
      IntModule.zero->IntModule.mulClamped(a),
      (),
    )
    t->Assert.deepEqual(a->IntModule.mulUnsafe(IntModule.zero), IntModule.zero, ())
    t->Assert.deepEqual(
      a->IntModule.mulUnsafe(IntModule.zero),
      IntModule.zero->IntModule.mulUnsafe(a),
      (),
    )
    t->Assert.deepEqual(a->IntModule.mul(IntModule.one), Some(a), ())
    t->Assert.deepEqual(a->IntModule.mul(IntModule.one), IntModule.one->IntModule.mul(a), ())
    t->Assert.deepEqual(a->IntModule.mulExn(IntModule.one), a, ())
    t->Assert.deepEqual(a->IntModule.mulExn(IntModule.one), IntModule.one->IntModule.mulExn(a), ())
    t->Assert.deepEqual(a->IntModule.mulClamped(IntModule.one), a, ())
    t->Assert.deepEqual(
      a->IntModule.mulClamped(IntModule.one),
      IntModule.one->IntModule.mulClamped(a),
      (),
    )
    t->Assert.deepEqual(a->IntModule.mulUnsafe(IntModule.one), a, ())
    t->Assert.deepEqual(
      a->IntModule.mulUnsafe(IntModule.one),
      IntModule.one->IntModule.mulUnsafe(a),
      (),
    )

    t->Assert.deepEqual(b->IntModule.mul(IntModule.zero), Some(IntModule.zero), ())
    t->Assert.deepEqual(b->IntModule.mul(IntModule.zero), IntModule.zero->IntModule.mul(b), ())
    t->Assert.deepEqual(b->IntModule.mulExn(IntModule.zero), IntModule.zero, ())
    t->Assert.deepEqual(
      b->IntModule.mulExn(IntModule.zero),
      IntModule.zero->IntModule.mulExn(b),
      (),
    )
    t->Assert.deepEqual(b->IntModule.mulClamped(IntModule.zero), IntModule.zero, ())
    t->Assert.deepEqual(
      b->IntModule.mulClamped(IntModule.zero),
      IntModule.zero->IntModule.mulClamped(b),
      (),
    )
    t->Assert.deepEqual(b->IntModule.mulUnsafe(IntModule.zero), IntModule.zero, ())
    t->Assert.deepEqual(
      b->IntModule.mulUnsafe(IntModule.zero),
      IntModule.zero->IntModule.mulUnsafe(b),
      (),
    )
    t->Assert.deepEqual(b->IntModule.mul(IntModule.one), Some(b), ())
    t->Assert.deepEqual(b->IntModule.mul(IntModule.one), IntModule.one->IntModule.mul(b), ())
    t->Assert.deepEqual(b->IntModule.mulExn(IntModule.one), b, ())
    t->Assert.deepEqual(b->IntModule.mulExn(IntModule.one), IntModule.one->IntModule.mulExn(b), ())
    t->Assert.deepEqual(b->IntModule.mulClamped(IntModule.one), b, ())
    t->Assert.deepEqual(
      b->IntModule.mulClamped(IntModule.one),
      IntModule.one->IntModule.mulClamped(b),
      (),
    )
    t->Assert.deepEqual(b->IntModule.mulUnsafe(IntModule.one), b, ())
    t->Assert.deepEqual(
      b->IntModule.mulUnsafe(IntModule.one),
      IntModule.one->IntModule.mulUnsafe(b),
      (),
    )
  }

  testInRange(less, greater)
  testInRange(minValue, 0.0)
  testInRange(maxValue, 0.0)
  testInRange(minValue, 1.0)
  testInRange(maxValue, 1.0)
  testInRange(minValue +. 1.0, 1.0)
  testInRange(maxValue -. 1.0, 1.0)

  let testOutOfRange = (a, b, value) => {
    let a = IntModule.fromFloatExn(a)
    let b = IntModule.fromFloatExn(b)
    let value = IntModule.fromFloatExn(value)
    t->Assert.deepEqual(a->IntModule.mul(b), None, ())
    t->Assert.deepEqual(b->IntModule.mul(a), None, ())
    t->assertOverflow(() => a->IntModule.mulExn(b))
    t->assertOverflow(() => b->IntModule.mulExn(a))
    t->Assert.deepEqual(a->IntModule.mulClamped(b), value, ())
    t->Assert.deepEqual(b->IntModule.mulClamped(a), value, ())
  }

  testOutOfRange(maxValue, 2.0, maxValue)
  testOutOfRange(maxValue, value, maxValue)

  loop100Fn(() => {
    let a = randomValue()
    let b = randomValue()
    let result = a *. b
    if result < minValue {
      testOutOfRange(a, b, minValue)
    } else if result > maxValue {
      testOutOfRange(a, b, maxValue)
    } else {
      testInRange(a, b)
    }
  })
})

test(`test ${name} division`, t => {
  let testNotDividedByZero = (a, b) => {
    if a !== 0.0 {
      let result = IntModule.fromFloatExn((b /. a)->toUint32)
      let a = IntModule.fromFloatExn(a)
      let b = IntModule.fromFloatExn(b)
      t->Assert.deepEqual(b->IntModule.div(a), Some(result), ())
      t->Assert.deepEqual(b->IntModule.divExn(a), result, ())
      t->Assert.deepEqual(b->IntModule.divUnsafe(a), result, ())
      t->Assert.deepEqual(IntModule.zero->IntModule.div(a), Some(IntModule.zero), ())
      t->Assert.deepEqual(IntModule.zero->IntModule.divExn(a), IntModule.zero, ())
      t->Assert.deepEqual(IntModule.zero->IntModule.divUnsafe(a), IntModule.zero, ())
    }

    if b !== 0.0 {
      let result = IntModule.fromFloatExn((a /. b)->toUint32)
      let a = IntModule.fromFloatExn(a)
      let b = IntModule.fromFloatExn(b)
      t->Assert.deepEqual(a->IntModule.div(b), Some(result), ())
      t->Assert.deepEqual(a->IntModule.divExn(b), result, ())
      t->Assert.deepEqual(a->IntModule.divUnsafe(b), result, ())
      t->Assert.deepEqual(IntModule.zero->IntModule.div(b), Some(IntModule.zero), ())
      t->Assert.deepEqual(IntModule.zero->IntModule.divExn(b), IntModule.zero, ())
      t->Assert.deepEqual(IntModule.zero->IntModule.divUnsafe(b), IntModule.zero, ())
    }

    let a = IntModule.fromFloatExn(a)
    let b = IntModule.fromFloatExn(b)
    t->Assert.deepEqual(a->IntModule.div(IntModule.one), Some(a), ())
    t->Assert.deepEqual(a->IntModule.divExn(IntModule.one), a, ())
    t->Assert.deepEqual(a->IntModule.divUnsafe(IntModule.one), a, ())
    t->Assert.deepEqual(b->IntModule.div(IntModule.one), Some(b), ())
    t->Assert.deepEqual(b->IntModule.divExn(IntModule.one), b, ())
    t->Assert.deepEqual(b->IntModule.divUnsafe(IntModule.one), b, ())
  }

  testNotDividedByZero(less, greater)
  testNotDividedByZero(minValue, less)
  testNotDividedByZero(minValue, greater)
  testNotDividedByZero(maxValue, less)
  testNotDividedByZero(maxValue, greater)
  testNotDividedByZero(less, 2.0)
  testNotDividedByZero(greater, 2.0)
  testNotDividedByZero(minValue, 2.0)
  testNotDividedByZero(maxValue, 2.0)
  testNotDividedByZero(minValue +. 1.0, 2.0)
  testNotDividedByZero(maxValue -. 1.0, 2.0)

  let testDividedByZero = i => {
    let a = IntModule.fromFloatExn(i)
    t->Assert.deepEqual(a->IntModule.div(IntModule.zero), None, ())
    t->assertDivisionByZero(() => a->IntModule.divExn(IntModule.zero))
    let negZero = IntModule.fromIntExn(%raw(`-0`))
    t->Assert.deepEqual(a->IntModule.div(negZero), None, ())
    t->assertDivisionByZero(() => a->IntModule.divExn(negZero))
  }

  testDividedByZero(0.0)
  testDividedByZero(1.0)
  testDividedByZero(less)
  testDividedByZero(greater)
  testDividedByZero(value)
  testDividedByZero(minValue)
  testDividedByZero(maxValue)
  testDividedByZero(minValue +. 1.0)
  testDividedByZero(maxValue -. 1.0)

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
    if a !== 0.0 {
      let result = IntModule.fromFloatExn(b->mod_float(a)->toUint32)
      let a = IntModule.fromFloatExn(a)
      let b = IntModule.fromFloatExn(b)
      t->Assert.deepEqual(b->IntModule.rem(a), Some(result), ())
      t->Assert.deepEqual(b->IntModule.remExn(a), result, ())
      t->Assert.deepEqual(b->IntModule.remUnsafe(a), result, ())
      t->Assert.deepEqual(IntModule.zero->IntModule.rem(a), Some(IntModule.zero), ())
      t->Assert.deepEqual(IntModule.zero->IntModule.remExn(a), IntModule.zero, ())
      t->Assert.deepEqual(IntModule.zero->IntModule.remUnsafe(a), IntModule.zero, ())
    }

    if b !== 0.0 {
      let result = IntModule.fromFloatExn(a->mod_float(b)->toUint32)
      let a = IntModule.fromFloatExn(a)
      let b = IntModule.fromFloatExn(b)
      t->Assert.deepEqual(a->IntModule.rem(b), Some(result), ())
      t->Assert.deepEqual(a->IntModule.remExn(b), result, ())
      t->Assert.deepEqual(a->IntModule.remUnsafe(b), result, ())
      t->Assert.deepEqual(IntModule.zero->IntModule.rem(b), Some(IntModule.zero), ())
      t->Assert.deepEqual(IntModule.zero->IntModule.remExn(b), IntModule.zero, ())
      t->Assert.deepEqual(IntModule.zero->IntModule.remUnsafe(b), IntModule.zero, ())
    }

    let a = IntModule.fromFloatExn(a)
    let b = IntModule.fromFloatExn(b)
    t->Assert.deepEqual(a->IntModule.rem(IntModule.one), Some(IntModule.zero), ())
    t->Assert.deepEqual(a->IntModule.remExn(IntModule.one), IntModule.zero, ())
    t->Assert.deepEqual(a->IntModule.remUnsafe(IntModule.one), IntModule.zero, ())
    t->Assert.deepEqual(b->IntModule.rem(IntModule.one), Some(IntModule.zero), ())
    t->Assert.deepEqual(b->IntModule.remExn(IntModule.one), IntModule.zero, ())
    t->Assert.deepEqual(b->IntModule.remUnsafe(IntModule.one), IntModule.zero, ())
  }

  testNotModByZero(less, greater)
  testNotModByZero(minValue, less)
  testNotModByZero(minValue, greater)
  testNotModByZero(maxValue, less)
  testNotModByZero(maxValue, greater)
  testNotModByZero(less, 2.0)
  testNotModByZero(greater, 2.0)
  testNotModByZero(minValue, 2.0)
  testNotModByZero(maxValue, 2.0)
  testNotModByZero(minValue +. 1.0, 2.0)
  testNotModByZero(maxValue -. 1.0, 2.0)

  let testModByZero = a => {
    let a = IntModule.fromFloatExn(a)
    t->Assert.deepEqual(a->IntModule.rem(IntModule.zero), None, ())
    t->assertDivisionByZero(() => a->IntModule.remExn(IntModule.zero))
    let negZero = IntModule.fromIntExn(%raw(`-0`))
    t->Assert.deepEqual(a->IntModule.rem(negZero), None, ())
    t->assertDivisionByZero(() => a->IntModule.remExn(negZero))
  }

  testModByZero(0.0)
  testModByZero(1.0)
  testModByZero(less)
  testModByZero(greater)
  testModByZero(value)
  testModByZero(minValue)
  testModByZero(maxValue)
  testModByZero(minValue +. 1.0)
  testModByZero(maxValue -. 1.0)

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
    let result = IntModule.fromFloatExn(arr->Array.reduce(0.0, (acc, v) => acc +. v))
    let arr = arr->Array.mapU(i => IntModule.fromFloatExn(i))
    t->Assert.deepEqual(arr->IntModule.sum, Some(result), ())
    t->Assert.deepEqual(arr->IntModule.sumExn, result, ())
    t->Assert.deepEqual(arr->IntModule.sumUnsafe, result, ())
  }

  testInRange([less])
  testInRange([greater])
  testInRange([minValue])
  testInRange([maxValue])
  testInRange([minValue +. 1.0])
  testInRange([maxValue -. 1.0])
  testInRange([less, greater])
  testInRange([greater, less])
  testInRange([less, 0.0, value, 1.0, greater])
  testInRange([minValue, maxValue])

  t->Assert.deepEqual(IntModule.sum([]), None, ())
  t->assertInvalidArgument(() => IntModule.sumExn([]))

  let testOutOfRange = arr => {
    let arr = arr->Array.mapU(i => IntModule.fromFloatExn(i))
    t->Assert.deepEqual(arr->IntModule.sum, None, ())
    t->assertOverflow(() => arr->IntModule.sumExn)
  }

  testOutOfRange([maxValue, 1.0])
  testOutOfRange([1.0, maxValue])
  testOutOfRange([maxValue, value])
  testOutOfRange([value, maxValue])
  testOutOfRange([maxValue, greater -. less, value])

  loop100Fn(() => {
    let a = randomValue()
    let b = randomValue()
    testInRange([a])
    testInRange([b])

    let result = a +. b
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
  let testIncInRange = f => {
    let result = IntModule.fromFloatExn(f +. 1.0)
    let n = IntModule.fromFloatExn(f)
    t->Assert.deepEqual(n->IntModule.inc, Some(result), ())
    t->Assert.deepEqual(n->IntModule.incExn, result, ())
    t->Assert.deepEqual(n->IntModule.incUnsafe, result, ())
  }

  testIncInRange(0.0)
  testIncInRange(1.0)
  testIncInRange(value)
  testIncInRange(minValue)
  testIncInRange(minValue +. 1.0)
  testIncInRange(maxValue -. 1.0)
  loop100Fn(() => testIncInRange(randomValue()))

  t->Assert.deepEqual(IntModule.maxValue->IntModule.inc, None, ())
  t->assertOverflow(() => IntModule.maxValue->IntModule.incExn)

  let testDecInRange = f => {
    let result = IntModule.fromFloatExn(f -. 1.0)
    let n = IntModule.fromFloatExn(f)
    t->Assert.deepEqual(n->IntModule.dec, Some(result), ())
    t->Assert.deepEqual(n->IntModule.decExn, result, ())
    t->Assert.deepEqual(n->IntModule.decUnsafe, result, ())
  }

  testDecInRange(1.0)
  testDecInRange(value)
  testDecInRange(minValue +. 1.0)
  testDecInRange(maxValue)
  testDecInRange(maxValue -. 1.0)
  loop100Fn(() => testDecInRange(randomFloat(minValue +. 1.0, maxValue)->Js.Math.floor_float))

  t->Assert.deepEqual(IntModule.minValue->IntModule.dec, None, ())
  t->assertOverflow(() => IntModule.minValue->IntModule.decExn)
})

test(`test ${name} bitwise`, t => {
  let assertNot1 = (a, b) => {
    let a = IntModule.fromFloatExn(a)
    let b = IntModule.fromFloatExn(b)
    t->Assert.deepEqual(a->IntModule.lnot, b, ())
    t->Assert.deepEqual(b->IntModule.lnot, a, ())
  }

  assertNot1(value, valueNot)
  assertNot1(minValue, maxValue)
  assertNot1(minValue +. 1.0, maxValue -. 1.0)

  let assertNot2 = i => {
    let n = IntModule.fromFloatExn(i)
    t->Assert.deepEqual(n->IntModule.lnot->IntModule.lnot, n, ())
  }

  assertNot2(0.0)
  assertNot2(1.0)
  assertNot2(value)
  assertNot2(minValue)
  assertNot2(maxValue)
  loop100Fn(() => assertNot2(randomValue()))

  let assertAnd = (a, b, value) => {
    let a = IntModule.fromFloatExn(a)
    let b = IntModule.fromFloatExn(b)
    let value = IntModule.fromFloatExn(value)
    t->Assert.deepEqual(a->IntModule.land(a), a, ())
    t->Assert.deepEqual(b->IntModule.land(b), b, ())
    t->Assert.deepEqual(value->IntModule.land(value), value, ())
    t->Assert.deepEqual(a->IntModule.land(a->IntModule.lnot), IntModule.zero, ())
    t->Assert.deepEqual(b->IntModule.land(b->IntModule.lnot), IntModule.zero, ())
    t->Assert.deepEqual(value->IntModule.land(value->IntModule.lnot), IntModule.zero, ())
    t->Assert.deepEqual(a->IntModule.land(b), value, ())
    t->Assert.deepEqual(b->IntModule.land(a), value, ())
  }

  assertAnd(value, valueNot, 0.0)
  assertAnd(minValue, maxValue, 0.0)
  assertAnd(minValue +. 1.0, maxValue -. 1.0, 0.0)
  assertAnd(less, greater, lessGreaterAnd)
  loop100Fn(() => {
    let value = randomValue()
    assertAnd(value, value, value)
  })

  let assertOr = (a, b, value) => {
    let a = IntModule.fromFloatExn(a)
    let b = IntModule.fromFloatExn(b)
    let value = IntModule.fromFloatExn(value)
    let oneBits = IntModule.maxValue
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
  assertOr(value, valueNot, maxValue)
  assertOr(minValue, maxValue, maxValue)
  assertOr(minValue +. 1.0, maxValue -. 1.0, maxValue)
  loop100Fn(() => {
    let value = randomValue()
    assertOr(value, value, value)
  })

  let assertXor = (a, b, value) => {
    let a = IntModule.fromFloatExn(a)
    let b = IntModule.fromFloatExn(b)
    let value = IntModule.fromFloatExn(value)
    let oneBits = IntModule.maxValue
    t->Assert.deepEqual(a->IntModule.lxor(a), IntModule.zero, ())
    t->Assert.deepEqual(b->IntModule.lxor(b), IntModule.zero, ())
    t->Assert.deepEqual(value->IntModule.lxor(value), IntModule.zero, ())
    t->Assert.deepEqual(a->IntModule.lxor(a->IntModule.lnot), oneBits, ())
    t->Assert.deepEqual(b->IntModule.lxor(b->IntModule.lnot), oneBits, ())
    t->Assert.deepEqual(value->IntModule.lxor(value->IntModule.lnot), oneBits, ())
    t->Assert.deepEqual(a->IntModule.lxor(b), value, ())
    t->Assert.deepEqual(b->IntModule.lxor(a), value, ())
  }

  assertXor(less, greater, lessGreaterXor)
  assertXor(value, valueNot, maxValue)
  assertXor(minValue, maxValue, maxValue)
  assertXor(minValue +. 1.0, maxValue -. 1.0, maxValue)
  loop100Fn(() => {
    let value = randomValue()
    assertXor(value, value, 0.0)
  })

  @warning("-27")
  let modBits = (i: int): int => %raw(`(i >>> 0) % bits`)

  {
    let value = IntModule.fromFloatExn(value)

    for i in -bits to 2 * bits {
      t->Assert.deepEqual(
        value->IntModule.lsl(i),
        IntModule.fromFloatExn(valueShiftLeft->Array.getExn(i->modBits)),
        (),
      )
    }
  }

  let testUnsignedShiftRight = value => {
    let n = IntModule.fromFloatExn(value)
    for i in -bits to 2 * bits {
      t->Assert.deepEqual(
        n->IntModule.lsr(i),
        IntModule.fromFloatExn(value->Float.toInt->lsr(i)->Int.toFloat->toUint32),
        (),
      )
    }
  }

  testUnsignedShiftRight(value)
  testUnsignedShiftRight(valueNot)
  loop100Fn(() => testUnsignedShiftRight(randomValue()))

  let testShiftRight = value => {
    let n = IntModule.fromFloatExn(value)
    for i in -bits to 2 * bits {
      t->Assert.deepEqual(
        n->IntModule.asr(i),
        IntModule.fromFloatExn((value /. 2.0 ** i->modBits->Int.toFloat)->Js.Math.floor_float),
        (),
      )
    }
  }

  testShiftRight(value)
  testShiftRight(valueNot)
  loop100Fn(() => testShiftRight(randomValue()))

  {
    let value = IntModule.fromFloatExn(value)

    for i in -bits to 2 * bits {
      t->Assert.deepEqual(
        value->IntModule.rsl(i),
        IntModule.fromFloatExn(valueRotateShiftLeft->Array.getExn(i->modBits)),
        (),
      )
    }
  }

  let testRotateShiftLeft = value => {
    let n = IntModule.fromFloatExn(value)
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
    let n = IntModule.fromFloatExn(value)
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

  testClz(IntModule.zero, bits)

  testClz(IntModule.minValue, bits)
  testClz(IntModule.maxValue, 0)

  for i in 0 to bits - 1 {
    testClz(IntModule.one->IntModule.lsl(i), bits - i - 1)
  }

  t->Assert.deepEqual(
    IntModule.fromFloatExn(less)->IntModule.imul(IntModule.fromFloatExn(greater)),
    IntModule.fromFloatExn(less *. greater),
    (),
  )

  let j = 2.0 ** bits->Int.toFloat

  let testImul = (a, b) => {
    let m = (a->Int.toFloat *. b->Int.toFloat)->mod_float(j)
    let result = m

    t->Assert.deepEqual(
      IntModule.fromIntExn(a)->IntModule.imul(IntModule.fromIntExn(b)),
      IntModule.fromFloatExn(result),
      (),
    )
    t->Assert.deepEqual(
      IntModule.fromIntExn(b)->IntModule.imul(IntModule.fromIntExn(a)),
      IntModule.fromFloatExn(result),
      (),
    )
  }

  let n = 1000000.0
  loop100Fn(() => testImul(randomInt(0.0, n), randomInt(0.0, n)))

  loop100Fn(() => {
    let a = randomValue()
    let b = randomValue()
    t->Assert.deepEqual(
      IntModule.fromFloatExn(a)->IntModule.imul(IntModule.fromFloatExn(b)),
      IntModule.fromFloatExn(b)->IntModule.imul(IntModule.fromFloatExn(a)),
      (),
    )
  })

  let testPowInRange = (a, b) => {
    let result = IntModule.fromFloatExn(a ** b->Int.toFloat)
    let a = IntModule.fromFloatExn(a)
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

  testPowInRange(0.0, 1)
  testPowInRange(1.0, 0)
  testPowInRange(value, 0)
  testPowInRange(minValue, 1)
  testPowInRange(minValue, value->Float.toInt)
  testPowInRange(maxValue, 1)

  let testPowOutOfRange = (a, b) => {
    let a = IntModule.fromFloatExn(a)
    let b = IntModule.fromIntExn(b)

    t->Assert.deepEqual(IntModule.pow(~base=a, ~exp=b), None, ())
    t->assertOverflow(() => IntModule.powExn(~base=a, ~exp=b))
  }

  testPowOutOfRange(maxValue, 2)

  loop100Fn(() => {
    let a = randomValue()
    let b = randomPositiveSmallInt()
    let result = a ** b->Int.toFloat

    if result >= minValue && result <= maxValue {
      testPowInRange(a, b)
    } else {
      testPowOutOfRange(a, b)
    }
  })

  loop100Fn(() => {
    let m = randomValue()
    let n = randomValue()
    let min = IntModule.fromFloatExn(m < n ? m : n)
    let max = IntModule.fromFloatExn(m > n ? m : n)

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
