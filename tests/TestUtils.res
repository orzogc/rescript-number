open Ava

let minSafeInteger = ResNumber__Utils.minSafeInteger

let maxSafeInteger = ResNumber__Utils.maxSafeInteger

let assertException = (t, f) =>
  try {
    f()->ignore
    t->Assert.fail("it should raise an exception")
  } catch {
  | _ => t->Assert.pass()
  }

let assertOverflow = (t, f) =>
  try {
    f()->ignore
    t->Assert.fail("it should raise an exception")
  } catch {
  | Number.NumberOverflow(_) => t->Assert.pass()
  }

let assertInvalidArgument = (t, f) =>
  try {
    f()->ignore
    t->Assert.fail("it should raise an exception")
  } catch {
  | Invalid_argument(_) => t->Assert.pass()
  }

let assertDivisionByZero = (t, f) =>
  try {
    f()->ignore
    t->Assert.fail("it should raise an exception")
  } catch {
  | Division_by_zero => t->Assert.pass()
  }

let loopFn = (f, n) =>
  for _ in 0 to n - 1 {
    f()->ignore
  }

let loop100Fn = f => loopFn(f, 100)

let randomFloat = (min, max) => Js.Math.random() *. (max -. min) +. min

let randomInt = (min, max) => randomFloat(min, max)->Js.Math.floor_int

let randomPositiveSmallInt = () => randomInt(0.0, 16.0)

let randomPositiveInt = () => randomInt(0.0, Js.Int.max->Js.Int.toFloat)

let randomNegativeInt = () => randomInt(Js.Int.min->Js.Int.toFloat, 0.0)

@warning("-27")
let intToUint32 = (i: int): float => %raw(`i >>> 0`)
