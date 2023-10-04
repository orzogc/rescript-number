open Ava

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

@warning("-27")
let toUint32 = (i: int): float => %raw(`i >>> 0`)
