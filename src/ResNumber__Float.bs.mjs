// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Js_int from "rescript/lib/es6/js_int.js";
import * as PervasivesU from "rescript/lib/es6/pervasivesU.js";
import * as Caml_splice_call from "rescript/lib/es6/caml_splice_call.js";
import * as ResNumber__Utils from "./ResNumber__Utils.bs.mjs";
import * as ResNumber__Operation from "./ResNumber__Operation.bs.mjs";

function raiseOperationNaN(a, b, s) {
  return PervasivesU.invalid_arg(a.toString() + " " + s + " " + b.toString() + " = Nan");
}

function toString(f) {
  return f.toString();
}

function fromIntUnsafe(i) {
  return i;
}

function fromInt(i) {
  return i;
}

function toIntUnsafe(f) {
  return f | 0;
}

function toInt(f) {
  if (Number.isInteger(f) && ResNumber__Utils.inInt32Range(f)) {
    return f | 0;
  }
  
}

function toIntExn(f) {
  if (Number.isInteger(f)) {
    if (ResNumber__Utils.inInt32Range(f)) {
      return f | 0;
    } else {
      return ResNumber__Utils.raiseOverflow(f, ResNumber__Utils.Int32Range);
    }
  } else {
    return PervasivesU.invalid_arg("float number " + f.toString() + " is not a integer");
  }
}

function toIntClamped(f) {
  if (f < Js_int.min) {
    return Js_int.min;
  } else if (f > Js_int.max) {
    return Js_int.max;
  } else {
    return f | 0;
  }
}

function fromFloat(f) {
  if (!Number.isNaN(f)) {
    return f;
  }
  
}

function fromFloatExn(f) {
  var v = fromFloat(f);
  if (v !== undefined) {
    return v;
  } else {
    return PervasivesU.invalid_arg("NaN");
  }
}

function fromFloatClamped(f) {
  if (Number.isNaN(f)) {
    return 0.0;
  } else {
    return f;
  }
}

function fromFloatUnsafe(f) {
  return f;
}

function toFloat(f) {
  return f;
}

function fromStringExn(s) {
  var f = ResNumber__Utils.stringToFloat(s);
  if (f !== undefined) {
    return f;
  } else {
    return PervasivesU.invalid_arg("the string is not a float number: " + s);
  }
}

function compare(a, b) {
  if (!Number.isNaN(a) && !Number.isNaN(b)) {
    return a < b ? -1 : (
              a > b ? 1 : 0
            );
  }
  
}

function compareExn(a, b) {
  var v = compare(a, b);
  if (v !== undefined) {
    return v;
  } else {
    return PervasivesU.invalid_arg(a.toString() + " or " + b.toString() + " is NaN");
  }
}

function eq(a, b) {
  return a === b;
}

function ne(a, b) {
  return a !== b;
}

function lt(a, b) {
  return a < b;
}

function le(a, b) {
  return a <= b;
}

function gt(a, b) {
  return a > b;
}

function ge(a, b) {
  return a >= b;
}

function min(a, b) {
  if (a < b) {
    return a;
  } else {
    return b;
  }
}

function minMany(arr) {
  var len = arr.length;
  if (len <= 0) {
    return ;
  }
  var f = len === 1 ? arr[0] : Caml_splice_call.spliceApply(Math.min, [arr]);
  if (!Number.isNaN(f)) {
    return f;
  }
  
}

function minManyExn(arr) {
  var len = arr.length;
  if (len <= 0) {
    return ResNumber__Utils.raiseEmptyArray(undefined);
  }
  var f = len === 1 ? arr[0] : Caml_splice_call.spliceApply(Math.min, [arr]);
  if (Number.isNaN(f)) {
    return PervasivesU.invalid_arg("NaN");
  } else {
    return f;
  }
}

function minManyUnsafe(arr) {
  return Caml_splice_call.spliceApply(Math.min, [arr]);
}

function max(a, b) {
  if (a > b) {
    return a;
  } else {
    return b;
  }
}

function maxMany(arr) {
  var len = arr.length;
  if (len <= 0) {
    return ;
  }
  var f = len === 1 ? arr[0] : Caml_splice_call.spliceApply(Math.max, [arr]);
  if (!Number.isNaN(f)) {
    return f;
  }
  
}

function maxManyExn(arr) {
  var len = arr.length;
  if (len <= 0) {
    return ResNumber__Utils.raiseEmptyArray(undefined);
  }
  var f = len === 1 ? arr[0] : Caml_splice_call.spliceApply(Math.max, [arr]);
  if (Number.isNaN(f)) {
    return PervasivesU.invalid_arg("NaN");
  } else {
    return f;
  }
}

function maxManyUnsafe(arr) {
  return Caml_splice_call.spliceApply(Math.max, [arr]);
}

function addUnsafe(a, b) {
  return a + b;
}

function add(a, b) {
  var f = a + b;
  if (!Number.isNaN(f)) {
    return f;
  }
  
}

function addExn(a, b) {
  var f = add(a, b);
  if (f !== undefined) {
    return f;
  } else {
    return raiseOperationNaN(a, b, "+");
  }
}

function addClamped(a, b) {
  var f = add(a, b);
  if (f !== undefined) {
    return f;
  } else {
    return 0.0;
  }
}

function subUnsafe(a, b) {
  return a - b;
}

function sub(a, b) {
  var f = a - b;
  if (!Number.isNaN(f)) {
    return f;
  }
  
}

function subExn(a, b) {
  var f = sub(a, b);
  if (f !== undefined) {
    return f;
  } else {
    return raiseOperationNaN(a, b, "-");
  }
}

function subClamped(a, b) {
  var f = sub(a, b);
  if (f !== undefined) {
    return f;
  } else {
    return 0.0;
  }
}

function mulUnsafe(a, b) {
  return a * b;
}

function mul(a, b) {
  var f = a * b;
  if (!Number.isNaN(f)) {
    return f;
  }
  
}

function mulExn(a, b) {
  var f = mul(a, b);
  if (f !== undefined) {
    return f;
  } else {
    return raiseOperationNaN(a, b, "*");
  }
}

function mulClamped(a, b) {
  var f = mul(a, b);
  if (f !== undefined) {
    return f;
  } else {
    return 0.0;
  }
}

function divUnsafe(a, b) {
  return a / b;
}

function div(a, b) {
  if (b === 0.0) {
    return ;
  }
  var f = a / b;
  if (!Number.isNaN(f)) {
    return f;
  }
  
}

function divExn(a, b) {
  if (b !== 0.0) {
    var f = a / b;
    if (Number.isNaN(f)) {
      return raiseOperationNaN(a, b, "/");
    } else {
      return f;
    }
  }
  throw {
        RE_EXN_ID: "Division_by_zero",
        Error: new Error()
      };
}

function remUnsafe(a, b) {
  return a % b;
}

function rem(a, b) {
  if (b === 0.0) {
    return ;
  }
  var f = a % b;
  if (!Number.isNaN(f)) {
    return f;
  }
  
}

function remExn(a, b) {
  if (b !== 0.0) {
    var f = a % b;
    if (Number.isNaN(f)) {
      return raiseOperationNaN(a, b, "%");
    } else {
      return f;
    }
  }
  throw {
        RE_EXN_ID: "Division_by_zero",
        Error: new Error()
      };
}

var include = ResNumber__Operation.MakeNumberSum({
      add: add,
      addExn: addExn,
      addClamped: addClamped,
      addUnsafe: addUnsafe
    });

function negUnsafe(f) {
  return - f;
}

function neg(f) {
  if (!Number.isNaN(f)) {
    return - f;
  }
  
}

function negExn(f) {
  var v = neg(f);
  if (v !== undefined) {
    return v;
  } else {
    return PervasivesU.invalid_arg("NaN");
  }
}

function absUnsafe(f) {
  return Math.abs(f);
}

function abs(f) {
  if (!Number.isNaN(f)) {
    return Math.abs(f);
  }
  
}

function absExn(f) {
  var v = abs(f);
  if (v !== undefined) {
    return v;
  } else {
    return PervasivesU.invalid_arg("NaN");
  }
}

function sign(f) {
  var match = Math.sign(f);
  if (match !== -1.0) {
    if (match !== 0.0) {
      if (match !== 1.0) {
        return ;
      } else {
        return 1;
      }
    } else {
      return 0;
    }
  } else {
    return -1;
  }
}

function signExn(f) {
  var v = sign(f);
  if (v !== undefined) {
    return v;
  } else {
    return PervasivesU.invalid_arg("NaN");
  }
}

function signRaw(f) {
  return Math.sign(f);
}

function acos(f) {
  return Math.acos(f);
}

function acosh(f) {
  return Math.acosh(f);
}

function asin(f) {
  return Math.asin(f);
}

function asinh(f) {
  return Math.asinh(f);
}

function atan(f) {
  return Math.atan(f);
}

function atan2(y, x) {
  return Math.atan2(y, x);
}

function atanh(f) {
  return Math.atanh(f);
}

function cbrt(f) {
  return Math.cbrt(f);
}

function ceil(f) {
  return Math.ceil(f);
}

var int32minMinus1 = Js_int.min - 1.0;

function ceilIntUnsafe(f) {
  return Math.ceil(f) | 0;
}

function ceilInt(f) {
  if (f > int32minMinus1 && f <= Js_int.max) {
    return ceilIntUnsafe(f);
  }
  
}

function ceilIntExn(f) {
  var v = ceilInt(f);
  if (v !== undefined) {
    return v;
  } else {
    return ResNumber__Utils.raiseOverflow(Math.ceil(f), ResNumber__Utils.Int32Range);
  }
}

function ceilIntClamped(f) {
  if (f < Js_int.min) {
    return Js_int.min;
  } else if (f > Js_int.max) {
    return Js_int.max;
  } else {
    return ceilIntUnsafe(f);
  }
}

function cos(f) {
  return Math.cos(f);
}

function cosh(f) {
  return Math.cosh(f);
}

function exp(f) {
  return Math.exp(f);
}

function expm1(f) {
  return Math.expm1(f);
}

function floor(f) {
  return Math.floor(f);
}

var int32maxPlus1 = Js_int.max + 1.0;

function floorIntUnsafe(f) {
  return Math.floor(f) | 0;
}

function floorInt(f) {
  if (f >= Js_int.min && f < int32maxPlus1) {
    return floorIntUnsafe(f);
  }
  
}

function floorIntExn(f) {
  var v = floorInt(f);
  if (v !== undefined) {
    return v;
  } else {
    return ResNumber__Utils.raiseOverflow(Math.floor(f), ResNumber__Utils.Int32Range);
  }
}

function floorIntClamped(f) {
  if (f < Js_int.min) {
    return Js_int.min;
  } else if (f > Js_int.max) {
    return Js_int.max;
  } else {
    return floorIntUnsafe(f);
  }
}

function fround(f) {
  return Math.fround(f);
}

function hypot(a, b) {
  return Math.hypot(a, b);
}

function hypotMany(arr) {
  return Caml_splice_call.spliceApply(Math.hypot, [arr]);
}

function log(f) {
  return Math.log(f);
}

function log10(f) {
  return Math.log10(f);
}

function log1p(f) {
  return Math.log1p(f);
}

function log2(f) {
  return Math.log2(f);
}

function pow(base, exp) {
  return Math.pow(base, exp);
}

function random() {
  return Math.random();
}

function randomRange(min, max) {
  return Math.random() * (max - min) + min;
}

function round(f) {
  return Math.round(f);
}

var int32minMinusHalf1 = Js_int.min - 0.5;

var int32maxPlusHalf1 = Js_int.max + 0.5;

function roundIntUnsafe(f) {
  return Math.round(f) | 0;
}

function roundInt(f) {
  if (f >= int32minMinusHalf1 && f < int32maxPlusHalf1) {
    return Math.round(f) | 0;
  }
  
}

function roundIntExn(f) {
  var v = roundInt(f);
  if (v !== undefined) {
    return v;
  } else {
    return ResNumber__Utils.raiseOverflow(Math.round(f), ResNumber__Utils.Int32Range);
  }
}

function roundIntClamped(f) {
  if (f < Js_int.min) {
    return Js_int.min;
  } else if (f > Js_int.max) {
    return Js_int.max;
  } else {
    return Math.round(f) | 0;
  }
}

function sin(f) {
  return Math.sin(f);
}

function sinh(f) {
  return Math.sinh(f);
}

function sqrt(f) {
  return Math.sqrt(f);
}

function tan(f) {
  return Math.tan(f);
}

function tanh(f) {
  return Math.tanh(f);
}

function trunc(f) {
  return Math.trunc(f);
}

function truncIntUnsafe(f) {
  return Math.trunc(f) | 0;
}

function truncInt(f) {
  if (f > int32minMinus1 && f < int32maxPlus1) {
    return truncIntUnsafe(f);
  }
  
}

function truncIntExn(f) {
  var v = truncInt(f);
  if (v !== undefined) {
    return v;
  } else {
    return ResNumber__Utils.raiseOverflow(Math.trunc(f), ResNumber__Utils.Int32Range);
  }
}

function truncIntClamped(f) {
  if (f < Js_int.min) {
    return Js_int.min;
  } else if (f > Js_int.max) {
    return Js_int.max;
  } else {
    return truncIntUnsafe(f);
  }
}

var Float64_sum = include.sum;

var Float64_sumExn = include.sumExn;

var Float64_sumUnsafe = include.sumUnsafe;

var Float64 = {
  fromInt: fromInt,
  fromIntExn: fromIntUnsafe,
  fromIntClamped: fromIntUnsafe,
  fromIntUnsafe: fromIntUnsafe,
  toInt: toInt,
  toIntExn: toIntExn,
  toIntClamped: toIntClamped,
  toIntUnsafe: toIntUnsafe,
  fromFloat: fromFloat,
  fromFloatExn: fromFloatExn,
  fromFloatClamped: fromFloatClamped,
  fromFloatUnsafe: fromFloatUnsafe,
  toFloat: toFloat,
  fromString: ResNumber__Utils.stringToFloat,
  fromStringExn: fromStringExn,
  toString: toString,
  zero: 0.0,
  one: 1.0,
  minValue: PervasivesU.neg_infinity,
  maxValue: PervasivesU.infinity,
  compare: compare,
  compareExn: compareExn,
  eq: eq,
  ne: ne,
  lt: lt,
  le: le,
  gt: gt,
  ge: ge,
  min: min,
  minMany: minMany,
  minManyExn: minManyExn,
  minManyUnsafe: minManyUnsafe,
  max: max,
  maxMany: maxMany,
  maxManyExn: maxManyExn,
  maxManyUnsafe: maxManyUnsafe,
  add: add,
  addExn: addExn,
  addClamped: addClamped,
  addUnsafe: addUnsafe,
  sub: sub,
  subExn: subExn,
  subClamped: subClamped,
  subUnsafe: subUnsafe,
  mul: mul,
  mulExn: mulExn,
  mulClamped: mulClamped,
  mulUnsafe: mulUnsafe,
  div: div,
  divExn: divExn,
  divUnsafe: divUnsafe,
  rem: rem,
  remExn: remExn,
  remUnsafe: remUnsafe,
  sum: Float64_sum,
  sumExn: Float64_sumExn,
  sumUnsafe: Float64_sumUnsafe,
  neg: neg,
  negExn: negExn,
  negUnsafe: negUnsafe,
  abs: abs,
  absExn: absExn,
  absUnsafe: absUnsafe,
  sign: sign,
  signExn: signExn,
  signRaw: signRaw,
  acos: acos,
  acosh: acosh,
  asin: asin,
  asinh: asinh,
  atan: atan,
  atan2: atan2,
  atanh: atanh,
  cbrt: cbrt,
  ceil: ceil,
  ceilInt: ceilInt,
  ceilIntExn: ceilIntExn,
  ceilIntClamped: ceilIntClamped,
  ceilIntUnsafe: ceilIntUnsafe,
  cos: cos,
  cosh: cosh,
  exp: exp,
  expm1: expm1,
  floor: floor,
  floorInt: floorInt,
  floorIntExn: floorIntExn,
  floorIntClamped: floorIntClamped,
  floorIntUnsafe: floorIntUnsafe,
  fround: fround,
  hypot: hypot,
  hypotMany: hypotMany,
  log: log,
  log10: log10,
  log1p: log1p,
  log2: log2,
  pow: pow,
  random: random,
  randomRange: randomRange,
  round: round,
  roundInt: roundInt,
  roundIntExn: roundIntExn,
  roundIntClamped: roundIntClamped,
  roundIntUnsafe: roundIntUnsafe,
  sin: sin,
  sinh: sinh,
  sqrt: sqrt,
  tan: tan,
  tanh: tanh,
  trunc: trunc,
  truncInt: truncInt,
  truncIntExn: truncIntExn,
  truncIntClamped: truncIntClamped,
  truncIntUnsafe: truncIntUnsafe
};

export {
  Float64 ,
}
/* include Not a pure module */
