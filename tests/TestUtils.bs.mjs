// Generated by ReScript, PLEASE EDIT WITH CARE

import * as $$Number from "../src/Number.bs.mjs";
import * as Js_math from "rescript/lib/es6/js_math.js";
import * as Caml_js_exceptions from "rescript/lib/es6/caml_js_exceptions.js";

var minSafeInteger = Number.MIN_SAFE_INTEGER;

var maxSafeInteger = Number.MAX_SAFE_INTEGER;

function assertException(t, f) {
  try {
    f(undefined);
    return t.fail("it should raise an exception");
  }
  catch (exn){
    t.pass(undefined);
    return ;
  }
}

function assertOverflow(t, f) {
  try {
    f(undefined);
    return t.fail("it should raise an exception");
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === $$Number.NumberOverflow) {
      t.pass(undefined);
      return ;
    }
    throw exn;
  }
}

function assertInvalidArgument(t, f) {
  try {
    f(undefined);
    return t.fail("it should raise an exception");
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Invalid_argument") {
      t.pass(undefined);
      return ;
    }
    throw exn;
  }
}

function assertDivisionByZero(t, f) {
  try {
    f(undefined);
    return t.fail("it should raise an exception");
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Division_by_zero") {
      t.pass(undefined);
      return ;
    }
    throw exn;
  }
}

function loopFn(f, n) {
  for(var _for = 0; _for < n; ++_for){
    f(undefined);
  }
}

function loop100Fn(f) {
  loopFn(f, 100);
}

function randomFloat(min, max) {
  return Math.random() * (max - min) + min;
}

function randomInt(min, max) {
  return Js_math.floor_int(randomFloat(min, max));
}

function toUint32(i) {
  return (i >>> 0);
}

export {
  minSafeInteger ,
  maxSafeInteger ,
  assertException ,
  assertOverflow ,
  assertInvalidArgument ,
  assertDivisionByZero ,
  loopFn ,
  loop100Fn ,
  randomFloat ,
  randomInt ,
  toUint32 ,
}
/* minSafeInteger Not a pure module */
