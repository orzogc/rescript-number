// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Js_int from "rescript/lib/es6/js_int.js";
import * as PervasivesU from "rescript/lib/es6/pervasivesU.js";
import * as ResNumber__Others from "./ResNumber__Others.res.mjs";

var Int32Range = {
  minValue: Js_int.min,
  maxValue: Js_int.max
};

function inInt32Range(f) {
  if (f >= Js_int.min) {
    return f <= Js_int.max;
  } else {
    return false;
  }
}

function stringToFloat(s) {
  var f = parseFloat(s);
  if (!Number.isNaN(f)) {
    return f;
  }
  
}

function raiseOverflow(value, range) {
  var Data_minValue = range.minValue;
  var Data_maxValue = range.maxValue;
  var Data = {
    value: value,
    minValue: Data_minValue,
    maxValue: Data_maxValue
  };
  throw {
        RE_EXN_ID: ResNumber__Others.NumberOverflow,
        _1: Data,
        Error: new Error()
      };
}

function raiseEmptyArray() {
  return PervasivesU.invalid_arg("the array is empty");
}

export {
  Int32Range ,
  inInt32Range ,
  stringToFloat ,
  raiseOverflow ,
  raiseEmptyArray ,
}
/* No side effect */