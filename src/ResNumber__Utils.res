open Js

@scope("Number") @val
external minSafeInteger: float = "MIN_SAFE_INTEGER"

@scope("Number") @val
external maxSafeInteger: float = "MAX_SAFE_INTEGER"

@scope("Number") @val
external isInteger: float => bool = "isInteger"

@scope("Number") @val
external isNaN: float => bool = "isNaN"

@val
external parseInt: string => float = "parseInt"

@val
external parseFloat: string => float = "parseFloat"

external floatToIntUnsafe: float => int = "%identity"

module Int32Range = {
  type t = int

  let minValue = Int.min

  let maxValue = Int.max
}

let inInt32Range = f => f >= Int.min->Int.toFloat && f <= Int.max->Int.toFloat

let stringToInt = s => {
  let i = s->parseInt

  !(i->isNaN) && i->inInt32Range ? Some(i->floatToIntUnsafe) : None
}

let stringToFloat = s => {
  let f = s->parseFloat

  !(f->isNaN) ? Some(f) : None
}

let raiseOverflow = (type a, value: a, range: module(ResNumber__Number.NumberRange)) => {
  let module(Range) = range

  module Data = {
    type valueType = a

    let value = value

    include Range
  }

  raise(ResNumber__Others.NumberOverflow(module(Data)))
}

let raiseEmptyArray = () => invalid_arg("the array is empty")
