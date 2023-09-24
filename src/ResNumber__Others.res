module type OverflowData = {
  type valueType

  let value: valueType

  type t

  let minValue: t

  let maxValue: t
}

exception NumberOverflow(module(OverflowData))

type order = | @as(-1) Less | @as(0) Equal | @as(1) Greater

external orderToInt: order => int = "%identity"

type sign = | @as(-1) Negative | @as(0) Zero | @as(1) Positive

external signToInt: sign => int = "%identity"
