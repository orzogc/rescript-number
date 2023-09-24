module type NumberZero = {
  type t

  let zero: t
}

module type NumberOne = {
  type t

  let one: t
}

module type NumberRange = {
  type t

  let minValue: t

  let maxValue: t
}

module type NumberComparison = {
  type t

  let compare: (t, t) => option<ResNumber__Others.order>

  let compareExn: (t, t) => ResNumber__Others.order

  let eq: (t, t) => bool

  let ne: (t, t) => bool

  let lt: (t, t) => bool

  let le: (t, t) => bool

  let gt: (t, t) => bool

  let ge: (t, t) => bool

  let min: (t, t) => t

  let minMany: array<t> => option<t>

  let minManyExn: array<t> => t

  let minManyUnsafe: array<t> => t

  let max: (t, t) => t

  let maxMany: array<t> => option<t>

  let maxManyExn: array<t> => t

  let maxManyUnsafe: array<t> => t
}

module type Number = {
  type t

  include NumberZero with type t := t

  include NumberOne with type t := t

  include NumberRange with type t := t

  include NumberComparison with type t := t

  include ResNumber__Operation.NumberOperation with type t := t
}
