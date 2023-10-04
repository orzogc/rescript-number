module type NumberAddition = {
  type t

  let add: (t, t) => option<t>

  let addExn: (t, t) => t

  let addClamped: (t, t) => t

  let addUnsafe: (t, t) => t
}

module type NumberSubtraction = {
  type t

  let sub: (t, t) => option<t>

  let subExn: (t, t) => t

  let subClamped: (t, t) => t

  let subUnsafe: (t, t) => t
}

module type NumberMultiplication = {
  type t

  let mul: (t, t) => option<t>

  let mulExn: (t, t) => t

  let mulClamped: (t, t) => t

  let mulUnsafe: (t, t) => t
}

module type NumberDivision = {
  type t

  let div: (t, t) => option<t>

  let divExn: (t, t) => t

  let divUnsafe: (t, t) => t
}

module type NumberRemainder = {
  type t

  let rem: (t, t) => option<t>

  let remExn: (t, t) => t

  let remUnsafe: (t, t) => t
}

module type NumberSum = {
  type t

  let sum: array<t> => option<t>

  let sumExn: array<t> => t

  let sumUnsafe: array<t> => t
}

module type NumberOperation = {
  type t

  include NumberAddition with type t := t

  include NumberSubtraction with type t := t

  include NumberMultiplication with type t := t

  include NumberDivision with type t := t

  include NumberRemainder with type t := t

  include NumberSum with type t := t
}

module type NumberIncDec = {
  type t

  let inc: t => option<t>

  let incExn: t => t

  let incUnsafe: t => t

  let dec: t => option<t>

  let decExn: t => t

  let decUnsafe: t => t
}

module MakeNumberSum = (Add: NumberAddition): (NumberSum with type t = Add.t) => {
  type t = Add.t

  open Belt

  let rec sum_ = (arr, sum, i) =>
    switch arr->Array.get(i) {
    | Some(n) =>
      switch sum->Add.add(n) {
      | Some(s) => sum_(arr, s, i + 1)
      | None => None
      }
    | None => Some(sum)
    }

  let sum = arr => arr->Array.length > 0 ? sum_(arr, arr->Array.getUnsafe(0), 1) : None

  let sumExn = arr => {
    let len = arr->Array.length

    if len > 0 {
      let s = ref(arr->Array.getUnsafe(0))
      for i in 1 to len - 1 {
        s := s.contents->Add.addExn(arr->Array.getUnsafe(i))
      }

      s.contents
    } else {
      invalid_arg("the array is empty")
    }
  }

  let sumUnsafe = arr => {
    let s = ref(arr->Array.getUnsafe(0))
    for i in 1 to arr->Array.length - 1 {
      s := s.contents->Add.addUnsafe(arr->Array.getUnsafe(i))
    }

    s.contents
  }
}
