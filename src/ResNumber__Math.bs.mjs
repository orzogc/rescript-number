// Generated by ReScript, PLEASE EDIT WITH CARE


function MakeIntegerMath(M) {
  var pow_ = function (base, exp) {
    return Math.pow(M.toFloat(base), M.toFloat(exp));
  };
  var pow = function (base, exp) {
    return M.fromFloat(pow_(base, exp));
  };
  var powExn = function (base, exp) {
    return M.fromFloatExn(pow_(base, exp));
  };
  var powUnsafe = function (base, exp) {
    return M.fromFloatUnsafe(pow_(base, exp));
  };
  var random = function (min, max) {
    var min_ = M.toFloat(min);
    var max_ = M.toFloat(max);
    return M.fromFloatClamped(Math.random() * (max_ - min_) + min_);
  };
  return {
          clz: M.clz,
          imul: M.imul,
          pow: pow,
          powExn: powExn,
          powUnsafe: powUnsafe,
          random: random
        };
}

export {
  MakeIntegerMath ,
}
/* No side effect */