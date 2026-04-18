import * as Maybe from "../Data.Maybe/index.js";

export function readOptionalImpl(key) {
  return function (obj) {
    const val = obj[key];
    if (val === undefined) return Maybe.Nothing.value;
    return Maybe.Just.create(val);
  };
}

export function readRequiredField(key) {
  return function (obj) {
    return obj[key];
  };
}
