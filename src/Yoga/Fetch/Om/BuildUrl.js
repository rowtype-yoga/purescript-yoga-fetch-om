export function encodeURIComponent_(value) {
  try {
    return encodeURIComponent(value);
  } catch (error) {
    if (!(error instanceof URIError)) throw error;
    return encodeURIComponent(toWellFormed(value));
  }
}

function toWellFormed(value) {
  if (typeof value.toWellFormed === "function") return value.toWellFormed();

  let result = "";
  let sliceStart = 0;
  for (let index = 0; index < value.length; index += 1) {
    const codeUnit = value.charCodeAt(index);
    const previous = index === 0 ? 0 : value.charCodeAt(index - 1);
    const next =
      index + 1 === value.length ? 0 : value.charCodeAt(index + 1);
    const loneHigh =
      codeUnit >= 0xd800 &&
      codeUnit <= 0xdbff &&
      (next < 0xdc00 || next > 0xdfff);
    const loneLow =
      codeUnit >= 0xdc00 &&
      codeUnit <= 0xdfff &&
      (previous < 0xd800 || previous > 0xdbff);

    if (!loneHigh && !loneLow) continue;
    result += value.slice(sliceStart, index) + "\ufffd";
    sliceStart = index + 1;
  }

  return sliceStart === 0 ? value : result + value.slice(sliceStart);
}

function urlSuffixIndex(url) {
  const queryIndex = url.indexOf("?");
  const fragmentIndex = url.indexOf("#");
  return Math.min(
    queryIndex === -1 ? url.length : queryIndex,
    fragmentIndex === -1 ? url.length : fragmentIndex,
  );
}

export function appendPath_(base) {
  return function (path) {
    const suffixIndex = urlSuffixIndex(base);
    const prefix = base.slice(0, suffixIndex).replace(/\/+$/, "");
    return prefix + path + base.slice(suffixIndex);
  };
}


export function substitutePathParam_(name) {
  const escapedName = name.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  const segment = new RegExp(`(^|/):${escapedName}(?=/|[?#]|$)`, "g");
  return function (value) {
    const encodedValue = encodeURIComponent_(value);
    return function (url) {
      const suffixIndex = urlSuffixIndex(url);
      const path = url.slice(0, suffixIndex);
      return (
        path.replace(segment, (_, prefix) => prefix + encodedValue) +
        url.slice(suffixIndex)
      );
    };
  };
}

export function appendQueryString_(url) {
  return function (queryString) {
    if (queryString === "") return url;

    const fragmentIndex = url.indexOf("#");
    const beforeFragment =
      fragmentIndex === -1 ? url : url.slice(0, fragmentIndex);
    const fragment = fragmentIndex === -1 ? "" : url.slice(fragmentIndex);
    const separator = beforeFragment.includes("?")
      ? beforeFragment.endsWith("?") || beforeFragment.endsWith("&")
        ? ""
        : "&"
      : "?";

    return beforeFragment + separator + queryString + fragment;
  };
}
