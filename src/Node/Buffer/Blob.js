import * as buffer from "buffer";

const fromSources = function (sources, options) {
  if (options === null) {
    return new buffer.Blob(sources);
  } else {
    return new buffer.Blob(sources, options);
  }
};

export const fromStringsImpl = function (sources) {
  return function (options) {
    return fromSources(sources, options);
  };
};


export const fromArrayBuffersImpl = function (sources) {
  return function (options) {
    return fromSources(sources, options);
  };
};

export const fromBlobsImpl = function (sources) {
  return function (options) {
    return fromSources(sources, options);
  };
};

export const fromDataViewImpl = function (sources) {
  return function (options) {
    return fromSources(sources, options);
  };
};

export const toArrayBuffer = function (blob) {
  return function () {
    return blob.arrayBuffer();
  };
};

export const size = function (blob) {
  return blob.size;
};

export const slice = function (start) {
  return function (end) {
    return function (blob) {
      return blob.slice(start, end);
    };
  };
};

export const stream = function (blob) {
  return function () {
    return blob.stream();
  };
};

export const text = function (blob) {
  return function () {
    return blob.text();
  };
};

export const tpe = function (blob) {
  return blob.type;
}; 
