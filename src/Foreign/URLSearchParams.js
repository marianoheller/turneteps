"use strict";

exports.fromString = function (x) {
  return new URLSearchParams(x);
};

exports.toString = function (p) {
  return p.toString();
};
