exports.btoa = function btoa(str) {
  return Buffer.from(str, "binary").toString("base64");
};
