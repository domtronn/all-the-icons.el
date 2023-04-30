module.exports = {
  plugins: [
    "preset-default",
    "convertStyleToAttrs",
    { name: "removeAttrs", params: { attrs: "(fill|color)" } },
  ],
};
