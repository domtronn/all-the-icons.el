module.exports = {
  multipass: true,
  plugins: [
    {
      name: "preset-default",
      params: {
        overrides: {
          inlineStyles: {
            onlyMatchedOnce: false,
          },
        },
      },
    },
    { name: "convertStyleToAttrs", params: { keepImportant: true } },
    { name: "removeAttrs", params: { attrs: "(fill|color)" } },
  ],
};
