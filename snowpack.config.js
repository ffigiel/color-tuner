// Snowpack Configuration File
// See all supported options: https://www.snowpack.dev/reference/configuration

/** @type {import("snowpack").SnowpackUserConfig } */
module.exports = {
  mount: {
    src: '/',
  },
  exclude: [
    '**/LICENSE',
    '**/Makefile',
    '**/README.md',
    '**/elm-analyse.json',
    '**/elm-stuff/**',
    '**/elm.json',
    '**/package-lock.json',
    '**/package.json',
    '**/postBuild.sh',
    '**/snowpack.config.js',
  ],
  plugins: [
    'snowpack-plugin-elm',
    '@snowpack/plugin-webpack',
  ],
  packageOptions: {
  },
  devOptions: {
    open: "none", // don't open a browser with the dev server
  },
  buildOptions: {
    out: "dist",
    baseUrl: "/color-tuner",
  },
};
