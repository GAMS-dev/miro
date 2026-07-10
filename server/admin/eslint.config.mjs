import globals from "globals";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { FlatCompat } from "@eslint/eslintrc";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const compat = new FlatCompat({
  baseDirectory: __dirname,
});

export default [
  {
    ignores: [
      "webpack.config.js",
      "webpack.config.cjs",
      "www/**/*"
    ],
  },

  ...compat.extends("airbnb-base"),

  {
    languageOptions: {
      ecmaVersion: "latest",
      globals: {
        ...globals.browser,
        ...globals.jquery,
        Shiny: "readonly"
      },
    },
    rules: {
      "func-names": ["error", "never"],
    },
  },
];
