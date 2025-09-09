import { defineConfig, globalIgnores } from 'eslint/config';
import js from '@eslint/js';
import globals from 'globals';
import { FlatCompat } from '@eslint/eslintrc';

// Plugins / configs (flat-native)
import importPlugin from 'eslint-plugin-import';
import prettierPlugin from 'eslint-plugin-prettier';
import prettierFlat from 'eslint-config-prettier/flat';

const compat = new FlatCompat({
  recommendedConfig: js.configs.recommended,
});

export default defineConfig([
  globalIgnores([
    'node_modules/**',
    'dist/**',
    'build/**',
    'out/**',
    'src/**',
    'server/**',
    'r-src/**',
    'doc/**',
    'build/lib_devel/**',
  ]),
  // ---------- Shared base (applies to all JS) ----------
  {
    files: ['**/*.js', '**/*.mjs', '**/*.cjs'],
    languageOptions: {
      ecmaVersion: 'latest',
      sourceType: 'module',
      globals: {
        ...globals.es2024,
        __electronLog: 'readonly',
      },
    },
    plugins: {
      import: importPlugin,
      prettier: prettierPlugin,
    },
    // Keep Airbnb-base via compat; Prettier last to disable conflicting rules
    extends: [
      ...compat.extends('airbnb-base'),
      prettierFlat,
      // If you want Prettier to run as an ESLint rule, uncomment:
      // prettierPlugin.configs.recommended,
    ],
    settings: {
      'import/resolver': {
        node: { extensions: ['.js', '.mjs', '.cjs', '.json'] },
      },
      'import/core-modules': ['electron'],
    },
    rules: {
      'no-multi-str': 'off',
      'no-underscore-dangle': ['error', { allowAfterThis: true }],

      // Omit .js on local imports, ignore packages
      'import/extensions': ['error', 'ignorePackages', { js: 'never' }],

      // Only allow dev deps in non-prod contexts
      'import/no-extraneous-dependencies': [
        'error',
        {
          devDependencies: [
            '**/*.test.js',
            '**/*.spec.js',
            '**/test/**',
            '**/tests/**',
            '**/scripts/**',
            'electron-builder.*',
            'vite.config.*',
            'webpack.config.*',
            'eslint.config.*',
          ],
        },
      ],

      'no-unused-vars': [
        'error',
        {
          caughtErrors: 'none',
        },
      ],
    },
  },

  // ---------- Electron MAIN (Node context, ESM) ----------
  {
    files: ['main.js', 'components/**/*.js'],
    languageOptions: {
      sourceType: 'module',
      globals: { ...globals.node, ...globals.es2024 },
    },
    rules: {
      // Main commonly touches build/dev deps; relax if you like:
      'import/extensions': ['error', 'ignorePackages', { js: 'always' }],
      // "import/no-extraneous-dependencies": "off",
      // In Node ESM, you must include .js (or the real ext) on local imports
    },
  },

  // ---------- Electron RENDERER (Browser context) ----------
  {
    files: ['renderer/**/*.js'],
    languageOptions: {
      sourceType: 'module',
      globals: { ...globals.browser, ...globals.es2024 },
    },
    rules: {
      // Prevent accidental Node-only APIs in renderer
      // (flip to "error" once you’re confident)
      'import/no-nodejs-modules': 'warn',
    },
  },

  // ignore unused vars in scripts/**
  {
    files: ['scripts/**/*.js'],
    rules: {
      'no-console': 'off',
    },
  },

  {
    files: ['eslint.config.*'],
    rules: {
      'import/no-unresolved': 'off',
    },
  },
]);
