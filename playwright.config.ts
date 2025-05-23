import { defineConfig } from '@playwright/test';

export default defineConfig({
  testMatch: /e2etests\/.*\.e2e\.ts/,
});
