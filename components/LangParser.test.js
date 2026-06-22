import { test, expect } from '@playwright/test';
import LangParser from './LangParser.js';

test('LangParser initializes without throwing errors for en, de, cn', async () => {
  ['en', 'de', 'cn'].forEach(lang => {
    expect(() => {
      const parser = new LangParser(lang);
      return parser;
    }).not.toThrow();
  });
});
