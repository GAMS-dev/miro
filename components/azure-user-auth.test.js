import { createRequire } from 'node:module';
import os from 'node:os';
import path from 'node:path';
import fs from 'node:fs/promises';
import { test, expect } from '@playwright/test';

const require = createRequire(import.meta.url);
const { InteractionRequiredAuthError } = require('@azure/msal-node');

const { AzureUserAuth } = require('./azure-user-auth.cjs');

const TENANT = '11111111-2222-3333-4444-555555555555';
const CLIENT = '66666666-7777-8888-9999-000000000000';
const signInEntry = {
  provider: 'azure', authMode: 'userSignIn', tenantId: TENANT, clientId: CLIENT,
};
const aiConfig = { apiKeys: [signInEntry] };

const silentLog = {
  debug() {}, info() {}, warn() {}, error() {},
};
const fakeSafeStorage = (available = true) => ({
  isAsyncEncryptionAvailable: async () => available,
  encryptStringAsync: async (text) => Buffer.from(`enc:${text}`),
  decryptStringAsync: async (buffer) => ({
    shouldReEncrypt: false,
    result: buffer.toString().replace(/^enc:/, ''),
  }),
});

// A stand-in for MSAL's PublicClientApplication that drives the cache plugin
// the way MSAL does, so the persistence path is exercised too.
const fakeClientFactory = (behaviour = {}) => {
  const created = [];
  const factory = (msalConfig) => {
    const plugin = msalConfig.cache.cachePlugin;
    const state = { accounts: behaviour.accounts ?? [], serialized: '' };
    const tokenCache = {
      serialize: () => JSON.stringify(state.accounts),
      deserialize: (text) => { state.serialized = text; },
      getAllAccounts: async () => {
        await plugin.beforeCacheAccess({ tokenCache, cacheHasChanged: false });
        return state.accounts;
      },
      removeAccount: async (account) => {
        state.accounts = state.accounts.filter((a) => a !== account);
        await plugin.afterCacheAccess({ tokenCache, cacheHasChanged: true });
      },
    };
    const client = {
      msalConfig,
      state,
      getTokenCache: () => tokenCache,
      acquireTokenSilent: behaviour.acquireTokenSilent ?? (async () => ({
        accessToken: 'silent-token', expiresOn: new Date(Date.now() + 3600e3),
      })),
      acquireTokenInteractive: behaviour.acquireTokenInteractive ?? (async (request) => {
        state.interactiveRequest = request;
        await request.openBrowser('https://login.microsoftonline.com/authorize?x');
        state.accounts = [{ username: 'user@contoso.com' }];
        await plugin.afterCacheAccess({ tokenCache, cacheHasChanged: true });
        return {
          accessToken: 'interactive-token',
          expiresOn: new Date(Date.now() + 3600e3),
          account: state.accounts[0],
        };
      }),
    };
    created.push(client);
    return client;
  };
  factory.created = created;
  return factory;
};

const newAuth = async (options = {}) => {
  const storageDir = await fs.mkdtemp(path.join(os.tmpdir(), 'miro-azure-auth-'));
  const opened = [];
  const auth = new AzureUserAuth({
    storageDir,
    safeStorage: options.safeStorage ?? fakeSafeStorage(),
    openExternal: async (url) => { opened.push(url); },
    log: silentLog,
    createClient: options.createClient ?? fakeClientFactory(options.behaviour),
  });
  return { auth, storageDir, opened };
};

const readTokenFile = async (auth) => JSON.parse(await fs.readFile(auth.tokenFile, 'utf8'));

test('sign-in opens the system browser against the configured tenant', async () => {
  const createClient = fakeClientFactory();
  const { auth, opened } = await newAuth({ createClient });
  expect(await auth.signIn(signInEntry)).toBe('user@contoso.com');
  expect(opened).toEqual(['https://login.microsoftonline.com/authorize?x']);
  expect(createClient.created[0].msalConfig.auth).toEqual({
    clientId: CLIENT,
    authority: `https://login.microsoftonline.com/${TENANT}`,
  });
  // the R processes get the fresh token right away
  expect((await readTokenFile(auth)).accessToken).toBe('interactive-token');
});

test('the token cache is only persisted encrypted', async () => {
  const { auth, storageDir } = await newAuth();
  await auth.signIn(signInEntry);
  const raw = await fs.readFile(path.join(storageDir, 'azure-msal-cache.bin'), 'utf8');
  expect(raw.startsWith('enc:')).toBe(true);
});

test('without OS encryption the token cache is never written to disk', async () => {
  const { auth, storageDir } = await newAuth({ safeStorage: fakeSafeStorage(false) });
  await auth.signIn(signInEntry);
  await expect(fs.access(path.join(storageDir, 'azure-msal-cache.bin'))).rejects.toThrow();
});

test('the token file is private to the user', async () => {
  test.skip(process.platform === 'win32', 'POSIX file modes only');
  const { auth } = await newAuth();
  await auth.signIn(signInEntry);
  // eslint-disable-next-line no-bitwise
  expect((await fs.stat(auth.tokenFile)).mode & 0o777).toBe(0o600);
});

test('silent token acquisition never opens a browser and reports a required sign-in as null', async () => {
  const { auth, opened } = await newAuth({ behaviour: { accounts: [] } });
  expect(await auth.getAccessToken(signInEntry)).toBeNull();

  const needsInteraction = await newAuth({
    behaviour: {
      accounts: [{ username: 'u' }],
      acquireTokenSilent: async () => {
        throw new InteractionRequiredAuthError('interaction_required');
      },
    },
  });
  expect(await needsInteraction.auth.getAccessToken(signInEntry)).toBeNull();
  expect(opened).toEqual([]);
  expect(needsInteraction.opened).toEqual([]);
});

test('the token file is refreshed only when close to expiry', async () => {
  let calls = 0;
  const { auth } = await newAuth({
    behaviour: {
      accounts: [{ username: 'u' }],
      acquireTokenSilent: async () => {
        calls += 1;
        return { accessToken: `tok-${calls}`, expiresOn: new Date(Date.now() + 3600e3) };
      },
    },
  });
  await auth.refreshTokenFile(aiConfig);
  await auth.refreshTokenFile(aiConfig);
  expect(calls).toBe(1);
  expect((await readTokenFile(auth)).accessToken).toBe('tok-1');

  await fs.writeFile(auth.tokenFile, JSON.stringify({
    accessToken: 'old', expiresOn: new Date(Date.now() + 60e3).toISOString(),
  }));
  await auth.refreshTokenFile(aiConfig);
  expect((await readTokenFile(auth)).accessToken).toBe('tok-2');
});

test('the token file is removed once Microsoft sign-in is no longer configured', async () => {
  const { auth } = await newAuth({ behaviour: { accounts: [{ username: 'u' }] } });
  await auth.refreshTokenFile(aiConfig);
  await auth.refreshTokenFile({ apiKeys: [{ provider: 'openai', key: 'sk-x' }] });
  await expect(fs.access(auth.tokenFile)).rejects.toThrow();
});

test('sign-out forgets the account and removes the token file', async () => {
  const { auth } = await newAuth();
  await auth.signIn(signInEntry);
  expect(await auth.getSignedInUser(signInEntry)).toBe('user@contoso.com');
  await auth.signOut(signInEntry);
  expect(await auth.getSignedInUser(signInEntry)).toBeNull();
  await expect(fs.access(auth.tokenFile)).rejects.toThrow();
});

test('missing tenant or client ids never reach MSAL', async () => {
  const createClient = fakeClientFactory();
  const { auth } = await newAuth({ createClient });
  await expect(auth.signIn({ tenantId: '', clientId: CLIENT })).rejects.toThrow('missing');
  await expect(auth.signIn({ tenantId: TENANT, clientId: '' })).rejects.toThrow('missing');
  expect(createClient.created).toEqual([]);
});

test('sign-in returns the result in the redirect query, not as a non-secure form post', async () => {
  const createClient = fakeClientFactory();
  const { auth } = await newAuth({ createClient });
  await auth.signIn(signInEntry);
  expect(createClient.created[0].state.interactiveRequest.responseMode).toBe('query');
});

test('a failed sign-in reports Entra\'s reason, without trace ids', async () => {
  const createClient = fakeClientFactory({
    acquireTokenInteractive: async () => {
      throw Object.assign(new Error('invalid_request'), {
        errorCode: 'invalid_request',
        errorMessage: 'AADSTS50011: The redirect URI does not match.\r\nTrace ID: abc\r\nCorrelation ID: def',
      });
    },
  });
  const { auth } = await newAuth({ createClient });
  await expect(auth.signIn(signInEntry)).rejects.toThrow(
    /^invalid_request: AADSTS50011: The redirect URI does not match\.$/,
  );
});
