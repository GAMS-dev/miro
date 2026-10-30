// Interactive Microsoft Entra ID sign-in for the AI assistant's Azure AI
// Foundry provider on the desktop: the user signs in with their own account
// in the system browser (authorization code flow with PKCE, handled by MSAL
// Node on a loopback redirect), so nothing like an API key or client secret
// has to be stored. Access to the Foundry resource is then governed by the
// user's own RBAC role on it.
const fs = require('node:fs/promises');
const path = require('node:path');
const { PublicClientApplication, InteractionRequiredAuthError } = require('@azure/msal-node');

const AZURE_AI_SCOPES = ['https://ai.azure.com/.default'];
// the user may simply close the browser tab, and MSAL would then wait forever
const SIGN_IN_TIMEOUT_MS = 5 * 60 * 1000;
const REFRESH_INTERVAL_MS = 5 * 60 * 1000;
// refresh well before expiry, so an R process never picks up a token that
// runs out in the middle of a conversation
const REFRESH_MARGIN_MS = 15 * 60 * 1000;

const signInPage = (message) => `<!DOCTYPE html><html><head><meta charset="utf-8">
<title>GAMS MIRO</title></head><body style="font-family: sans-serif; margin: 3em">
<h2>GAMS MIRO</h2><p>${message}</p></body></html>`;

const describeSignInError = (err) => {
  const message = String(err?.errorMessage || err?.message || err || 'Unknown error').split(/\r?\n/)[0].trim();
  const code = err?.errorCode;
  return code && !message.includes(code) ? `${code}: ${message}` : message;
};

const firstAccount = async (client) => {
  const accounts = await client.getTokenCache().getAllAccounts();
  return accounts[0] ?? null;
};

const isUserSignInEntry = (entry) => entry?.provider === 'azure' && entry.authMode === 'userSignIn';

class AzureUserAuth {
  #cacheFile;

  #tokenFile;

  #safeStorage;

  #openExternal;

  #log;

  #clients = new Map();

  #refreshTimer = null;

  #createClient;

  // `createClient` exists for the tests; it defaults to MSAL's own client
  constructor({
    storageDir, safeStorage, openExternal, log,
    createClient = (msalConfig) => new PublicClientApplication(msalConfig),
  }) {
    this.#createClient = createClient;
    this.#cacheFile = path.join(storageDir, 'azure-msal-cache.bin');
    this.#tokenFile = path.join(storageDir, 'azure-access-token.json');
    this.#safeStorage = safeStorage;
    this.#openExternal = openExternal;
    this.#log = log;
  }

  get tokenFile() {
    return this.#tokenFile;
  }

  static findUserSignInEntry(aiAssistantConfig) {
    return aiAssistantConfig?.apiKeys?.find(isUserSignInEntry) ?? null;
  }

  // The MSAL token cache (refresh token included) is only ever written to
  // disk encrypted with the OS keychain via Electron's safeStorage. Without
  // it, the cache stays in memory and the user signs in once per session.
  #cachePlugin() {
    return {
      beforeCacheAccess: async (cacheContext) => {
        if (!(await this.#safeStorage.isAsyncEncryptionAvailable())) return;
        try {
          const encrypted = await fs.readFile(this.#cacheFile);
          const { result } = await this.#safeStorage.decryptStringAsync(encrypted);
          cacheContext.tokenCache.deserialize(result);
        } catch (err) {
          if (err.code !== 'ENOENT') {
            this.#log.warn(`Could not read the Azure sign-in cache: ${err.message}`);
          }
        }
      },
      afterCacheAccess: async (cacheContext) => {
        if (!cacheContext.cacheHasChanged) return;
        if (!(await this.#safeStorage.isAsyncEncryptionAvailable())) {
          this.#log.warn('OS encryption is unavailable: Azure sign-in is kept in memory only.');
          return;
        }
        await fs.mkdir(path.dirname(this.#cacheFile), { recursive: true });
        await fs.writeFile(
          this.#cacheFile,
          await this.#safeStorage.encryptStringAsync(cacheContext.tokenCache.serialize()),
          { mode: 0o600 },
        );
      },
    };
  }

  #client({ tenantId, clientId }) {
    if (!tenantId || !clientId) {
      throw new Error('Azure tenant or client id is missing.');
    }
    const cacheKey = `${tenantId}|${clientId}`;
    if (!this.#clients.has(cacheKey)) {
      this.#clients.set(cacheKey, this.#createClient({
        auth: {
          clientId,
          authority: `https://login.microsoftonline.com/${encodeURIComponent(tenantId)}`,
        },
        cache: { cachePlugin: this.#cachePlugin() },
      }));
    }
    return this.#clients.get(cacheKey);
  }

  // Opens the system browser. Resolves to the signed-in account's username.
  async signIn(config) {
    const client = this.#client(config);
    let timeoutId;
    const timeout = new Promise((_, reject) => {
      timeoutId = setTimeout(() => reject(new Error('Sign-in timed out.')), SIGN_IN_TIMEOUT_MS);
    });
    try {
      const result = await Promise.race([
        client.acquireTokenInteractive({
          scopes: AZURE_AI_SCOPES,
          prompt: 'select_account',
          responseMode: 'query',
          openBrowser: (url) => this.#openExternal(url),
          successTemplate: signInPage('You are signed in. You can close this tab and return to GAMS MIRO.'),
          errorTemplate: signInPage('Sign-in failed. You can close this tab; the reason is shown in GAMS MIRO.'),
        }),
        timeout,
      ]);
      await this.#writeTokenFile(result);
      return result.account?.username ?? null;
    } catch (err) {
      throw new Error(describeSignInError(err));
    } finally {
      clearTimeout(timeoutId);
    }
  }

  // Silent only: never opens a browser. Resolves to null when the user has to
  // sign in (again), e.g. because they never did or the refresh token expired.
  async getAccessToken(config) {
    const client = this.#client(config);
    const account = await firstAccount(client);
    if (!account) return null;
    try {
      return await client.acquireTokenSilent({ account, scopes: AZURE_AI_SCOPES });
    } catch (err) {
      if (err instanceof InteractionRequiredAuthError) return null;
      throw err;
    }
  }

  async getSignedInUser(config) {
    const account = await firstAccount(this.#client(config));
    return account?.username ?? null;
  }

  async signOut(config) {
    const client = this.#client(config);
    const cache = client.getTokenCache();
    const accounts = await cache.getAllAccounts();
    await Promise.all(accounts.map((account) => cache.removeAccount(account)));
    await this.removeTokenFile();
  }

  // Refreshes the token file for the R processes unless it is still fresh.
  async refreshTokenFile(aiAssistantConfig) {
    const entry = AzureUserAuth.findUserSignInEntry(aiAssistantConfig);
    if (!entry) {
      await this.removeTokenFile();
      return;
    }
    try {
      const current = JSON.parse(await fs.readFile(this.#tokenFile, 'utf8'));
      if (Date.parse(current.expiresOn) - Date.now() > REFRESH_MARGIN_MS) return;
    } catch (_) {
      // missing or unreadable: fetch a new one below
    }
    try {
      const result = await this.getAccessToken(entry);
      if (result) {
        await this.#writeTokenFile(result);
      } else {
        this.#log.warn('Azure sign-in for the AI assistant has expired, the user has to sign in again.');
      }
    } catch (err) {
      this.#log.warn(`Could not refresh the Azure access token: ${err.message}`);
    }
  }

  startRefresher(getAiAssistantConfig) {
    this.stopRefresher();
    const refresh = async () => this.refreshTokenFile(await getAiAssistantConfig());
    refresh();
    this.#refreshTimer = setInterval(refresh, REFRESH_INTERVAL_MS);
  }

  stopRefresher() {
    if (this.#refreshTimer) clearInterval(this.#refreshTimer);
    this.#refreshTimer = null;
  }

  async removeTokenFile() {
    await fs.rm(this.#tokenFile, { force: true });
  }

  // written to a temp file first and renamed, so an R process reading it
  // concurrently never sees a half-written token
  async #writeTokenFile(result) {
    await fs.mkdir(path.dirname(this.#tokenFile), { recursive: true });
    const tmpFile = `${this.#tokenFile}.${process.pid}.tmp`;
    await fs.writeFile(tmpFile, JSON.stringify({
      accessToken: result.accessToken,
      expiresOn: new Date(result.expiresOn).toISOString(),
    }), { mode: 0o600 });
    await fs.rename(tmpFile, this.#tokenFile);
  }
}

module.exports = { AzureUserAuth, AZURE_AI_SCOPES };
