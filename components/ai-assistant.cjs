const axios = require('axios');

const REQUEST_TIMEOUT = 10000;
const DEPLOYMENT_PROBE_TIMEOUT = 60000;
const PROBE_MAX_TOKENS = 2048;

// Callers must be able to tell "the provider rejected this key" apart from "we could
// not reach the provider": the first means the key is bad and must not be stored, the
// second says nothing about the key, so an already stored one has to be left alone.
const KEY_VALID = 'valid';
const KEY_INVALID = 'invalid';
const KEY_UNREACHABLE = 'unreachable';

// only these say something about the key itself; rate limits, outages etc. do not
const isKeyRejected = (provider, status) => status === 401 || status === 403
  || (provider === 'google' && status === 400);

const testAiProviderApiKey = async (provider, apiKey) => {
  if (apiKey.length < 10) {
    return KEY_INVALID;
  }
  let url = '';
  let headers = {};

  switch (provider) {
    case 'openai':
      url = 'https://api.openai.com/v1/models';
      headers = { 'Authorization': `Bearer ${apiKey}` };
      break;
    case 'anthropic':
      url = 'https://api.anthropic.com/v1/models';
      headers = {
        'x-api-key': apiKey,
        'anthropic-version': '2023-06-01'
      };
      break;
    case 'google':
      url = 'https://generativelanguage.googleapis.com/v1beta/models';
      headers = { 'x-goog-api-key': apiKey };
      break;
    default:
      return KEY_INVALID;
  }
  try {
    const response = await axios.get(url, {
      headers,
      timeout: REQUEST_TIMEOUT
    });
    return response.status === 200 ? KEY_VALID : KEY_UNREACHABLE;
  } catch (error) {
    if (error.response && isKeyRejected(provider, error.response.status)) {
      __electronLog.warn(`API rejected key for ${provider}. Status: ${error.response.status}`);
      return KEY_INVALID;
    }
    __electronLog.error(`Could not verify key for ${provider}:`, error.response
      ? `HTTP ${error.response.status}` : error.message);
    return KEY_UNREACHABLE;
  }
}

const isValidAzureEndpoint = (endpoint) => /^https:\/\/[^/?#@\s]+(\/[^?#@\s]*)?$/.test(endpoint);

// Accepts what users paste: the resource root, a Foundry project endpoint,
// the documented `.../openai/v1/` base URL or a full `/chat/completions` (or `/responses`) route.
const normalizeAzureEndpoint = (endpoint) => String(endpoint || '')
  .trim()
  .replace(/\/+$/, '')
  .replace(/\/openai(\/v1(\/chat\/completions|\/responses)?)?$/, '');

const buildAzureAuthHeaders = (config, accessToken) => {
  if (config.authMode === 'userSignIn') {
    // obtained by the caller from the signed-in user (see azure-user-auth.cjs)
    return { Authorization: `Bearer ${accessToken}` };
  }
  return { 'api-key': config.key };
};


const extractProbeErrorMessage = (error) => {
  const body = error.response?.data;
  if (typeof body === 'string' && body) return body;
  return body?.error?.message ?? body?.message ?? null;
};

const probeCustomProviderDeployments = async (providerLabel, deployments, buildRequest) => {
  const candidates = deployments.filter((deployment) => String(deployment.id || '').trim());
  const failedIds = new Set();

  /* eslint-disable no-restricted-syntax, no-await-in-loop */
  for (const deployment of candidates) {
    const id = String(deployment.id).trim();
    const { url, headers, body } = buildRequest(deployment);
    try {
      await axios.post(url, body, {
        headers: { ...headers, 'Content-Type': 'application/json' },
        timeout: DEPLOYMENT_PROBE_TIMEOUT,
      });
      const models = candidates
        .filter((d) => !failedIds.has(String(d.id).trim()))
        .map((d) => ({ id: String(d.id).trim(), label: String(d.label || '').trim() || String(d.id).trim() }));
      return { status: KEY_VALID, models };
    } catch (error) {
      const status = error.response?.status;
      if (!error.response) {
        __electronLog.error(`Could not reach ${providerLabel} deployment ${id}:`, error.message);
        return { status: KEY_UNREACHABLE, models: [] };
      }
      if (status === 401) {
        __electronLog.warn(`${providerLabel} rejected the credential. Status: ${status}`);
        return { status: KEY_INVALID, models: [] };
      }
      const detail = extractProbeErrorMessage(error);
      if (status === 408 || status === 429 || status >= 500) {
        __electronLog.warn(
          `Could not validate ${providerLabel} deployment ${id}. Status: ${status}${detail ? ` - ${detail}` : ''}`,
        );
        return { status: KEY_UNREACHABLE, models: [] };
      }
      __electronLog.warn(
        `${providerLabel} deployment ${id} is not usable. Status: ${status}${detail ? ` - ${detail}` : ''}`,
      );
      failedIds.add(id);
    }
  }
  /* eslint-enable no-restricted-syntax, no-await-in-loop */
  return { status: KEY_INVALID, models: [] };
};

// `accessToken` is only used, and required, with authMode 'userSignIn'
const testAzureFoundryConnection = async (config, deployments = [], { accessToken } = {}) => {
  const isUserSignIn = config?.authMode === 'userSignIn';
  if (!['apiKey', 'userSignIn', undefined].includes(config?.authMode)) {
    __electronLog.warn(`Unsupported Azure auth mode rejected: ${config?.authMode}`);
    return { status: KEY_INVALID, models: [] };
  }
  if (!config?.endpoint || (isUserSignIn ? !accessToken : !config?.key)) {
    return { status: KEY_INVALID, models: [] };
  }
  const endpoint = normalizeAzureEndpoint(config.endpoint);
  if (!isValidAzureEndpoint(endpoint)) {
    __electronLog.warn('Azure endpoint rejected: not an https:// URL without credentials, query or fragment');
    return { status: KEY_INVALID, models: [] };
  }
  if (isUserSignIn && !(config.tenantId && config.clientId)) {
    __electronLog.warn('Azure Entra ID tenant or client id is missing');
    return { status: KEY_INVALID, models: [] };
  }
  const headers = buildAzureAuthHeaders(config, accessToken);

  const messages = [{ role: 'user', content: 'ping' }];
  // Claude deployments only speak the Anthropic Messages API
  return probeCustomProviderDeployments('Azure', deployments, (deployment) => (
    /claude/i.test(deployment.id)
      ? {
        url: `${endpoint}/anthropic/v1/messages`,
        headers: { ...headers, 'anthropic-version': '2023-06-01' },
        body: { model: deployment.id, max_tokens: PROBE_MAX_TOKENS, messages },
      }
      : {
        url: `${endpoint}/openai/v1/chat/completions`,
        headers,
        body: { model: deployment.id, messages },
      }));
};

// the region becomes part of the host the key is sent to
const isValidBedrockRegion = (region) => /^[a-z]{2}(-gov)?-[a-z]+-[0-9]$/.test(region ?? '');

const testBedrockConnection = async (config, deployments = []) => {
  if (!config?.key || !isValidBedrockRegion(config.region)) {
    return { status: KEY_INVALID, models: [] };
  }
  return probeCustomProviderDeployments('Amazon Bedrock', deployments, (deployment) => ({
    url: `https://bedrock-runtime.${config.region}.amazonaws.com/model/${encodeURIComponent(String(deployment.id).trim())}/converse`,
    headers: { Authorization: `Bearer ${config.key}` },
    body: {
      messages: [{ role: 'user', content: [{ text: 'ping' }] }],
      inferenceConfig: { maxTokens: PROBE_MAX_TOKENS },
    },
  }));
};

module.exports = {
  testAiProviderApiKey,
  testAzureFoundryConnection,
  testBedrockConnection,
  KEY_VALID,
  KEY_INVALID,
  KEY_UNREACHABLE,
};
