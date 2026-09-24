const axios = require('axios');

const REQUEST_TIMEOUT = 10000;

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

module.exports = {
  testAiProviderApiKey,
  KEY_VALID,
  KEY_INVALID,
  KEY_UNREACHABLE,
};
