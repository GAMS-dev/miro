const axios = require('axios');

const REQUEST_TIMEOUT = 10000;

const testAiProviderApiKey = async (provider, apiKey) => {
  if (apiKey.length < 10) {
    return false;
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
      return false;
  }
  try {
    const response = await axios.get(url, {
      headers,
      timeout: REQUEST_TIMEOUT
    });
    return response.status === 200;
  } catch (error) {
    if (error.response) {
      __electronLog.warn(`API rejected key for ${provider}. Status: ${error.response.status}`);
    } else {
      __electronLog.error(`Network or configuration error for ${provider}:`, error.message);
    }
    return false;
  }
}

module.exports = {
  testAiProviderApiKey,
};
