const axios = require('axios');

const TOKEN_EXPIRATION_SECONDS = 604800;
const REQUIRED_SCOPES = 'NAMESPACES JOBS HYPERCUBE USAGE AUTH USERS';

class EngineError extends Error {
  constructor(message, statusCode, field) {
    super(message);

    this.name = 'EngineError';
    this.statusCode = statusCode;
    this.field = field;
  }
}

const getEngineAuthProviders = async (url, signal) => {
  let requestOptions;
  if (signal != null) {
    requestOptions = { signal };
  }
  const authProviderReq = await axios.get(`${url}/auth/providers`, requestOptions);
  if (!authProviderReq.data.find((idp) => idp.is_main_identity_provider)) {
    throw new Error('Not a valid Engine server');
  }
  return authProviderReq.data;
};

const getEngineUserInfo = async (jwt, engineConfig, namespace) => {
  const userInfo = { username: null, is_admin: false, namespace };
  try {
    const userInfoReq = await axios.get(
      `${engineConfig.url}/users/?everyone=false`,
      { headers: { Authorization: `Bearer ${jwt}` } },
    );
    const userInfoRaw = userInfoReq.data[0];
    if (userInfoRaw.deleted === true) {
      throw new EngineError('User is deleted', 200, 'username');
    }
    userInfo.username = userInfoRaw.username;
    if (userInfo.username == null || userInfo.username.length === 0) {
      throw new Error('Invalid username returned by Engine');
    }
    userInfo.is_admin = userInfoRaw.roles.includes('admin');
  } catch (err) {
    if (err?.response?.status === 401) {
      __electronLog.info(`Problems getting user info. Invalid JWT: ${JSON.stringify(err)}`);
      throw new EngineError('Invalid JWT', 401, 'jwt');
    }
    throw new Error(`Problems getting user info. Error message: ${err}`);
  }

  try {
    if (userInfo.namespace == null || userInfo.namespace.length === 0) {
      throw new EngineError('Namespace not found', 404, 'namespace');
    }
    const userPermissionReq = await axios.get(
      `${engineConfig.url}/namespaces/${userInfo.namespace}/permissions?username=${encodeURIComponent(userInfo.username)}`,
      { headers: { Authorization: `Bearer ${jwt}` } },
    );
    if (!userInfo.is_admin && userPermissionReq.permission < 7) {
      throw new EngineError('No permission on namespace', 403, 'namespace');
    }
  } catch (err) {
    if (err?.response?.status === 404) {
      throw new EngineError('Namespace not found', 404, 'namespace');
    }
    throw new Error(`Problems getting namespace permission info. Error message: ${err}`);
  }
  return {
    url: engineConfig.url,
    username: userInfo.username,
    jwt,
    namespace: userInfo.namespace,
    verifypeer: true,
  };
};

const getEngineJwt = async (username, password, loginMethod, engineConfig) => {
  if (loginMethod == null || loginMethod === '_main') {
    const loginReq = await axios.post(`${engineConfig.url}/auth/login`, {
      username,
      password,
      expires_in: TOKEN_EXPIRATION_SECONDS,
      scope: REQUIRED_SCOPES,
    });
    return loginReq.data.token;
  } if (!engineConfig.ldapProviders.includes(loginMethod)) {
    throw new Error(`Invalid login method: ${loginMethod}`);
  }
  const loginReq = await axios.post(`${engineConfig.url}/auth/ldap-providers/${loginMethod}/login`, {
    username,
    password,
    expires_in: TOKEN_EXPIRATION_SECONDS,
    scope: REQUIRED_SCOPES,
  });
  return loginReq.data.token;
};

const refreshEngineJwt = async (url, jwt) => {
  const loginReq = await axios.post(`${url}/auth/`, {
    expires_in: TOKEN_EXPIRATION_SECONDS,
    scope: REQUIRED_SCOPES,
  }, { headers: { Authorization: `Bearer ${jwt}` } });
  return loginReq.data.token;
};

function EngineConfig() {
  this.init();
}
EngineConfig.prototype.init = () => {
  this.url = null;
  this.jwt = null;
  this.ldapProviders = [];
  this.oauthProviders = [];
};

module.exports = {
  getEngineAuthProviders,
  getEngineUserInfo,
  getEngineJwt,
  refreshEngineJwt,
  EngineConfig,
  EngineError,
};
