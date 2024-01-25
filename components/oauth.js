const OAuthClient = class {
  #keyPair;

  constructor(rsaKeyPair) {
    if (typeof rsaKeyPair === 'undefined') {
      throw new Error('Cannot be called directly');
    }
    this.#keyPair = rsaKeyPair;
  }

  static async build() {
    const rsaKeyPair = await window.crypto.subtle.generateKey(
      {
        name: 'RSA-OAEP',
        modulusLength: 2048,
        publicExponent: new Uint8Array([0x01, 0x00, 0x01]),
        hash: 'SHA-256',
      },
      true,
      ['encrypt', 'decrypt'],
    );
    return new OAuthClient(rsaKeyPair);
  }

  static #str2ab(str) {
    const buf = new ArrayBuffer(str.length);
    const bufView = new Uint8Array(buf);
    for (let i = 0, strLen = str.length; i < strLen; i += 1) {
      bufView[i] = str.charCodeAt(i);
    }
    return buf;
  }

  static #ab2str(buf) {
    return String.fromCharCode.apply(null, new Uint8Array(buf));
  }

  static #base64URLEncode(buffer) {
    return window.btoa(buffer)
      .replace(/\+/g, '-')
      .replace(/\//g, '_')
      .replace(/=/g, '');
  }

  static #base64URLDecode(input) {
    let output = input
      .replace(/-/g, '+')
      .replace(/_/g, '/');
    const pad = output.length % 4;
    if (pad) {
      if (pad === 1) {
        throw new Error('InvalidLengthError: Input base64url string is the wrong length to determine padding');
      }
      output += new Array(5 - pad).join('=');
    }
    return window.atob(output);
  }

  async getB64URLEncodedPublicKey() {
    const publicKeyRaw = await window.crypto.subtle.exportKey('spki', this.#keyPair.publicKey);
    const publicKeyString = OAuthClient.#ab2str(publicKeyRaw);
    return OAuthClient.#base64URLEncode(publicKeyString);
  }

  async decryptData(b64URLEncodedData, aesKeyB64, aesIvB64) {
    const binaryEncryptedAES = OAuthClient.#str2ab(OAuthClient.#base64URLDecode(aesKeyB64));
    const binaryAES = await window.crypto.subtle.decrypt(
      {
        name: 'RSA-OAEP',
      },
      this.#keyPair.privateKey,
      binaryEncryptedAES,
    );
    const aesKey = await window.crypto.subtle.importKey('raw', binaryAES, 'AES-GCM', true, [
      'encrypt',
      'decrypt',
    ]);
    const encryptedData = OAuthClient.#str2ab(OAuthClient.#base64URLDecode(b64URLEncodedData));
    const iv = OAuthClient.#str2ab(OAuthClient.#base64URLDecode(aesIvB64));
    const decryptedData = await window.crypto.subtle.decrypt(
      { name: 'AES-GCM', iv },
      aesKey,
      encryptedData,
    );
    return OAuthClient.#ab2str(decryptedData);
  }
};

module.exports = { OAuthClient };
