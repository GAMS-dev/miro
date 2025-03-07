import Store from 'electron-store';

const schema = {
  apps: {
    type: 'array',
    items: {
      type: 'object',
      properties: {
        id: {
          type: 'string',
          minLength: 1,
        },
        title: {
          type: 'string',
          minLength: 1,
        },
        description: {
          type: 'string',
        },
        logoPath: {
          type: 'string',
          minLength: 1,
        },
        dbpath: {
          type: 'string',
          minLength: 1,
        },
        gmsName: {
          type: 'string',
          minLength: 1,
        },
        version: {
          type: 'string',
          minLength: 1,
        },
        authors: {
          type: 'array',
          items: {
            type: 'string',
            minLength: 1,
          }
        },
        apiversion: {
          type: 'integer',
          minimum: 1,
        },
        usetmpdir: {
          type: 'boolean',
        },
        miroversion: {
          type: 'string',
          pattern: '^[0-9]+\\.[0-9]+\\.[0-9]+$',
        },
        modesAvailable: {
          type: 'array',
          uniqueItems: true,
          minItems: 1,
          items: {
            type: 'string',
            enum: [
              'base',
              'hcube',
            ],
          },
        },
      },
      required: ['id', 'title', 'modesAvailable', 'usetmpdir', 'apiversion', 'miroversion'],
    },
  },
};

class AppDataStore extends Store {
  constructor(configPath) {
    super({
      schema,
      cwd: configPath,
      encryptionKey: 'MIROobfuscatedConfigFile',
    });
    this.apps = this.get('apps') || [];
  }

  saveApps() {
    this.set('apps', this.apps);
    return this;
  }

  updateApps(apps) {
    this.apps = apps;
    return this.saveApps();
  }

  updateApp(app) {
    const appIdx = this.apps.findIndex((t) => t.id === app.id);
    if (appIdx === -1) {
      return this;
    }
    this.apps[appIdx] = app;
    return this.saveApps();
  }

  getAppConfig(id) {
    const appIdx = this.apps.findIndex((t) => t.id === id);
    if (appIdx === -1) {
      throw new Error('App ID not found');
    }
    return this.apps[appIdx];
  }

  getApps() {
    this.apps = this.get('apps') || [];
    return this.apps;
  }

  getAppConfigValue(id, key) {
    const appIdx = this.apps.findIndex((t) => t.id === id);
    if (appIdx === -1) {
      throw new Error('App ID not found');
    }
    return this.apps[appIdx][key];
  }

  isUniqueId(id) {
    if (this.apps.filter((t) => t.id.toLowerCase() === id.toLowerCase()).length) {
      return false;
    }
    return true;
  }

  addApp(app) {
    if (!this.isUniqueId(app.id)) {
      throw new Error('DuplicatedId');
    }
    this.apps = [...this.apps, app];

    return this.saveApps();
  }

  deleteApp(id) {
    this.apps = this.apps.filter((t) => t.id !== id);
    return this.saveApps();
  }
}

export default AppDataStore;
