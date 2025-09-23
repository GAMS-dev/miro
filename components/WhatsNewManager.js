import fs from 'fs/promises';
import path from 'path';

export default class WhatsNewManager {
  constructor(configManager, whatsNewDir, miroVersion) {
    this.configManager = configManager;
    this.whatsNewDir = whatsNewDir;
    this.miroVersion = miroVersion;
    this.content = null;
  }

  async initialize({ force = false } = {}) {
    if (this.content != null) {
      return;
    }
    if (force !== true && this.configManager.isNewMiroInstallation) {
      // don't show what's new if installation is new
      return;
    }
    const previousMiroVersion = WhatsNewManager.parseSemver(
      await this.configManager.get('previousMIROVersion'),
    );
    let entries = [];
    try {
      entries = await fs.readdir(this.whatsNewDir, { withFileTypes: true });
    } catch (e) {
      return;
    }

    let candidates = entries
      .filter((d) => d.isFile())
      .map((d) => d.name)
      .map((name) => ({
        name,
        ver: WhatsNewManager.parseWhatsNewFilename(name),
      }))
      .filter((x) => x.ver)
      .sort((a, b) => {
        if (a.ver.major !== b.ver.major) {
          return b.ver.major - a.ver.major;
        }
        return b.ver.minor - a.ver.minor;
      });

    if (force === true) {
      // get latest what's new entry
      candidates = candidates.slice(0, 1);
    } else if (previousMiroVersion != null) {
      candidates = candidates.filter(
        (x) => WhatsNewManager.cmpMajorMinor(x.ver, previousMiroVersion) > 0,
      );
    }
    if (candidates.length === 0) {
      return;
    }
    const majorAll = [];
    const minorAll = [];

    // eslint-disable-next-line no-restricted-syntax
    for (const file of candidates) {
      const full = path.join(this.whatsNewDir, file.name);
      const json = JSON.parse(await fs.readFile(full, 'utf8')); // eslint-disable-line no-await-in-loop
      if (Array.isArray(json.major)) majorAll.push(...json.major);
      if (Array.isArray(json.minor)) minorAll.push(...json.minor);
    }

    this.content = WhatsNewManager.createHTML({
      title: "What's new",
      major: majorAll,
      minor: minorAll,
    });
  }

  async getContent({ force = false } = {}) {
    await this.initialize({ force });
    this.configManager.set({ previousMIROVersion: this.miroVersion });
    return this.content;
  }

  static createHTML({ title, major, minor }) {
    const escapeHTML = (s) =>
      String(s)
        .replaceAll('&', '&amp;')
        .replaceAll('<', '&lt;')
        .replaceAll('>', '&gt;')
        .replaceAll('"', '&quot;')
        .replaceAll("'", '&#39;');
    const section = (heading, items) => {
      if (!items?.length) return '';
      const lis = items.map((x) => `<li class="mb-2">${x}</li>`).join('');
      return `
      <div class="card mb-4" style="overflow:auto;">
        <div class="card-body">
          <h5 class="card-title">${escapeHTML(heading)}</h5>
          <ul class="mb-0">
            ${lis}
          </ul>
        </div>
      </div>`;
    };

    return `<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8" />
  <meta http-equiv="Content-Security-Policy" content="script-src 'self';" />
  <meta name="color-scheme" content="dark light" />
  <meta name="viewport" content="width=device-width, initial-scale=1.0" />
  <link
    rel="stylesheet"
    href="./node_modules/bootstrap/dist/css/bootstrap.min.css"
  />
  <link id="theme-style" rel="stylesheet" href="./css/styles.css" />
</head>
<body>
  <div class="container my-4">
    <h1 class="h3">${escapeHTML(title)}</h1>

    ${
      major.length || minor.length
        ? `<div>
           ${section('New features', major)}
           ${section('Minor new features and improvements', minor)}
         </div>`
        : `<div class="alert alert-secondary">No new items.</div>`
    }
  </div>
</body>

</html>`;
  }

  static parseSemver(v) {
    const [maj = '0', min = '0', pat = '0'] = String(v).split('.');
    return { major: +maj || 0, minor: +min || 0, patch: +pat || 0 };
  }

  static cmpMajorMinor(a, b) {
    if (a.major !== b.major) return a.major - b.major;
    return a.minor - b.minor;
  }

  static parseWhatsNewFilename(fileName) {
    const m = /^(\d+)\.(\d+)\.json$/i.exec(fileName.trim());
    return m ? { major: +m[1], minor: +m[2] } : null;
  }
}
