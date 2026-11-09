import fs from 'fs/promises';
import path from 'path';

const RELEASE_NOTES_URL = 'https://gams.com/miro/release.html';

// same as renderMiraIcon() in R
const MIRA_ICON = `<svg class="wn-mira-icon" viewBox="0 0 24 24" aria-hidden="true" focusable="false">
<path fill-rule="evenodd" d="M10 3.2Q11.9 10.1 18.8 12Q11.9 13.9 10 20.8Q8.1 13.9 1.2 12Q8.1 10.1 10 3.2ZM10 10.3A1.7 1.7 0 1 0 10 13.7A1.7 1.7 0 1 0 10 10.3Z"/>
<path d="M18.5 1.8Q19 5 22.2 5.5Q19 6 18.5 9.2Q18 6 14.8 5.5Q18 5 18.5 1.8Z"/>
<circle cx="19.4" cy="18.8" r="1.35"/></svg>`;

const escapeHTML = (s) =>
  String(s)
    .replaceAll('&', '&amp;')
    .replaceAll('<', '&lt;')
    .replaceAll('>', '&gt;')
    .replaceAll('"', '&quot;')
    .replaceAll("'", '&#39;');

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
    const files = await Promise.all(
      candidates.map(async ({ name }) =>
        JSON.parse(await fs.readFile(path.join(this.whatsNewDir, name), 'utf8')),
      ),
    );
    const { major, minor } = candidates[0].ver;
    this.content = WhatsNewManager.createHTML({
      version: `${major}.${minor}`,
      highlight: files[0].highlight,
      major: files.flatMap((f) => f.major ?? []),
      minor: files.flatMap((f) => f.minor ?? []),
    });
  }

  async getContent({ force = false } = {}) {
    await this.initialize({ force });
    this.configManager.set({ previousMIROVersion: this.miroVersion });
    return this.content;
  }

  static renderHighlight(h) {
    if (h == null) return '';
    // preload="none": nothing is fetched until the user starts the video
    const features = h.features
      .map(
        (f) => `<div class="wn-feature">
          <div class="wn-feature-icon"><i class="fa-solid fa-${escapeHTML(f.icon)}" aria-hidden="true"></i></div>
          <div><div class="wn-feature-title">${f.title}</div><div class="wn-feature-text">${f.text}</div></div>
        </div>`,
      )
      .join('');
    return `<section class="wn-hero">
      <div class="wn-eyebrow">${MIRA_ICON}<span>${escapeHTML(h.eyebrow)}</span></div>
      <h2 class="wn-hero-title">${h.title}</h2>
      <p class="wn-lead">${h.description}</p>
      <div class="wn-media">
        <video controls preload="none" playsinline src="${escapeHTML(h.video)}" poster="${escapeHTML(h.poster)}"
          aria-label="${escapeHTML(h.mediaAlt)}"></video>
      </div>
      <div class="wn-features">${features}</div>
      <div class="wn-hero-footer">
        <p class="wn-note">${h.note}</p>
        <a class="wn-cta" href="${escapeHTML(h.link.href)}" target="_blank" rel="noreferrer noopener">${escapeHTML(h.link.text)} &rarr;</a>
      </div>
    </section>`;
  }

  static createHTML({ version, highlight, major, minor }) {
    const section = (heading, items, modifier) => {
      if (!items?.length) return '';
      const lis = items.map((x) => `<li>${x}</li>`).join('');
      return `
      <section class="wn-section">
        <h3 class="wn-section-title">${escapeHTML(heading)}</h3>
        <ul class="wn-list wn-list-${modifier}">${lis}</ul>
      </section>`;
    };

    return `<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8" />
  <meta http-equiv="Content-Security-Policy" content="script-src 'self';" />
  <meta name="color-scheme" content="dark light" />
  <meta name="viewport" content="width=device-width, initial-scale=1.0" />
  <link rel="stylesheet" href="./node_modules/@fortawesome/fontawesome-free/css/all.min.css" />
  <style>${WhatsNewManager.CSS}</style>
</head>
<body>
  <main class="wn-container">
    <header class="wn-header">
      <img class="wn-logo" src="./static/icon_64x64.png" alt="" />
      <div>
        <div class="wn-product">GAMS MIRO ${escapeHTML(version)}</div>
        <h1 class="wn-title">What's New</h1>
      </div>
    </header>
    ${WhatsNewManager.renderHighlight(highlight)}
    ${section('New features', major, 'major')}
    ${section('Minor new features and improvements', minor, 'minor')}
    <footer class="wn-footer">
      <a href="${RELEASE_NOTES_URL}" target="_blank" rel="noreferrer noopener">Read the full release notes &rarr;</a>
    </footer>
  </main>
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

WhatsNewManager.CSS = `
:root {
  --wn-bg: #f5f6f8;
  --wn-surface: #ffffff;
  --wn-text: #1d2433;
  --wn-muted: #5d6576;
  --wn-border: rgba(16, 24, 40, 0.1);
  --wn-accent: #f39619;
  --wn-accent-strong: #c86f00;
  --wn-accent-soft: rgba(243, 150, 25, 0.12);
  --wn-link: #1f5fa8;
  --wn-shadow: 0 1px 2px rgba(16, 24, 40, 0.06), 0 12px 32px -12px rgba(16, 24, 40, 0.18);
}
@media (prefers-color-scheme: dark) {
  :root {
    --wn-bg: #191b1f;
    --wn-surface: #23262c;
    --wn-text: #eceef2;
    --wn-muted: #a3a9b3;
    --wn-border: rgba(255, 255, 255, 0.09);
    --wn-accent-strong: #ffb24d;
    --wn-accent-soft: rgba(243, 150, 25, 0.16);
    --wn-link: #7cb3f2;
    --wn-shadow: 0 1px 2px rgba(0, 0, 0, 0.4), 0 12px 32px -12px rgba(0, 0, 0, 0.6);
  }
}
* { box-sizing: border-box; }
html, body {
  margin: 0;
  background: var(--wn-bg);
  color: var(--wn-text);
  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, "Noto Sans", sans-serif;
  font-size: 15px;
  line-height: 1.55;
  -webkit-font-smoothing: antialiased;
}
a { color: var(--wn-link); text-decoration: none; }
a:hover { text-decoration: underline; }
.code { font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; font-size: 0.9em; padding: 1px 5px; border-radius: 4px; background: var(--wn-accent-soft); }
.wn-container { max-width: 860px; margin: 0 auto; padding: 28px 24px 32px; }
.wn-header { display: flex; align-items: center; gap: 14px; margin-bottom: 22px; }
.wn-logo { width: 44px; height: 44px; }
.wn-product { font-size: 13px; font-weight: 600; letter-spacing: 0.08em; text-transform: uppercase; color: var(--wn-muted); }
.wn-title { margin: 0; font-size: 28px; font-weight: 700; letter-spacing: -0.01em; line-height: 1.2; }

.wn-hero {
  position: relative;
  padding: 26px 26px 22px;
  margin-bottom: 28px;
  border-radius: 16px;
  border: 1px solid var(--wn-border);
  background:
    radial-gradient(120% 70% at 100% 0%, var(--wn-accent-soft), transparent 60%),
    var(--wn-surface);
  box-shadow: var(--wn-shadow);
  overflow: hidden;
}
.wn-hero::before {
  content: "";
  position: absolute;
  inset: 0 0 auto 0;
  height: 4px;
  background: linear-gradient(90deg, var(--wn-accent), #ffc56b 55%, var(--wn-accent));
}
.wn-eyebrow {
  display: inline-flex; align-items: center; gap: 7px;
  padding: 4px 12px 4px 10px;
  border-radius: 999px;
  background: var(--wn-accent-soft);
  color: var(--wn-accent-strong);
  font-size: 12.5px; font-weight: 700; letter-spacing: 0.04em; text-transform: uppercase;
}
.wn-mira-icon { width: 1.15em; height: 1.15em; fill: currentColor; flex: 0 0 auto; }
.wn-hero-title { margin: 14px 0 8px; font-size: 30px; font-weight: 800; letter-spacing: -0.02em; line-height: 1.15; }
.whats-new-accent {
  background: linear-gradient(90deg, var(--wn-accent), #ffb347);
  -webkit-background-clip: text; background-clip: text;
  color: transparent;
}
.wn-lead { margin: 0 0 18px; font-size: 16px; color: var(--wn-muted); max-width: 64ch; }
.wn-lead b { color: var(--wn-text); }
.wn-media {
  border-radius: 12px;
  overflow: hidden;
  border: 1px solid var(--wn-border);
  background: #000;
  aspect-ratio: 16 / 9;
  margin-bottom: 20px;
}
.wn-media video { display: block; width: 100%; height: 100%; object-fit: contain; }
.wn-features { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 12px; }
@media (max-width: 680px) { .wn-features { grid-template-columns: 1fr; } }
.wn-feature {
  display: flex; gap: 12px; align-items: flex-start;
  padding: 14px;
  border-radius: 12px;
  border: 1px solid var(--wn-border);
  background: var(--wn-surface);
}
.wn-feature-icon {
  flex: 0 0 34px; height: 34px;
  display: flex; align-items: center; justify-content: center;
  border-radius: 9px;
  background: var(--wn-accent-soft);
  color: var(--wn-accent-strong);
  font-size: 15px;
}
.wn-feature-title { font-weight: 700; margin-bottom: 2px; }
.wn-feature-text { font-size: 14px; color: var(--wn-muted); }
.wn-hero-footer { display: flex; flex-wrap: wrap; align-items: center; justify-content: space-between; gap: 12px 20px; margin-top: 18px; }
.wn-note { margin: 0; flex: 1 1 320px; font-size: 13.5px; color: var(--wn-muted); }
.wn-cta {
  display: inline-flex; align-items: center; gap: 4px;
  padding: 9px 16px;
  border-radius: 999px;
  background: var(--wn-accent);
  color: #1d2433;
  font-weight: 700; font-size: 14px;
  white-space: nowrap;
}
.wn-cta:hover { text-decoration: none; filter: brightness(1.08); }

.wn-section { margin-bottom: 24px; }
.wn-section-title {
  margin: 0 0 12px;
  font-size: 13px; font-weight: 700; letter-spacing: 0.08em; text-transform: uppercase;
  color: var(--wn-muted);
}
.wn-list { list-style: none; margin: 0; padding: 0; }
.wn-list-major { display: grid; gap: 10px; }
.wn-list-major li {
  padding: 14px 16px 14px 18px;
  border-radius: 12px;
  border: 1px solid var(--wn-border);
  border-left: 3px solid var(--wn-accent);
  background: var(--wn-surface);
}
.wn-list-minor {
  border-radius: 12px;
  border: 1px solid var(--wn-border);
  background: var(--wn-surface);
  overflow: hidden;
}
.wn-list-minor li { position: relative; padding: 11px 16px 11px 34px; font-size: 14px; }
.wn-list-minor li + li { border-top: 1px solid var(--wn-border); }
.wn-list-minor li::before {
  content: "";
  position: absolute; left: 16px; top: 19px;
  width: 6px; height: 6px; border-radius: 50%;
  background: var(--wn-accent);
}
.wn-list img { display: block; max-width: 100%; height: auto; margin-top: 12px; border-radius: 8px; border: 1px solid var(--wn-border); }
.wn-footer { padding-top: 8px; text-align: center; font-size: 14px; font-weight: 600; }
`;
