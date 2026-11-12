import fs from 'fs';
import path from 'path';
import * as cheerio from 'cheerio';

const inPath = process.argv[2] || 'release.html';
const outPath = process.argv[3] || 'release-notes.json';
const BASE =
  process.argv[4] || process.env.MIRO_BASE || 'https://gams.com/miro/';

// ---------- helpers ----------
function parseVersion(str) {
  const m = /(\d+)\.(\d+)/.exec(str);
  return m ? { major: Number(m[1]), minor: Number(m[2]) } : null;
}
function cmpVersion(a, b) {
  if (a.major !== b.major) return b.major - a.major;
  return b.minor - a.minor;
}
function normalizeHeadingText(s) {
  return s.replace(/\s+/g, ' ').trim().toLowerCase();
}
function trimHtml(s) {
  return s.replace(/\s+/g, ' ').trim();
}
function nextListAfter($, start) {
  let el = start.next();
  while (el && el.length) {
    if (el.is('ul, ol')) return el;
    if (el.is('h3, h2')) return null;
    el = el.next();
  }
  return null;
}

// Absolutize all <a href> under a given cloned element
function absolutizeAnchors($, root, base) {
  root.find('a[href]').each((_, a) => {
    const $a = $(a);
    const href = ($a.attr('href') || '').trim();
    if (!href) return;

    // If already absolute (scheme or protocol-relative), skip
    const isAbsolute =
      /^(?:[a-z]+:)?\/\//i.test(href) || /^[a-z]+:/i.test(href);
    if (isAbsolute) return;

    // Convert everything else (including "./", "../", and "#...") against base
    try {
      const abs = new URL(href, base).toString();
      $a.attr('href', abs);
      // Optional: force target=_blank so Electron treats as external (your main process can intercept anyway)
      $a.attr('target', '_blank');
      $a.attr('rel', 'noreferrer noopener');
    } catch {
      // ignore malformed hrefs
    }
  });
  return root;
}

/** Get outer HTML of an element with nested lists removed, preserving inline HTML and absolutizing anchors. */
function htmlWithoutNestedLists($, el, base) {
  const clone = $(el).clone();
  clone.find('ul, ol').remove();
  absolutizeAnchors($, clone, base);
  return trimHtml(clone.html() || '');
}

/**
 * Extract items from a list while preserving HTML and making <a> absolute.
 * - Plain <li> -> "…inline <em>HTML</em>…"
 * - <li>Label (may include <a>): <ul><li>sub1</li><li>sub2</li></ul>
 *     -> ["Label: sub1", "Label: sub2"]
 */
function extractReleaseItems($, rootList, base) {
  const items = [];
  rootList.children('li').each((_, li) => {
    const $li = $(li);
    const childLists = $li.children('ul, ol');

    if (childLists.length) {
      const labelHTML = htmlWithoutNestedLists($, $li, base).replace(
        /\s*:\s*$/,
        '',
      );
      childLists.each((__, subList) => {
        $(subList)
          .children('li')
          .each((___, subLi) => {
            const subHTML = htmlWithoutNestedLists($, subLi, base);
            if (subHTML)
              items.push(labelHTML ? `${labelHTML}: ${subHTML}` : subHTML);
          });
      });
    } else {
      const itemHTML = htmlWithoutNestedLists($, $li, base);
      if (itemHTML) items.push(itemHTML);
    }
  });
  return items;
}

// ---------- main ----------
// Abort if JSON already exists
if (fs.existsSync(outPath)) {
  console.error(`❌ Aborting: ${outPath} already exists.`);
  process.exit(1);
}

const html = fs.readFileSync(inPath, 'utf8');
const $ = cheerio.load(html);

// Collect versioned sections (e.g., "GAMS MIRO 2.13")
const sections = [];
$('section.doc-section').each((_, s) => {
  const h2 = $(s).find('h2.section-title').first();
  if (!h2.length) return;
  const ver = parseVersion(h2.text());
  if (ver) sections.push({ el: $(s), ver, title: h2.text() });
});
if (sections.length === 0) {
  console.error('No versioned sections found.');
  process.exit(1);
}

// Pick latest by numeric X.Y
sections.sort((a, b) => cmpVersion(a.ver, b.ver));
const latest = sections[0];

// Extract items under the two headings
let majorItems = [];
let minorItems = [];

latest.el.find('h3').each((_, h3) => {
  const heading = normalizeHeadingText($(h3).text());
  if (
    heading === 'new features' ||
    heading === 'minor new features and improvements'
  ) {
    const list = nextListAfter($, $(h3));
    if (!list) return;
    const items = extractReleaseItems($, list, BASE);
    if (heading === 'new features') majorItems = items;
    if (heading === 'minor new features and improvements') minorItems = items;
  }
});

const result = { major: majorItems, minor: minorItems };
fs.writeFileSync(outPath, JSON.stringify(result, null, 2), 'utf8');

console.log(`✅ Parsed latest release ${latest.title.trim()}`);
console.log(`   Major: ${majorItems.length}, Minor: ${minorItems.length}`);
console.log(`   Base for <a> links: ${BASE}`);
console.log(`   Wrote ${path.resolve(outPath)}`);
