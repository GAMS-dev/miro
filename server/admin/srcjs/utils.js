export function unicodeToHTMLID(str) {
  const idTmp = window
    .btoa(encodeURIComponent(str))
    .replace(/\+/g, '-')
    .replace(/\//g, '_')
    .replace(/=/g, '');
  return `a${idTmp}`;
}

export function HTMLIDToUnicode(str) {
  if (str == null) {
    return null;
  }
  if (str === '') {
    return '';
  }
  const unicodeTmp = str.substring(1);
  return decodeURIComponent(window.atob(unicodeTmp));
}

// taken from bjornd: https://stackoverflow.com/questions/6234773/can-i-escape-html-special-chars-in-javascript
export function escapeHtml(unsafe) {
  return unsafe
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&#039;');
}

// eslint-disable-next-line no-eval
export const indirectEval = eval;
