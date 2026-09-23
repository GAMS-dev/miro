/* global $:false, HTMLWidgets:false */

// Handsontable inputs get a fixed pixel height from the server, so they either overflow
// the screen or leave space unused. Handsontable renders rows virtually and needs its
// height in pixels (viewport units don't work), so we measure the space that is left
// below each table and apply it: every table spans the available height of its tab,
// but never more than its content.
// (The MIRO pivot table and chart are sized in CSS instead, see modern.less.)

const MIN_HEIGHT = 250;
// extra room below the rows, so a table that exactly fits never becomes scrollable
const SLACK = 2;
// space kept free below a table (bottom padding of the section it lives in)
const BOTTOM_GAP = 20;
// below this width the layout stacks and the page scrolls as a whole
const MIN_VIEWPORT_WIDTH = 768;

// Thickness of a (native) scrollbar in Handsontable's scroll area: 0 with overlay
// scrollbars (macOS trackpads), ~15px with classic ones. Measured once.
let hotScrollbarSize = null;
function getHotScrollbarSize() {
  if (hotScrollbarSize === null) {
    const outer = document.createElement('div');
    outer.className = 'handsontable';
    outer.style.cssText = 'position:absolute;top:-9999px;left:-9999px;visibility:hidden';
    const holder = document.createElement('div');
    holder.className = 'wtHolder';
    holder.style.cssText = 'width:100px;height:100px;overflow:scroll';
    outer.appendChild(holder);
    document.body.appendChild(outer);
    hotScrollbarSize = holder.offsetHeight - holder.clientHeight;
    outer.remove();
  }
  return hotScrollbarSize;
}

function isVisible(el) {
  return el.offsetParent !== null;
}

// height from the table's top edge to the bottom of the viewport, minus whatever
// follows the table inside its tab pane
function availableHeight(el) {
  const rect = el.getBoundingClientRect();
  const pane = el.closest('.tab-pane');
  const below = pane ? Math.max(0, pane.getBoundingClientRect().bottom - rect.bottom) : 0;
  const top = rect.top + window.scrollY;
  return Math.max(MIN_HEIGHT, Math.floor(window.innerHeight - top - below - BOTTOM_GAP));
}

// Handsontable measures a wrapped column header only once and caches its height, so after
// the window is resized the header rows of the frozen row-header clones no longer match
// the table and every row appears shifted. Forget the cached heights before rendering.
function resyncHot(hot) {
  const viewport = hot.view && hot.view.wt && hot.view.wt.wtViewport;
  if (viewport) {
    viewport.hasOversizedColumnHeadersMarked = {};
    viewport.oversizedColumnHeaders = [];
  }
  hot.render();
}

function fitHot(el, resync) {
  const instance = HTMLWidgets.getInstance(el);
  const hot = instance && instance.hot;
  if (!hot) {
    return;
  }
  if (resync) {
    resyncHot(hot);
  }
  const holder = el.querySelector('.ht_master .wtHolder');
  const content = el.querySelector('.ht_master .wtHider');
  if (!holder || !content) {
    return;
  }
  // Always leave room for a horizontal scrollbar, even if none is shown right now: when one
  // appears later (wider columns, or while a cell gets selected in Safari), the rows must
  // still fit. Otherwise a vertical scrollbar follows, which narrows the table, which keeps
  // the horizontal one, and both scrollbars stay for good.
  const natural = content.offsetHeight + getHotScrollbarSize() + SLACK;
  const target = Math.min(availableHeight(el), natural);
  if (Math.abs(el.offsetHeight - target) > 1) {
    el.style.height = `${target}px`;
    hot.updateSettings({ height: target });
  }
}

function fitAll(resync = false) {
  if (window.innerWidth < MIN_VIEWPORT_WIDTH) {
    return;
  }
  document.querySelectorAll('.rhandsontable').forEach((el) => {
    if (isVisible(el)) {
      fitHot(el, resync);
    }
  });
}

function debounced(fn, ms) {
  let timeoutId;
  return (...args) => {
    clearTimeout(timeoutId);
    timeoutId = setTimeout(() => fn(...args), ms);
  };
}

export default function registerHeightFitting() {
  const scheduleFit = debounced(fitAll, 120);
  const scheduleResync = debounced(() => fitAll(true), 150);
  window.addEventListener('resize', scheduleResync);
  // new tab shown, sidebar toggled or content (re)rendered by the server
  $(document).on('shown.bs.tab expanded.pushMenu collapsed.pushMenu', () => scheduleResync());
  $(document).on('shiny:idle', () => scheduleFit());
}
