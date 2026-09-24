// Counting dragenter/dragleave is unreliable (e.g. Esc sends no dragleave), so the zone is
// derived from the element under the pointer on every dragover instead.
const DRAGOVER_CLASS = 'drag-drop-area-dragover';
const RESET_TIMEOUT_MS = 1000;

export default function registerDropZoneHighlight(isDisabled = () => false) {
  let highlightedZone = null;
  let resetTimer = null;

  const setHighlightedZone = (zone) => {
    if (zone === highlightedZone) {
      return;
    }
    if (highlightedZone) {
      highlightedZone.classList.remove(DRAGOVER_CLASS);
    }
    if (zone) {
      zone.classList.add(DRAGOVER_CLASS);
    }
    highlightedZone = zone;
  };
  const clearHighlight = () => {
    clearTimeout(resetTimer);
    setHighlightedZone(null);
  };
  const updateHighlight = (e) => {
    if (isDisabled()) {
      return;
    }
    const target = e.target instanceof Element ? e.target : null;
    setHighlightedZone(target?.closest('.drag-drop-area') ?? null);
    clearTimeout(resetTimer);
    resetTimer = setTimeout(clearHighlight, RESET_TIMEOUT_MS);
  };

  // capture phase: the zones' own handlers stop propagation
  document.addEventListener('dragenter', updateHighlight, true);
  document.addEventListener('dragover', updateHighlight, true);
  document.addEventListener('dragleave', (e) => {
    // Safari sets relatedTarget to null for every dragleave
    if (e.relatedTarget == null && (e.clientX <= 0 || e.clientY <= 0
      || e.clientX >= window.innerWidth || e.clientY >= window.innerHeight)) {
      clearHighlight();
    }
  }, true);
  document.addEventListener('drop', clearHighlight, true);
  document.addEventListener('dragend', clearHighlight, true);
}
