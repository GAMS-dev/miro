// Counting dragenter/dragleave is unreliable (nested targets, Esc), so the target is
// derived from the element under the pointer on every dragover instead.
const RESET_TIMEOUT_MS = 1000;

function registerDropZoneHighlight(targets, isDisabled = () => false) {
  let current = null;
  let resetTimer = null;

  const setCurrent = (next) => {
    if (current?.element === next?.element) {
      return;
    }
    if (current) {
      current.target.setHighlight(current.element, false);
    }
    if (next) {
      next.target.setHighlight(next.element, true);
    }
    current = next;
  };
  const clearHighlight = () => {
    clearTimeout(resetTimer);
    setCurrent(null);
  };
  const updateHighlight = (e) => {
    if (isDisabled()) {
      clearHighlight();
      return;
    }
    const el = e.target instanceof Element ? e.target : null;
    let next = null;
    if (el) {
      const target = targets.find((t) => el.closest(t.selector));
      if (target) {
        next = { target, element: el.closest(target.selector) };
      }
    }
    setCurrent(next);
    clearTimeout(resetTimer);
    resetTimer = setTimeout(clearHighlight, RESET_TIMEOUT_MS);
  };

  // capture phase: the targets' own handlers stop propagation
  document.addEventListener('dragenter', updateHighlight, true);
  document.addEventListener('dragover', updateHighlight, true);
  document.addEventListener(
    'dragleave',
    (e) => {
      if (
        e.relatedTarget == null &&
        (e.clientX <= 0 ||
          e.clientY <= 0 ||
          e.clientX >= window.innerWidth ||
          e.clientY >= window.innerHeight)
      ) {
        clearHighlight();
      }
    },
    true,
  );
  document.addEventListener('drop', clearHighlight, true);
  document.addEventListener('dragend', clearHighlight, true);
}

module.exports = { registerDropZoneHighlight };
