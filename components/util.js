const isFalse = (val) => val === 'false' || val === false || val === 0;
const isTrue = (val) => val === 'true' || val === true || val === 1;
// on Wayland, attaching a dialog to a window that was never shown kills the app
const dialogParent = (win) =>
  win && !win.isDestroyed() && win.isVisible() ? win : undefined;

export { isFalse, isTrue, dialogParent };
