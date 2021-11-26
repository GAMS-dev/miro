const { app, Menu, shell } = require('electron');

const isMac = process.platform === 'darwin';

module.exports = (
  addExampleAppsCallback,
  activateEditCallback,
  addMiroscenCallback,
  showSettingsCallback,
  openCheckUpdateWindow,
  openAboutDialog,
) => {
  const lang = global.lang.menu;
  return Menu.buildFromTemplate([
    // { role: 'appMenu' }
    ...(isMac ? [{
      label: app.getName(),
      submenu: [
        {
          label: lang.about,
          click: async () => {
            await openAboutDialog();
          },
        },
        { type: 'separator' },
        {
          label: lang.pref,
          accelerator: 'Cmd+,',
          click: async () => {
            await showSettingsCallback();
          },
        },
        { label: lang.services, role: 'services' },
        { type: 'separator' },
        { label: lang.hide, role: 'hide' },
        { label: lang.hideothers, role: 'hideothers' },
        { label: lang.unhide, role: 'unhide' },
        { type: 'separator' },
        { label: lang.quit, role: 'quit' },
      ],
    }] : []),
    // { role: 'fileMenu' }
    {
      label: lang.file,
      submenu: [
        isMac ? { label: lang.close, role: 'close' } : { label: lang.quit, role: 'quit' },
        ...(isMac ? [] : [{
          label: lang.pref,
          accelerator: 'F7',
          click: async () => {
            await showSettingsCallback();
          },
        }]),
      ],
    },
    {
      label: lang.edit,
      submenu: [
        {
          label: lang.addApp,
          accelerator: 'CmdOrCtrl+O',
          click: async () => {
            await activateEditCallback(true, true);
          },
        },
        {
          label: lang.editApp,
          accelerator: 'CmdOrCtrl+E',
          click: async () => {
            await activateEditCallback();
          },
        },
        {
          label: lang.addMiroScen,
          click: async () => {
            await addMiroscenCallback();
          },
        },
        {
          label: lang.addExampleApps,
          click: async () => {
            await addExampleAppsCallback();
          },
        },
        { label: lang.undo, accelerator: 'CmdOrCtrl+Z', selector: 'undo:' },
        { label: lang.redo, accelerator: 'Shift+CmdOrCtrl+Z', selector: 'redo:' },
        { type: 'separator' },
        { label: lang.cut, accelerator: 'CmdOrCtrl+X', selector: 'cut:' },
        { label: lang.copy, accelerator: 'CmdOrCtrl+C', selector: 'copy:' },
        { label: lang.paste, accelerator: 'CmdOrCtrl+V', selector: 'paste:' },
        { label: lang.selectAll, accelerator: 'CmdOrCtrl+A', selector: 'selectAll:' },
      ],
    },
    // { role: 'viewMenu' }
    {
      label: lang.view,
      submenu: [
        { label: lang.fullscreen, role: 'togglefullscreen', accelerator: isMac ? 'Ctrl+Cmd+F' : 'F11' },
        {
          label: lang.resetZoom, role: 'resetZoom', enabled: false, visible: false,
        },
        {
          label: lang.zoomIn, role: 'zoomIn', enabled: false, visible: false,
        },
        {
          label: lang.zoomOut, role: 'zoomOut', enabled: false, visible: false,
        },
      ],
    },
    // { role: 'windowMenu' }
    {
      label: lang.window,
      submenu: [
        { label: lang.minimize, role: 'minimize' },
        ...(isMac ? [
          { label: lang.zoom, role: 'zoom' },
          { type: 'separator' },
          { label: lang.front, role: 'front' },
        ] : [
          { label: lang.close, role: 'close' },
        ]),
      ],
    },
    {
      role: 'help',
      label: lang.help,
      submenu: [
        {
          label: lang.doc,
          click: async () => {
            await shell.openExternal('https://gams.com/miro');
          },
        },
        ...(isMac ? [
          { type: 'separator' },
        ] : [
          { type: 'separator' },
          {
            label: lang.about,
            click: async () => {
              await openAboutDialog();
            },
          },
          { type: 'separator' },
        ]),
        {
          label: lang.update,
          click: () => {
            openCheckUpdateWindow();
          },
        },
      ],
    },
  ]);
};
