const { app } = require('electron');
const path = require('node:path');

const getAppDbPath = (appDbPath) => ((appDbPath === '' || appDbPath == null) ? path.join(app.getPath('home'), '.miro', 'app_data') : appDbPath);
const isFalse = (val) => val === 'false'
  || val === false || val === 0;

module.exports = {
  getAppDbPath,
  isFalse,
};
