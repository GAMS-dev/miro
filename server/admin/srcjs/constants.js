export const PLACEHOLDERS = {
  appFiles: 'Drop your MIRO app here or click to browse.',
  appName: 'App title',
  appDesc: 'Short model description (optional)',
  appGroups: 'Access groups (optional)',
  appLogo: 'Different app logo? Drop your MIRO app logo here.',
};

export const SUPPORTED_FILE_TYPES = ['gdx', 'miroscen', 'xlsx', 'xlsm', 'xls', 'zip'];

export const YES_NO_BUTTONS = { confirm: { label: 'Yes' }, cancel: { label: 'No' } };

export const ADD_APP_WRAPPER_HTML = `<div id="addAppBox" class="add-app-box app-box-fixed-height">
  <div class="app-logo-wrapper"><p class="add-app-box-logo"></p></div>
  <div>
    <div class="add-app-box-title"></div>
    <div class="add-app-box-desc"></div>
  </div>
  <a class="btn-add-app" id="addApp"><i class="fas fa-circle-plus"></i></a>
</div>`;
