import bootbox from 'bootbox';
import { state, dom } from './state';
import {
  expandAddAppForm, refreshConfigList, openAdvancedSettingsDialog, showOverwriteDialog,
  pinTileHeights,
} from './ui';
import {
  sendAddRequest, sendUpdateRequest, sendRemoveRequest, sendUpdateAppOrderRequest,
  sendUpdateAppShinyEvent,
} from './api';
import { escapeHtml, HTMLIDToUnicode, unicodeToHTMLID } from './utils';
import { PLACEHOLDERS, SUPPORTED_FILE_TYPES } from './constants';
import registerDropZoneHighlight from './drop_zones';

export default function setupEventListeners() {
  $(document).on('keyup', (event) => {
    if (event.key === 'Enter' && !event.ctrlKey) {
      if (
        $('#shiny-modal').find('.selectize-input.input-active').length > 0
        || $('#shiny-modal').find('*[data-dismiss="modal"]').is(':focus')
      ) {
        return;
      }
      $('.bt-gms-confirm:visible:enabled').trigger('click');
    } // ENTER will confirm modal dialogues
  });

  $(document).on('dragover', '.drag-drop-area', (e) => {
    e.preventDefault();
    e.stopPropagation();
  });

  $(document).on('dragenter dragleave', '.drag-drop-area', (e) => {
    if (state.reorderAppsMode) {
      return;
    }
    e.preventDefault();
    e.stopPropagation();
  });
  registerDropZoneHighlight(() => state.reorderAppsMode);

  $(document).on('shiny:inputchanged', (event) => {
    const eventName = event.name;
    if (
      eventName.startsWith('appFiles_')
      && event.value != null
      && event.value.length > 0
    ) {
      const appId = HTMLIDToUnicode(eventName.substring(9));
      const fileTypes = event.value.map((fileTmp) => fileTmp.name.split('.').pop().toLowerCase());
      state.uploadAppDataFinished[appId] = true;
      setTimeout(() => sendUpdateAppShinyEvent(appId, fileTypes), 800);
    }
  });

  $(document).on('click', '#btLogin', () => {
    $('#loginFormWrapper').hide();
    $('#loginError').hide();
    $('#loginLoadingIndicator').show();
    Shiny.setInputValue(
      'loginRequest',
      {
        user: document.getElementById('loginUser').value,
        password: document.getElementById('loginPassword').value,
      },
      {
        priority: 'event',
      },
    );
  });

  const $appsWrapper = $('#appsWrapper');

  $appsWrapper.on('click', '#addAppBox', expandAddAppForm);
  $appsWrapper.on('click', '.cancel-btn', refreshConfigList);
  $appsWrapper.on('click', '#btAddApp', sendAddRequest);
  $appsWrapper.on('click', '.open-config-btn', openAdvancedSettingsDialog);

  $appsWrapper.on('drop', '.app-logo', (e) => {
    if (state.reorderAppsMode) {
      return;
    }
    e.preventDefault();
    e.stopPropagation();
    state.dragAddAppCounter = 0;
    $('.btn-save-changes').attr('disabled', true);
    $('#btAddApp').attr('disabled', true);
  });

  $appsWrapper.on('dragenter', '#addAppBox', (e) => {
    e.preventDefault();
    e.stopPropagation();
    state.openAddAppCounter += 1;
    if (state.dragTimerAddAppBox == null) {
      state.dragTimerAddAppBox = setTimeout(() => {
        state.openAddAppCounter = 0;
        state.dragTimerAddAppBox = null;
        expandAddAppForm();
      }, 700);
    }
  });

  $appsWrapper.on('dragleave', '#addAppBox', (e) => {
    e.preventDefault();
    e.stopPropagation();
    state.openAddAppCounter -= 1;
    if (state.openAddAppCounter === 0) {
      clearTimeout(state.dragTimerAddAppBox);
      state.dragTimerAddAppBox = null;
    }
  });

  $appsWrapper.on('dragover', '#addAppBox', (e) => {
    e.preventDefault();
    e.stopPropagation();
  });

  $appsWrapper.on('change', '.input-app-logo', () => {
    $('.btn-save-changes').attr('disabled', true);
    $('#btAddApp').attr('disabled', true);
  });

  $appsWrapper.on('dragstart', '.app-box-draggable', (e) => {
    state.reorderAppsMode = true;
    e.originalEvent.dataTransfer.setData(
      'text/plain',
      e.originalEvent.target.id,
    );
    e.originalEvent.target.style.opacity = 0.5;
  });

  $appsWrapper.on('dragenter', '.app-box-draggable', function (e) {
    if (!state.reorderAppsMode) {
      return;
    }
    e.preventDefault();
    e.stopPropagation();
    state.dragAddAppCounter += 1;
    $(this).addClass('drag-drop-area-dragover');
  });

  $appsWrapper.on('dragover', '.app-box-draggable', (e) => {
    if (!state.reorderAppsMode) {
      return;
    }
    e.preventDefault();
    e.stopPropagation();
  });

  $appsWrapper.on('dragleave', '.app-box-draggable', function (e) {
    if (!state.reorderAppsMode) {
      return;
    }
    e.preventDefault();
    e.stopPropagation();
    state.dragAddAppCounter -= 1;
    if (state.dragAddAppCounter === 0) {
      $(this).removeClass('drag-drop-area-dragover');
    }
  });

  $appsWrapper.on('dragend', '.app-box-draggable', (e) => {
    if (!state.reorderAppsMode) {
      return;
    }
    e.preventDefault();
    e.stopPropagation();
    state.reorderAppsMode = false;
    e.originalEvent.dataTransfer.clearData();
    $('.app-box-draggable')
      .removeClass('drag-drop-area-dragover')
      .css('opacity', '');
  });

  $appsWrapper.on('drop', '#newAppFiles', function (e) {
    if (state.reorderAppsMode) {
      return;
    }
    e.preventDefault();
    e.stopPropagation();
    state.dragAddAppCounter = 0;
    $(this).removeClass('drag-drop-area-dragover');
  });

  $appsWrapper.on('drop', '.app-box-draggable', function (e) {
    e.preventDefault();
    let filesDropped = false;
    if (!state.reorderAppsMode) {
      if (!e.originalEvent.dataTransfer.files) {
        return;
      }
      filesDropped = true;
    }
    state.reorderAppsMode = false;
    state.dragAddAppCounter = 0;
    $('.app-box-draggable')
      .removeClass('drag-drop-area-dragover')
      .css('opacity', '');

    const idToRaw = $(this).attr('id');
    const idTo = HTMLIDToUnicode(idToRaw.slice(7));
    if (filesDropped) {
      const filesTmp = [...e.originalEvent.dataTransfer.files];
      const fileTypes = filesTmp.map((fileTmp) => fileTmp.name.split('.').pop().toLowerCase());
      if (fileTypes.length === 1 && fileTypes[0] === 'miroapp') {
        showOverwriteDialog(idTo, true, fileTypes);
        return;
      }
      if (fileTypes.length > 10) {
        bootbox.alert({
          title: 'Too many files',
          message: 'Only up to 10 files can be added at once.',
          centerVertical: true,
        });
        $(`#appFiles_${unicodeToHTMLID(idTo)}_progress`).css('visibility', '');
        return;
      }
      const invalidFileTypes = fileTypes.filter(
        (fileType) => !SUPPORTED_FILE_TYPES.includes(fileType),
      );
      if (invalidFileTypes.length === 0) {
        showOverwriteDialog(idTo, true, fileTypes);
        return;
      }
      bootbox.alert({
        title: 'Invalid file',
        message: `The file you dropped (${escapeHtml(invalidFileTypes.join(','))}) is not supported. Please drop either a new MIRO app (.miroapp) to update the current version or a valid data file (${escapeHtml(SUPPORTED_FILE_TYPES.join(','))}).`,
        centerVertical: true,
      });
      $(`#appFiles_${unicodeToHTMLID(idTo)}_progress`).css('visibility', '');
      return;
    }

    const idFromRaw = e.originalEvent.dataTransfer.getData('text/plain');
    const idFrom = HTMLIDToUnicode(idFromRaw.slice(7));
    if (idFrom !== idTo) {
      sendUpdateAppOrderRequest(idFrom, idTo, idFromRaw, idToRaw);
    }
  });

  $appsWrapper.on('click', '.app-box', function (e) {
    const $this = $(this);
    if ($this.hasClass('app-box-expanded')) {
      return;
    }

    e.preventDefault();
    e.stopPropagation();
    $this.children('.app-data-file-input').hide();
    const $target = $(e.target);
    if (
      dom.$overlay.is(':visible')
      || $target.hasClass('cancel-btn')
      || $target.hasClass('delete-app-button')
      || $target.parents('.delete-app-button').length
    ) {
      return;
    }
    // only mark the box as expanded once it really opens (e.g. not when its delete
    // button was clicked), as expanded boxes disable the hover styles of all tiles
    pinTileHeights();
    $this.css('height', '').addClass('app-box-expanded');
    const appIDEncoded = this.dataset.id;
    const appID = HTMLIDToUnicode(appIDEncoded);
    const appIndex = this.dataset.index;
    if (appIndex) {
      $(`#appBox_${appIDEncoded}`).removeClass('app-box-fixed-height');
      $(`#appLogo_${appIDEncoded}`)
        .html(
          `<label for="updateMiroAppLogo" style="width:100%;height:100%;">
                  <div class="drag-drop-area-text">
                  ${PLACEHOLDERS.appLogo}
                  </div>
                </label>
                <input class="input-btn input-app-logo" style="margin-top:1rem;" type="file" name="updateMiroAppLogo" id="updateMiroAppLogo" data-restore="">`,
        )
        .addClass('drag-drop-area input-group')
        .after(`<div id="updateMiroAppLogo_progress" class="progress active shiny-file-input-progress">
                  <div class="progress-bar"></div>
                </div>`);
      $(`#appTitle_${appIndex}`).show();
      $(`#staticAppTitle_${appIndex}`).hide();
      $(`#appBox_${appIDEncoded} .app-id-field`).hide();
      $(`#appDesc_${appIndex}`).show();
      $(`#appEnv_${appIndex}`).show();
      $(`#staticAppDesc_${appIndex}`).hide();
      $(`#appGroupsWrapper_${appIndex}`).show();
      Shiny.bindAll(document.getElementById(`appBox_${appIDEncoded}`));
    }
    $(`#appBox_${appIDEncoded} .edit-bt-group`).slideDown(200);
    $this.css('z-index', 11);
    state.currentAppId = appID;
    dom.$overlay.data('current', $this).fadeIn(300);
  });

  $appsWrapper.on('click', '.btn-save-changes', function () {
    const appIndex = this.dataset.index;
    const appID = HTMLIDToUnicode(this.dataset.id);
    if (!appIndex || !appID) {
      bootbox.alert({
        title: 'Unexpected error',
        message:
          'Ooops. Something went wrong. Please contact GAMS if this error persists!',
        centerVertical: true,
      });
      return;
    }
    sendUpdateRequest(appIndex, appID);
  });

  $appsWrapper.on('click', '.delete-app-button', function () {
    const appIndex = this.dataset.index;
    const appId = HTMLIDToUnicode(this.dataset.id);
    bootbox.confirm({
      message:
        'Are you sure you want to remove this app? This cannot be undone.',
      centerVertical: true,
      onEscape: false,
      callback: (removeAppConfirmed) => {
        if (!removeAppConfirmed) {
          return;
        }
        bootbox.prompt({
          required: true,
          title: 'Remove data',
          message: `Do you want to permanently remove all data belonging to this app? Please type <b>${escapeHtml(appId)}</b> to confirm or press 'Cancel' to keep the data.`,
          centerVertical: true,
          onEscape: false,
          callback: (result) => {
            if (result === null) {
              sendRemoveRequest(appIndex, false);
              return true;
            }
            if (result !== appId) {
              return false;
            }
            sendRemoveRequest(appIndex, true);
            return true;
          },
        });
      },
    });
  });

  $appsWrapper.on('change', '#newAppName', () => {
    const newAppName = document.getElementById('newAppName').value;

    if (newAppName != null && newAppName.trim() !== '') {
      document.getElementById('btAddApp').disabled = false;
    } else {
      document.getElementById('btAddApp').disabled = true;
    }
  });
}
