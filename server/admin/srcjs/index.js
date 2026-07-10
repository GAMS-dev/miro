/* global $:false Shiny: false */
import 'core-js/stable';
import 'regenerator-runtime/runtime';

import { dom, state } from './state';
import setupEventListeners from './events';
import setupShinyHandlers from './shiny';
import { registerSelectizeInputs } from './ui';
import { HTMLIDToUnicode } from './utils';
import { sendUpdateAppShinyEvent } from './api';

$(() => {
  dom.$overlay = $('#overlayScreen');
  dom.$loadingScreen = $('#loading-screen');
  dom.$appsWrapper = $('#appsWrapper');

  if ($('.select-app-groups').length > 0) {
    state.currentGroupList = [];
    $('.select-app-groups')
      .first()
      .children('option')
      .each(function () {
        state.currentGroupList.push($(this).val());
      });
  }

  registerSelectizeInputs();
  setupEventListeners();
  setupShinyHandlers();

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

  $(document).on('keyup', (event) => {
    if (event.key === 'Enter' && !event.ctrlKey) {
      if (
        $('#shiny-modal').find('.selectize-input.input-active').length > 0
        || $('#shiny-modal').find('*[data-dismiss="modal"]').is(':focus')
      ) {
        return;
      }
      $('.bt-gms-confirm:visible:enabled').trigger('click');
    }
  });

  $(document).on('dragover', '.drag-drop-area', (e) => {
    e.preventDefault();
    e.stopPropagation();
  });

  $(document).on('dragenter', '.drag-drop-area', function (e) {
    if (state.reorderAppsMode) {
      return;
    }
    e.preventDefault();
    e.stopPropagation();
    state.dragAddAppCounter += 1;
    $(this).addClass('drag-drop-area-dragover');
  });

  $(document).on('dragleave', '.drag-drop-area', function (e) {
    if (state.reorderAppsMode) {
      return;
    }
    e.preventDefault();
    e.stopPropagation();
    state.dragAddAppCounter -= 1;
    if (state.dragAddAppCounter === 0) {
      $(this).removeClass('drag-drop-area-dragover');
    }
  });

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
});
