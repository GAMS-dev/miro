import { dom, state } from './state';
import setupEventListeners from './events';
import setupShinyHandlers from './shiny';
import { registerSelectizeInputs } from './ui';

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
});
