/* global $:false HTMLWidgets:false Chart:false */

export function sleep(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

function changeChartjsTheme(dark = false) {
  if (typeof Chart === 'undefined') {
    return;
  }
  if (dark) {
    Chart.defaults.global.defaultFontColor = 'white';
  } else {
    Chart.defaults.global.defaultFontColor = '#666';
  }
  Chart.helpers.each(Chart.instances, (instance) => {
    instance.chart.update();
  });
}

export function changeTheme(dark = false) {
  changeChartjsTheme(dark);
}

export function changeActiveButtons(tabId) {
  switch (tabId) {
    case 'inputData':
      $('#btImport').show();
      $('.btSolve').show();
      $('#btInterrupt').hide();
      $('.btSplitView').hide();
      $('#btCompareScen').hide();
      break;

    case 'outputData':
      $('#btImport').hide();
      $('.btSolve').hide();
      $('#btInterrupt').hide();
      $('.btSplitView').hide();
      $('#btCompareScen').hide();
      break;

    case 'gamsinter':
      $('#btImport').hide();
      $('.btSolve').hide();
      $('#btInterrupt').show();
      $('.btSplitView').hide();
      $('#btCompareScen').hide();
      break;

    case 'scenarios':
      $('#btImport').hide();
      $('.btSolve').hide();
      $('#btInterrupt').hide();
      $('.btSplitView').show();
      if ($('#btCompareScen').attr('data-noshow') === 'false') {
        $('#btCompareScen').show();
      }
      break;

    default:
      $('#btImport').hide();
      $('.btSolve').hide();
      $('#btInterrupt').hide();
      $('.btSplitView').hide();
      $('#btCompareScen').hide();
  }
}

export function switchTabInTabset(tabsetID, tabValue) {
  const tabset = $(`#${tabsetID}`);
  tabset.find(`a[data-value='${tabValue}']`).tab('show');
}

export function removeModal() {
  $('#shiny-modal-wrapper').find('.modal').modal('hide');
}

export function switchTab(el) {
  switch (el) {
    case 'input':
      changeActiveButtons('inputData');
      $('[href="#shiny-tab-inputData"]').tab('show');
      break;

    case 'output':
      changeActiveButtons('outputData');
      $('[href="#shiny-tab-outputData"]').tab('show');
      break;

    case 'gamsinter':
      changeActiveButtons('gamsinter');
      $('[href="#shiny-tab-gamsinter"]').tab('show');
      break;

    case 'importData':
      changeActiveButtons('importData');
      $('[href="#shiny-tab-importData"]').tab('show');
      break;

    case 'hcubeAna':
      changeActiveButtons('default');
      $('[href="#shiny-tab-hcubeAnalyze"]').tab('show');
      break;

    case 'scenComp':
      changeActiveButtons('scenarios');
      $('[href="#shiny-tab-scenarios"]').tab('show');
      break;
    default:
      break;
  }
}

export function isInputEl(id) {
  if ($(id).parents('.form-group').length) {
    return true;
  }
  return false;
}
export function rerenderDygraph(delay = 100) {
  try {
    setTimeout(() => {
      HTMLWidgets.getInstance($('.dygraphs:visible').get(0)).dygraph.resize();
    }, delay);
  } catch (e) {
    // continue regardless of error
  }
}
export function rerenderHot(delay = 100) {
  setTimeout(() => {
    const el = $('.rhandsontable:visible').get(0);
    if (el !== undefined) {
      HTMLWidgets.getInstance(el).hot.render();
    }
  }, delay);
}

export function showHideEl(el, delay, msg = null) {
  if (msg !== null) {
    $(el).text(msg);
  }
  $(el).show().delay(delay).fadeOut();
}

export function scrollDown(id, delay = 500) {
  setTimeout(() => {
    $(id).animate({
      scrollTop: $(id)[0].scrollHeight - $(id)[0].clientHeight,
    }, 300);
  }, delay);
}
