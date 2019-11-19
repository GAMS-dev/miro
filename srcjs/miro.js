/* global $:false Shiny: false HTMLWidgets:false MathJax:false Selectize:false */

const spinnerActive = {};

function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

function changeActiveButtons(tabId) {
  switch (tabId) {
    case 'inputData':
      $('#btImport').show();
      $('.btSolve').show();
      $('#btInterrupt').hide();
      $('#btSplitView').hide();
      $('#btCompareScen').hide();
      break;

    case 'outputData':
      $('#btImport').hide();
      $('.btSolve').hide();
      $('#btInterrupt').hide();
      $('#btSplitView').hide();
      $('#btCompareScen').hide();
      break;

    case 'gamsinter':
      $('#btImport').hide();
      $('.btSolve').hide();
      $('#btInterrupt').show();
      $('#btSplitView').hide();
      $('#btCompareScen').hide();
      break;

    case 'scenarios':
      $('#btImport').hide();
      $('.btSolve').hide();
      $('#btInterrupt').hide();
      $('#btSplitView').show();
      $('#btCompareScen').show();
      break;

    default:
      $('#btImport').hide();
      $('.btSolve').hide();
      $('#btInterrupt').hide();
      $('#btSplitView').hide();
      $('#btCompareScen').hide();
  }
}

function switchTab(el) {
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

export function showSpinnerIcon(el, delay = 3000) {
  if (spinnerActive[$(el).prop('id')]) {
    return;
  }

  spinnerActive[$(el).prop('id')] = true;
  const content = $(el).html();
  $(el).html('<i class="fa fa-refresh fa-spin"></i>');
  setTimeout(() => {
    $(el).html(content);
    spinnerActive[$(el).prop('id')] = false;
  }, delay);
}

export function changeTab(object, idActive, idRefer) {
  const tabPane = object.closest('.tabbable');
  tabPane.find(`li:nth-of-type(${idActive})`).removeClass();
  tabPane.find(`li:nth-of-type(${idRefer})`).addClass('active');
  tabPane.find(`.tab-content div:nth-child(${idActive})`).removeClass('active');
  tabPane.find(`.tab-content div:nth-child(${idRefer})`).addClass('active');
}

function switchTabInTabset(tabsetID, tabValue) {
  const tabset = $(`#${tabsetID}`);
  tabset.find(`a[data-value='${tabValue}']`).tab('show');
}

function removeModal() {
  $('#shiny-modal-wrapper').find('.modal').modal('hide');
}

export function slideToggleEl(data) {
  if (data.toggleIconDiv !== undefined) {
    if ($(data.id).is(':visible')) {
      $(data.toggleIconDiv).html('<i class="fa fa-plus"></i>');
    } else {
      $(data.toggleIconDiv).html('<i class="fa fa-minus"></i>');
    }
  }
  let duration = 400;
  if (data.duration === undefined) {
    ({ duration } = data);
  }
  $(data.id).slideToggle(duration);
}

export function showNewNameBaseDialog() {
  $('#base-overwrite-container').hide();
  $('#loadBase_snameExistsMsg').hide();
  $('#loadBase_newName').show();
  $('#btCheckSnameBase').show();
}

export function confirmModalShow(title, desc, cancelTxt, confirmTxt = null, confirmCall = null) {
  const btDataDismiss = `<button type="button" class="btn btn-default" data-dismiss="modal">${cancelTxt}</button>`;
  let btDataConfirm = '';

  if (confirmCall !== null) {
    btDataConfirm = '<button type="button" class="btn btn-default bt-highlight-1 bt-gms-confirm" '
      + `id="" onclick="${confirmCall}" data-dismiss="modal">${confirmTxt}</button>`;
  }

  const cModal = $('#confirmModal');
  cModal.find('.modal-title').html(title);
  cModal.find('.modal-body').html(desc);
  cModal.find('.modal-footer').html(btDataDismiss + btDataConfirm);
  cModal.modal('show');
}

export function removeAttachment(elId) {
  $(`#btRemoveAttachment_${elId}`).parent().parent().remove();
  Shiny.setInputValue(`btRemoveAttachment_${elId}`, 1, {
    priority: 'event',
  });
}
export function downloadAttachment(elId) {
  Shiny.setInputValue('downloadAttachment', elId, {
    priority: 'event',
  });
  setTimeout(() => {
    $('#downloadAttachmentData')[0].click();
  }, 200);
}

export function changeDDButtonEvent(elText, DDBtnID, actionID) {
  $(DDBtnID).attr('onclick',
    `Shiny.setInputValue('${actionID}',1,{priority: 'event'});`);
  $(DDBtnID).text(elText);
  if ($(DDBtnID).is(':enabled')) {
    Shiny.setInputValue(actionID, 1, {
      priority: 'event',
    });
  }
}

export function showJobsDialog(hcubeMode) {
  removeModal();
  if (hcubeMode) {
    switchTab('importData');
    return;
  }
  switchTab('gamsinter');
  switchTabInTabset('jobListPanel', 'joblist');
}

export function renderMathJax() {
  MathJax.Hub.Queue(['Typeset', MathJax.Hub, 'wrapper-documentation']);
}

export function validateSname(el, inputID = 'btCheckSnameLocalConfirm') {
  if (/^[a-f0-9]{64}$/i.test($(el).val()) !== true && /^\s*$/.test($(el).val()) !== true) {
    $(el).removeClass('invalidInput');
    if (inputID === 'internal') {
      return true;
    }
    Shiny.setInputValue(inputID, 1, {
      priority: 'event',
    });
    return true;
  }
  $(el).addClass('invalidInput');
  return false;
}

export function validateHcubeHash() {
  const hashVal = $('#hcHashLookup').val();

  if (/^[a-f0-9]{64}$/i.test(hashVal) === true) {
    if (!validateSname('#hcube_newScenName', 'internal')) {
      return;
    }
    $('#hcHashLookup').removeClass('invalidInput');
    Shiny.setInputValue('hcHashLookup', hashVal, {
      priority: 'event',
    });
    return;
  }

  $('#hcHashLookup').addClass('invalidInput');
}

export function hcHashImport(sid) {
  if (!validateSname('#hcube_newScenName', 'internal')) {
    return;
  }
  Shiny.setInputValue('loadHcubeHashSid', sid, {
    priority: 'event',
  });
}

export async function jumpToLogMark(id) {
  switchTab('gamsinter');
  $('#logFileTabsset [data-value="mirolog"]').tab('show');
  await sleep(200);
  const el = $(`#mlogMark_${id}`);
  if (el !== undefined) {
    el[0].scrollIntoView();
    el.animate({ backgroundColor: 'yellow' }, 400)
      .delay(1000)
      .animate({ backgroundColor: 'transparent' }, 400);
  }
}

function isInputEl(id) {
  if ($(id).parents('.form-group').length) {
    return true;
  }
  return false;
}
function rerenderDygraph(delay = 100) {
  try {
    setTimeout(() => {
      HTMLWidgets.getInstance($('.dygraphs:visible').get(0)).dygraph.resize();
    }, delay);
  } catch (e) {
    // continue regardless of error
  }
}
function rerenderHot(delay = 100) {
  setTimeout(() => {
    const el = $('.rhandsontable:visible').get(0);
    if (el !== undefined) {
      HTMLWidgets.getInstance(el).hot.render();
    }
  }, delay);
}

function showHideEl(el, delay, msg = null) {
  if (msg !== null) {
    $(el).text(msg);
  }
  $(el).show().delay(delay).fadeOut();
}

function scrollDown(id, delay = 500) {
  setTimeout(() => {
    $(id).animate({
      scrollTop: $(id)[0].scrollHeight - $(id)[0].clientHeight,
    }, 300);
  }, delay);
}

$(document).ready(() => {
  $('#toolCategories').on('click', '.category-btn', function () {
    const catId = this.dataset.cat;
    const catBody = $(`.cat-body-${catId}`);
    $('.cat-body:visible').hide();
    $('.category-btn').removeClass('category-btn-active');
    $(this).addClass('category-btn-active');
    catBody.show();
  });
  $('.dropdown').on('show.bs.dropdown', (e) => {
    const ddButton = $(e.target);
    const dropdown = ddButton.children('.dropdown-menu').first();
    if (dropdown.width() <= ddButton.width()
      + ddButton.offset().left - ddButton.parent().offset().left) {
      dropdown.addClass('dropdown-menu-right');
    } else {
      dropdown.removeClass('dropdown-menu-right');
    }
  });
  // code snippet taken from SwishWez: https://stackoverflow.com/questions/21582558/disable-remove-on-backspace-or-remove-ibeam-entirely
  if (typeof Selectize !== 'undefined') {
    Selectize.define('no_delete', function () {
      const self = this;
      this.deleteSelection = (function () {
        const original = self.deleteSelection;
        return function (e, ...args) {
          if (!e || (e.keyCode !== 8 && e.keyCode !== 46)) {
            return original.apply(this, args);
          }
          return false;
        };
      }());
    });
  }

  $('body').addClass('fixed'); // besides these updates, gms-switchTab (see below) has always has to be considered as well

  $('#btImport').show();
  $('.btSolve').show();
  $('#btInterrupt').hide();
  $('#btSplitView').hide();
  $('#btCompareScen').hide();
  $('#btLoadScen').hide();
  $('a[data-value="inputData"]').click(() => {
    changeActiveButtons('inputData');
    rerenderHot();
  });

  $('a[data-value="outputData"]').click(() => {
    changeActiveButtons('outputData');
  });
  $('a[data-value="gamsinter"]').click(() => {
    changeActiveButtons('gamsinter');
  });
  $('a[data-value="scenarios"]').click(() => {
    changeActiveButtons('scenarios');
  });
  $('#scenTabset').on('click', 'a[data-toggle="tab"]', () => {
    rerenderDygraph();
  });
  $('a[data-value="advanced"],a[data-value="importData"],a[data-value="loadResults"],a[data-value="hcubeAnalyze"]').click(() => {
    changeActiveButtons('default');
  });
  $('#inputTabset li').click(() => {
    rerenderHot();
  });
  $('#scenTabset').append('<li id="scenTabsetAdd"><a href="#" id="btLoadScen" data-value="scen_add" '
    + 'onclick="Shiny.setInputValue(\'btLoadScen\', 1, {priority: \'event\'});">'
    + '<i class="far fa-plus-square" style="font-size:13pt;"></i></a></li>'); // show/hide buttons after (R triggered) tab switch.

  Shiny.addCustomMessageHandler('gms-switchTab', (el) => {
    switchTab(el);
  });
  $('body').on('click', '.bt-highlight-1, .bt-highlight-2, .bt-highlight-3', function () {
    const btn = $(this);
    if (btn.hasClass('dropdown-toggle')) {
      return;
    }
    btn.prop('disabled', true);
    setTimeout(() => {
      btn.prop('disabled', false);
    }, 1500);
  }); // hide pivot filter boxes when clicked outside of box

  $(document).click((e) => {
    const { target } = e;

    if (!$(target).is('.pvtAttr') && !$(target).parents('.pvtAttr').length && !$(target).is('.pvtFilterBox') && !$(target).parents('.pvtFilterBox').length) {
      $('.pvtFilterBox').hide();
    }
  });
  $('.sidebar-toggle').click(() => {
    rerenderHot(400);
  });
  window.addEventListener('beforeunload', (e) => {
    if ($('#shiny-disconnected-overlay').length === 0) {
      e.preventDefault();
      e.returnValue = 'Are you sure you want to leave? Unsaved changes will be lost!';
    }
  });
  Shiny.addCustomMessageHandler('gms-showEl', (id) => {
    if (isInputEl(id)) {
      $(id).closest('.shiny-input-container').show();
    } else {
      $(id).show();
    }
  });
  Shiny.addCustomMessageHandler('gms-scriptExecuted', (data) => {
    let scriptOutputContainer;
    if (data.hcube === true) {
      scriptOutputContainer = $('#scriptOutput_hcube');
      scriptOutputContainer.show();
      scriptOutputContainer = scriptOutputContainer.children('.script-output');
    } else if (data.sid == null) {
      scriptOutputContainer = $(`#scriptOutput_${data.id} .script-output`);
      Shiny.setInputValue('outputGenerated', 1,
        {
          priority: 'event',
        });
      $(`#scriptOutput_${data.id} .script-spinner`).hide();
    } else {
      scriptOutputContainer = $(`#scenScript_${data.sid}_${data.id}`);
    }
    const scriptOutputContainerIframe = scriptOutputContainer[0].contentWindow.document;
    scriptOutputContainerIframe.open();
    scriptOutputContainerIframe.write(data.isError === true ? `<div style='margin:5px;color:#F39619;font-weight:bold;font-size:15pt;text-align:center;'>\
${data.data}</div>` : data.data);
    scriptOutputContainerIframe.close();
    scriptOutputContainer.show();
  });
  Shiny.addCustomMessageHandler('gms-showElReplaceTxt', (data) => {
    $(data.id).text(data.txt).show();
  });
  Shiny.addCustomMessageHandler('gms-showLogContent', (data) => {
    $(data.id).text(data.content);
    if (data.chunkCount <= 1) {
      return;
    }

    const { noChunks } = data;
    if (noChunks === undefined || noChunks <= 1) {
      return;
    }
    let counter = 1;
    let isLoading = false;
    $(data.id).on('scroll', () => {
      if ($(data.id)[0].scrollHeight - $(data.id).scrollTop()
           < $(data.id).outerHeight() + 200
          && isLoading === false) {
        isLoading = true;
        Shiny.setInputValue('loadTextEntityChunk',
          {
            jID: data.jID,
            chunkCount: counter,
            type: data.type,
          }, {
            priority: 'event',
          });
        counter += 1;

        if (counter === noChunks) {
          $(data.id).off('scroll');
        }
      }
    });
    $(data.id).on('change', () => {
      setTimeout(() => {
        isLoading = false;
      }, 1000);
    });
  });
  Shiny.addCustomMessageHandler('gms-hideEl', (id) => {
    if (isInputEl(id)) {
      $(id).closest('.shiny-input-container').hide();
    } else {
      $(id).hide();
    }
  });
  Shiny.addCustomMessageHandler('gms-showHideEl', (data) => {
    showHideEl(data.id, data.delay, data.msg);
  });
  Shiny.addCustomMessageHandler('gms-enableEl', (id) => {
    $(id).prop('disabled', false);
  });
  Shiny.addCustomMessageHandler('gms-disableEl', (id) => {
    $(id).prop('disabled', true);
  });
  Shiny.addCustomMessageHandler('gms-toggleEl', (id) => {
    if ($(id).is(':visible')) {
      $(id).toggle();
      $(id).trigger('shown');
    } else {
      $(id).fadeToggle(600);
      $(id).trigger('hidden');
    }
  });
  Shiny.addCustomMessageHandler('gms-slideToggleEl', (data) => {
    slideToggleEl(data);
  });
  Shiny.addCustomMessageHandler('gms-addClassEl', (el) => {
    $(el.id).addClass(el.newclass);
  });
  Shiny.addCustomMessageHandler('gms-removeClassEl', (el) => {
    $(el.id).removeClass(el.oldclass);
  });
  Shiny.addCustomMessageHandler('gms-emptyEl', (id) => {
    $(id).empty();
  });
  Shiny.addCustomMessageHandler('gms-appendEl', (data) => {
    let { content } = data;
    if (data.text) {
      content = document.createTextNode(content);
    }
    $(data.id).append(content);

    if (data.triggerChange) {
      $(data.id).trigger('change');
    }

    if (data.scroll) {
      scrollDown(data.id, 50);
    }
  });
  Shiny.addCustomMessageHandler('gms-scrollDown', (id) => {
    scrollDown(id);
  });
  Shiny.addCustomMessageHandler('gms-startUpdateJobProgress', (data) => {
    const interval = setInterval(() => {
      if ($(data.id).attr('aria-valuenow') === '100'
        || !$(data.id).is(':visible')) {
        clearInterval(interval);
      }
      Shiny.setInputValue('updateJobProgress', data.jID, {
        priority: 'event',
      });
    }, 5000);
  });
  Shiny.addCustomMessageHandler('gms-updateJobProgress', (data) => {
    if (!$(data.id).is(':visible')) {
      return;
    }
    const percentCompleted = Math.round(parseInt(data.progress.noCompleted, 10)
          / parseInt(data.progress.noTotal, 10) * 100);
    $(data.id)
      .css('width', `${percentCompleted}%`)
      .attr('aria-valuenow', percentCompleted)
      .text(`${data.progress.noCompleted}/${data.progress.noTotal}`);
  });
  Shiny.addCustomMessageHandler('gms-markJobDownloadComplete', (data) => {
    if (data.triggerImport === true
      && $(`#jobImportDlProgressWrapper_${data.id}`).is(':visible')) {
      Shiny.setInputValue('importJob', data.id, { priority: 'event' });
    }
    $(`#jobImportDlProgressWrapper_${data.id}`).siblings('div:first').text(data.text);
    $(`#jobImportDlProgressWrapper_${data.id}`).hide();
    $(`#btDownloadJob_${data.id}`).hide();
    $(`#btImportJob_${data.id}`).show();
  });
  Shiny.addCustomMessageHandler('gms-hideModal', (delay) => {
    setTimeout(() => {
      $('#shiny-modal-wrapper').find('.modal').modal('hide');
    }, delay * 1000);
  });
  Shiny.addCustomMessageHandler('gms-updateAttachList', (el) => {
    const id = $.makeArray(el.id);
    const name = $.makeArray(el.name);

    for (let i = 0; i < id.length; i += 1) {
      let checkBoxHTML = '';
      if (el.allowExec) {
        checkBoxHTML = `<div class="col-sm-6"><div class="form-group shiny-input-container"><div class="checkbox"><label><input type="checkbox" onchange="Shiny.setInputValue('execPermAttachment_${id[i]}', $(this).is(':checked'));" checked="checked"><span>${el.labelCb}</span></label></div></div></div>`;
      }

      $(`<div class="row attachment-line"><div class="col-sm-6"><button class="btn btn-default bt-icon" id="btRemoveAttachment_${id[i]}" type="button" onclick="Miro.removeAttachment(${id[i]})"><i class="fa fa-times-circle"></i></button><a href="#" onclick="Miro.downloadAttachment(${id[i]})"> ${name[i]}</a></div>${checkBoxHTML}</div>`).insertBefore('#endAttachList');
    }
  });
  Shiny.addCustomMessageHandler('gms-fitTitleInBox', (id) => {
    setTimeout(() => {
      const el = $(id);
      const parentEl = el.parent()[0];
      el.css('font-size', '18px');
      let currSize = 18;

      while (parentEl.scrollWidth > parentEl.clientWidth && currSize >= 10) {
        currSize -= 2;
        el.css('font-size', `${currSize}px`);
      }
    }, 500);
  });
  Shiny.addCustomMessageHandler('gms-showValidationErrors', (content) => {
    const inSyms = Object.keys(content);
    $('.input-validation-error').empty();
    inSyms.forEach((key) => {
      if (Array.isArray(content[key])) {
        content[key].forEach((item) => {
          $(`#valErr_${key}`).append(item);
        });
      } else {
        $(`#valErr_${key}`).append(content[key]);
      }
    });
    $('.input-validation-error').show();
  });
  const autoNumericBinding = new Shiny.InputBinding();
  $.extend(autoNumericBinding, {
    find(scope) {
      return $(scope).find('.miro-auto-numeric');
    },
    getValue(el) {
      return parseFloat($(el).autoNumeric('get'));
    },
    setValue(el, value) {
      $(el).autoNumeric('set', value);
    },
    receiveMessage(el, data) {
      if (Object.prototype.hasOwnProperty.call(data, 'value')) {
        $(el).autoNumeric('set', data.value);
      }
    },
    subscribe(el, callback) {
      $(el).on('change.autoNumericBinding', () => {
        callback(true);
      });
    },
    getRatePolicy() {
      return {
        policy: 'debounce',
        delay: 250,
      };
    },
    initialize(el) {
      $(el).autoNumeric('init');
    },
    unsubscribe(el) {
      $(el).autoNumeric('destroy');
    },
  });
  Shiny.inputBindings.register(autoNumericBinding);
});

// counter
let count = 1; // maximum number of scenarios that can be loaded in compare view

const maxNumScen = 50;
$(document).keyup((event) => {
  if (event.keyCode === 13 && !event.ctrlKey) {
    if ($('#shiny-modal').find('.selectize-input.input-active').length > 0
      || $('#shiny-modal').find('*[data-dismiss="modal"]').is(':focus')) {
      return;
    }

    $('.bt-gms-confirm:visible:enabled').click();
    return;
  } // ENTER will confirm modal dialogues

  if (event.keyCode === 27) {
    $('.modal').modal('hide');
    return;
  } // ESC will close modal dialogues

  if (!event.ctrlKey || !event.altKey) {
    return;
  }

  if (event.keyCode === 73) {
    if ($('#btImport').is(':visible')) {
      $('#btImport').click();
    } else if ($('#btLoadScen').is(':visible')) {
      $('#btLoadScen').click();
    }
    return;
  } // Import shortcut: CTRL + ALT + I


  if (event.keyCode === 83) {
    Shiny.setInputValue('btSave', 1, {
      priority: 'event',
    });
    return;
  } // SAVE shortcut: CTRL + ALT + S


  if (event.keyCode === 13) {
    $('#btSolve:visible:enabled').click();
    return;
  } // Solve shortcut: CTRL + ALT + ENTER


  if (event.keyCode === 82) {
    Shiny.setInputValue('btDelete', 1, {
      priority: 'event',
    });
    return;
  } // Remove shortcut: CTRL + ALT + R


  if (event.keyCode === 67) {
    $('.btRemove:visible').click();
    return;
  } // Close shortcut (remove button in input sheet): CTRL + ALT + C


  if (event.keyCode === 67) {
    for (let i = 2; i <= maxNumScen; i += 1) {
      $(`#close_${i}:visible`).click();
    }
    return;
  } // Close shortcut (remove button in output sheet): CTRL + ALT + C


  if (event.keyCode === 70) {
    $('body').toggleClass('sidebar-collapse');
    rerenderHot(400);
    return;
  } // Fullscreen mode (hide sidebar) shortcut: CTRL + ALT + F


  if (event.keyCode === 49) {
    $('a[href="#shiny-tab-inputData"]').click();
    return;
  } // Select input menu shortcut: CTRL + ALT + 1


  if (event.keyCode === 50) {
    const tab = $('a[href="#shiny-tab-outputData"]');

    if (tab.length > 0) {
      tab.click();
    } else {
      $('a[href="#shiny-tab-importData"]').click();
    }
    return;
  } // Select output menu shortcut: CTRL + ALT + 2


  if (event.keyCode === 51) {
    const tab = $('a[href="#shiny-tab-gamsinter"]');

    if (tab.length > 0) {
      tab.click();
    } else {
      $('a[href="#shiny-tab-loadResults"]').click();
    }
    return;
  } // Select gams interaction menu shortcut: CTRL + ALT + 3


  if (event.keyCode === 52) {
    $('a[href="#shiny-tab-scenarios"]').click();
    return;
  }// Select scenario menu shortcut: CTRL + ALT + 4

  if (event.keyCode === 53) {
    const tab = $('a[href="#shiny-tab-hcubeAnalyze"]');

    if (tab.length > 0) {
      tab.click();
    }
    return;
  } // Select scenario menu shortcut: CTRL + ALT + 5


  if (event.keyCode === 84) {
    if ($('#btGraphIn').is(':visible')) {
      $('#btGraphIn:enabled').click();
      return;
    }

    if ($('#outputTableView').is(':visible')) {
      $('#outputTableView').click();
      return;
    }

    for (let i = 2; i <= maxNumScen + 3; i += 1) {
      $(`#table_${i}:visible`).click();
    }
    return;
  } // Table view (scenario compare mode) shortcut: CTRL + ALT + T


  if (event.keyCode === 39) {
    Shiny.onInputChange('tabsetShortcutNext', count);
    count += 1;
    return;
  } // Select next tab shortcut: CTRL + ALT + arrow right


  if (event.keyCode === 37) {
    Shiny.onInputChange('tabsetShortcutPrev', count);
    count += 1;
    return;
  } // Select previous tab shortcut: CTRL + ALT + arrow left


  if (event.keyCode === 40) {
    Shiny.onInputChange('tabsetShortcutNest', count);
    count += 1;
    return;
  } // Nest to next lower tabset shortcut: CTRL + ALT + arrow down


  if (event.keyCode === 38) {
    Shiny.onInputChange('tabsetShortcutUnnest', count);
    count += 1;
    return;
  } // Unnest to next higher tabset shortcut: CTRL + ALT + arrow up


  if (event.keyCode === 32 && $('#btCompareScen').is(':enabled') && $('#btCompareScen').is(':visible')) {
    $('#btCompareScen').click();
  }// Activate/deactivate scenario comparison mode: CTRL + ALT + space
});
