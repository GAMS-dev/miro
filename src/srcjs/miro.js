/* global $:false Shiny: false Selectize:false renderMathInElement:false */

import 'core-js/stable';
import AutoNumeric from 'autonumeric';

import {
  sleep, changeActiveButtons, switchTabInTabset, removeModal,
  switchTab, isInputEl, rerenderDygraph, rerenderHot, showHideEl, scrollDown,
  changeTheme, LoadingScreen, colorPickerBinding, getActualHeight,
} from './util';

import ShortcutManager from './shortcut_manager';

import {
  activateMiroPivotPresentation, deactivateMiroPivotPresentation,
  activateMiroPivotPresentationObservers,
} from './miro_pivot';

const loadingScreen = new LoadingScreen();

export function changeTab(object, idActive, idRefer) {
  const tabPane = object.closest('.tabbable');
  tabPane.find(`li:nth-of-type(${idActive})`).removeClass();
  tabPane.find(`li:nth-of-type(${idRefer})`).addClass('active');
  tabPane.find(`.tab-content div:nth-child(${idActive})`).removeClass('active');
  tabPane.find(`.tab-content div:nth-child(${idRefer})`).addClass('active');
}

export function slideToggleEl(data) {
  if (data.toggleIconDiv !== undefined) {
    if ($(data.id).is(':visible')) {
      $(data.toggleIconDiv).html('<i class="fa fa-plus" role="presentation" aria-label="More options"></i>');
    } else {
      $(data.toggleIconDiv).html('<i class="fa fa-minus" role="presentation" aria-label="Less options"></i>');
    }
  }
  let duration = 400;
  if (data.duration === undefined) {
    ({ duration } = data);
  }
  $(data.id).slideToggle(duration);
}

export function confirmModalShow(
  title,
  desc,
  cancelTxt,
  confirmTxt = null,
  confirmCall = null,
) {
  const btDataDismiss = `<button type="button" class="btn btn-default" data-dismiss="modal">\
${cancelTxt}</button>`;
  let btDataConfirm = '';

  if (confirmCall !== null) {
    btDataConfirm = `<button type="button" class="btn btn-default bt-highlight-1 bt-gms-confirm" \
id="" onclick="${confirmCall}" data-dismiss="modal">${confirmTxt}</button>`;
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

export function showJobsDialog() {
  removeModal();
  switchTab('gamsinter');
  switchTabInTabset('jobListPanel', 'joblist');
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

export function sendSelectedRowsRequest(
  tableId,
  shinyId,
  noneSelectedErrorId = null,
  downloadLink = false,
) {
  const data = [];

  $(`#${tableId} tr.selected`).each(function () {
    data.push($(this).children('td')
      .map((i, el) => {
        const { val } = el.dataset;
        if (val != null && val !== '') {
          return atob(val);
        }
        return el.innerText;
      }).get());
  });
  if (noneSelectedErrorId && data.length === 0) {
    showHideEl(`#${noneSelectedErrorId}`, 4000);
    return;
  }
  Shiny.setInputValue(shinyId, JSON.stringify(data), {
    priority: 'event',
  });
  if (downloadLink === true) {
    setTimeout(() => {
      $(`#${shinyId}`)[0].click();
    }, 200);
  }
}

export function selectAllRows(tableId) {
  $(`#${tableId} tbody tr`).addClass('selected');
}
export function selectNoRow(tableId) {
  $(`#${tableId} tbody tr`).removeClass('selected');
}

export async function jumpToLogMark(id) {
  switchTab('gamsinter');
  $('#logFileTabsset [data-value="mirolog"]').tab('show');
  await sleep(200);
  const el = $(`#mlogMark_${id}`);
  if (el !== undefined) {
    el[0].scrollIntoView();
    el.fadeOut(100).fadeIn(100).fadeOut(100).fadeIn(100);
  }
}

export function resetDropdownFilter(that) {
  const dropdown = $(that).parent().children('ul');
  const filter = dropdown.children('input')[0];
  if (filter.value === '') {
    return;
  }
  filter.value = '';
  $(dropdown).children('li').show();
}

export function filterMiroDropdown(that) {
  const filterTxt = that.value.toLowerCase();
  $(that).siblings('li').each(function () {
    if (this.children[0].textContent.toLowerCase().indexOf(filterTxt) > -1) {
      this.style.display = '';
    } else {
      this.style.display = 'none';
    }
  });
}

export function modal(
  msg,
  okButton,
  cancelButton,
  value,
  callback,
  ...callbackArgs
) {
  Shiny.modal.show({
    html: `<div id="shiny-modal" class="modal fade"
    tabindex="-1" data-backdrop="static" data-keyboard="false">
  <div class="modal-dialog modal-sm">
    <div class="modal-content">
      <div class="modal-body">
         ${value == null ? `<label>${msg}</label>`
    : `<div class="form-group shiny-input-container">
            <label class="control-label" for="miroPromptInput">${msg}</label>
            <input id="miroPromptInput" type="text" class="form-control" value="${value}"/>
          </div>`}
      </div>
      <div class="modal-footer">
        <button type="button" class="btn btn-default" data-dismiss="modal">${cancelButton}</button>
        <button id="miroModalConfirmButton" type="button"
        class="btn btn-default bt-highlight-1 bt-gms-confirm">${okButton}</button>
      </div>
    </div>
  </div>
  <script>$('#shiny-modal').modal().focus();</script>
</div>`,
  });
  $(document).off('click', '#miroModalConfirmButton');
  if (value == null) {
    $(document).on('click', '#miroModalConfirmButton', () => {
      if (callback(...callbackArgs) !== false) {
        $('#shiny-modal').modal('hide');
      }
    });
  } else {
    $(document).on('click', '#miroModalConfirmButton', () => {
      if (callback(
        document.getElementById('miroPromptInput').value,
        ...callbackArgs,
      ) !== false) {
        $('#shiny-modal').modal('hide');
      }
    });
  }
}

export function parseKatex(element) {
  renderMathInElement(element, {
    throwOnError: false,
    delimiters: [{
      left: '$$',
      right: '$$',
      display: true,
    }, { left: '$', right: '$', display: false }],
  });
}

const shortcutManager = new ShortcutManager(50);
$(document).on('keyup', (event) => {
  shortcutManager.handleKeyPressEvent(event);
});

$(() => {
  if (typeof window.matchMedia('(prefers-color-scheme: dark)').addEventListener !== 'undefined') {
    // browser supports listening to matchMedia change
    window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', (e) => {
      changeTheme(e.matches);
    });
  }
  $(document).on('click', '.toggle-label-height', function () {
    const $this = $(this);
    $this.children('.fa').toggleClass('fa-circle-chevron-down fa-circle-chevron-up');
    $this.parent().prev().toggleClass('label-full label-collapsed', 500);
  });
  $('.label-wrapper').each(function () {
    const $this = $(this);
    if (getActualHeight($this, 'outerHeight') < 99.8) {
      $this.removeClass('label-collapsed');
      $this.next().hide();
    }
  });
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
  // change "More" tab dropdown label when tab from dropdown is selected
  $('.nav-tabs-dropdown a').on('shown.bs.tab', (e) => {
    const currTab = $(e.target);
    if (currTab.closest('.dropdown').find('.dropdown-toggle').length) {
      currTab.closest('.dropdown').find('.dropdown-toggle').html(`${currTab.text()} <i class="fa fa-angle-double-right" role="presentation" aria-label="More tabs"></i>`);
      return;
    }
    const prevTab = $(e.relatedTarget).closest('.dropdown').find('.dropdown-toggle');
    if (prevTab.length) {
      prevTab.html(` ${prevTab.data('defaultlabel')} <i class="fa fa-angle-double-right" role="presentation" aria-label="More tabs"></i>`);
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
  $(document).on('click', '.activate-pivot-controls', function () {
    activateMiroPivotPresentation(this.dataset.id);
  });
  $(document).on('click', '.deactivate-pivot-controls', function () {
    deactivateMiroPivotPresentation(this.dataset.id);
  });
  Shiny.addCustomMessageHandler('gms-activateMiroPivotPresentationObservers', (id) => {
    activateMiroPivotPresentationObservers(id);
  });
  $(document).on('click', '.btn-proxy', function () {
    setTimeout(() => {
      $(`#${this.dataset.proxyId}`)[0].click();
    }, 200);
  });

  $('.toggle-config-view-left').click(() => {
    $('#config-right-graph')[0].setAttribute('style', '-webkit-transition: width 0.3s ease;-moz-transition: width 0.3s ease;-o-transition: width 0.3s ease;transition: width 0.3s ease;');
    $('#config-left-graph')[0].setAttribute('style', '-webkit-transition: margin 0.3s ease;-moz-transition: margin 0.3s ease;-o-transition: margin 0.3s ease;transition: margin 0.3s ease;');

    $('#config-right-graph').removeClass('collapse-config-right').toggleClass('col-sm-12 col-sm-6');
    $('#config-left-graph').toggleClass('collapse-config-left');
    if ($('#config-right-graph').hasClass('col-sm-12')) {
      $('#config-left-graph').removeClass('col-sm-12').addClass('col-sm-6');
      $('#toggleFullscreenRight').find('i:last').removeClass('fa-compress').addClass('fa-expand');
    }
    $('#toggleFullscreenLeft').find('i:first').toggleClass('fa-expand fa-compress');
    $(window).trigger('resize');
    return false;
  });
  $('.toggle-config-view-right').click(() => {
    $('#config-left-graph')[0].setAttribute('style', '-webkit-transition: width 0.3s ease;-moz-transition: width 0.3s ease;-o-transition: width 0.3s ease;transition: width 0.3s ease;');
    $('#config-right-graph')[0].setAttribute('style', '-webkit-transition: margin 0.3s ease;-moz-transition: margin 0.3s ease;-o-transition: margin 0.3s ease;transition: margin 0.3s ease;');

    $('#config-left-graph').removeClass('collapse-config-left').toggleClass('col-sm-12 col-sm-6');
    $('#config-right-graph').toggleClass('collapse-config-right');
    if ($('#config-left-graph').hasClass('col-sm-12')) {
      $('#config-right-graph').removeClass('col-sm-12').addClass('col-sm-6');
      $('#toggleFullscreenLeft').find('i:first').removeClass('fa-compress').addClass('fa-expand');
    }
    $('#toggleFullscreenRight').find('i:last').toggleClass('fa-expand fa-compress');
    $(window).trigger('resize');
    return false;
  });

  $('body').addClass('fixed'); // besides these updates, gms-switchTab (see below) has always has to be considered as well

  $('#btImport').show();
  $('.btSolve').show();
  $('#btInterrupt').hide();
  $('.btSplitView').hide();
  $('#btCompareScen').hide();
  $('#btLoadScen').hide();
  $('a[data-value="inputData"]').click(() => {
    changeActiveButtons('inputData');
    rerenderHot();
  });

  $('a[data-value="outputData"]').on('click', () => {
    changeActiveButtons('outputData');
  });
  $('a[data-value="gamsinter"]').on('click', () => {
    changeActiveButtons('gamsinter');
  });
  $('a[data-value="scenarios"]').on('click', () => {
    changeActiveButtons('scenarios');
  });
  $('#scenTabset').on('click', 'a[data-toggle="tab"]', () => {
    rerenderDygraph();
  });
  $('a[data-value="advanced"],a[data-value="importData"],a[data-value="loadResults"]').on('click', () => {
    changeActiveButtons('default');
  });
  $('#inputTabset li').on('click', () => {
    rerenderHot();
  });
  $('#scenTabset').append('<li id="scenTabsetAdd"><a href="#" id="btLoadScen" data-value="scen_add" '
    + 'onclick="Shiny.setInputValue(\'btLoadScen\', 1, {priority: \'event\'});">'
    + '<i class="far fa-square-plus" style="font-size:13pt;" role="presentation" aria-label="Add scenario"></i></a></li>'); // show/hide buttons after (R triggered) tab switch.

  Shiny.addCustomMessageHandler('gms-switchTab', (el) => {
    switchTab(el);
  });
  $(document).on('click', '#miroPivotCbCustomColorInputs', function () {
    if (this.checked) {
      $('.miro-pivot-custom-colors-wrapper .miro-color-picker input').show();
    } else {
      $('.miro-pivot-custom-colors-wrapper .miro-color-picker input').hide();
    }
  });
  $(document).on('click', '.btn-switch-sidebar', function () {
    switchTab(this.dataset.target);
  });
  $(document).on('click', '.bt-highlight-1, .bt-highlight-2, .bt-highlight-3', function () {
    const btn = $(this);
    if (btn.hasClass('dropdown-toggle') || btn.hasClass('bt-no-disable')) {
      return;
    }
    btn.prop('disabled', true);
    setTimeout(() => {
      btn.prop('disabled', false);
    }, 1500);
  }); // hide pivot filter boxes when clicked outside of box
  $(document).on('click', '.change-dd-button', function () {
    const params = this.dataset;
    if (params.isClickable !== 'false') {
      $(params.btnSelector).attr(
        'onclick',
        `Shiny.setInputValue('${params.actionId}','${params.actionVal == null
          ? 1 : params.actionVal}',{priority: 'event'});`,
      );
    }
    $(params.btnSelector).text(params.btnText);
    if (params.isClickable === 'false' || $(params.btnSelector).is(':enabled')) {
      Shiny.setInputValue(params.actionId, params.actionVal == null ? 1 : params.actionVal, {
        priority: 'event',
      });
    }
  });
  $(document).on('click', '.bt-export-canvas', function () {
    const data = document.getElementById(this.dataset.canvasid).toDataURL('image/png');
    this.href = data;
  });
  // miro dashboard value boxes click handler
  $(document).on('click', '.miro-dashboard-valueboxes-wrapper .shiny-html-output', function () {
    let namespaceId = this.id.split('-');
    namespaceId = `${namespaceId[0]}-${namespaceId[1]}`;
    Shiny.setInputValue(`${namespaceId}-showChart`, this.id, { priority: 'event' });
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
  Shiny.addCustomMessageHandler('gms-setAttribs', (data) => {
    for (let i = 0; i < data.selectors.length; i += 1) {
      $(data.selectors[i]).attr(data.attr, data.vals[i]);
    }
  });
  Shiny.addCustomMessageHandler('gms-showLoadingScreen', (delay) => {
    loadingScreen.show(delay);
  });
  Shiny.addCustomMessageHandler('gms-hideLoadingScreen', (e) => { // eslint-disable-line no-unused-vars
    loadingScreen.hide();
  });
  Shiny.addCustomMessageHandler('gms-showEl', (data) => {
    let el;
    if (isInputEl(data.id)) {
      el = $(data.id).closest('.shiny-input-container');
    } else {
      el = $(data.id);
    }
    if (data.inline) {
      el.css('display', 'inline-block');
    } else {
      el.show();
    }
    $(data.id).trigger('shown');
  });
  Shiny.addCustomMessageHandler('gms-setCss', (data) => {
    $(data.id).css(data.css);
  });
  Shiny.addCustomMessageHandler('gms-scriptExecuted', (data) => {
    let scriptOutputContainer;
    if (data.hcube === true) {
      scriptOutputContainer = $('#scriptOutput_hcube');
      scriptOutputContainer.show();
      scriptOutputContainer = scriptOutputContainer.children('.script-output');
    } else if (data.sid == null) {
      scriptOutputContainer = $(`#scriptOutput_${data.id} .script-output`);
      Shiny.setInputValue(
        'outputGenerated',
        1,
        {
          priority: 'event',
        },
      );
      $(`#scriptOutput_${data.id} .script-spinner`).hide();
    } else {
      scriptOutputContainer = $(`#scenScript_${data.sid}_${data.id}`);
    }
    const scriptOutputContainerIframe = scriptOutputContainer[0].contentWindow.document;
    scriptOutputContainerIframe.open();
    scriptOutputContainerIframe.write(data.isError === true
      ? `<div style='margin:5px;color:#F39619;font-weight:bold;\
font-size:15pt;text-align:center;'>${data.data}</div>` : data.data);
    scriptOutputContainerIframe.close();
    scriptOutputContainer.show();
  });
  Shiny.addCustomMessageHandler('gms-showElReplaceTxt', (data) => {
    $(data.id).text(data.txt).show();
  });
  Shiny.addCustomMessageHandler('gms-setContent', (data) => {
    $(data.selector).html(data.content);
  });
  Shiny.addCustomMessageHandler('gms-setTextContent', (data) => {
    if (data.keepChildNodes === true) {
      $(data.selector).contents().filter(function () { return this.nodeType === 3; }).first()
        .replaceWith(new Text(data.content));
    } else {
      $(data.selector).text(data.content);
    }
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
        Shiny.setInputValue(
          'loadTextEntryChunk',
          {
            jID: data.jID,
            chunkCount: counter,
            type: data.type,
          },
          {
            priority: 'event',
          },
        );
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
    $(id).trigger('hidden');
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
    let replaceText = false;
    if (data.text) {
      let crPosition = Infinity;
      let nlPosition = -1;
      let contentToAppend = '';
      for (; ;) {
        if (crPosition - 1 < 0) {
          crPosition = -1;
          content += contentToAppend;
          break;
        }
        crPosition = content.lastIndexOf('\r', crPosition - 1);
        if (crPosition === -1) {
          content += contentToAppend;
          break;
        }
        if (content[crPosition + 1] === '\n' || content[crPosition + 1] === '\r') {
          // \r\n should not cause line to be removed
          continue; // eslint-disable-line no-continue
        }
        contentToAppend = content.substring(crPosition + 1) + contentToAppend;
        nlPosition = content.lastIndexOf('\n', crPosition - 1);
        if (nlPosition === -1) {
          break;
        }
        content = content.substring(0, nlPosition + 1);
      }
      if (crPosition !== -1) {
        const prevContent = $(data.id).text();
        replaceText = true;
        const nlPositionPrev = prevContent.lastIndexOf('\n');
        if (nlPositionPrev !== -1) {
          content = prevContent.substring(0, nlPositionPrev + 1) + contentToAppend;
        } else {
          content = contentToAppend;
        }
      }
      if (replaceText === false) {
        content = document.createTextNode(content);
      }
    }
    if (replaceText) {
      $(data.id).text(content);
    } else {
      $(data.id).append(content);
    }

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
    }, 2000);
  });
  Shiny.addCustomMessageHandler('gms-updateJobProgress', (data) => {
    if (!$(data.id).is(':visible')) {
      return;
    }
    const percentCompleted = Math.round((parseInt(data.progress.noCompleted, 10)
      / parseInt(data.progress.noTotal, 10)) * 100);
    $(data.id)
      .css('width', `${percentCompleted}%`)
      .attr('aria-valuenow', percentCompleted)
      .text(`${data.progress.noCompleted}/${data.progress.noTotal}${data.progress.noFail == null ? '' : ` (${data.progress.noFail})`}`);
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
        checkBoxHTML = `<div class="col-sm-6">
        <div class="form-group shiny-input-container">
          <div class="checkbox">
            <label>
              <input type="checkbox" \
onchange="Shiny.setInputValue('execPermAttachment_${id[i]}',$(this).is(':checked'),\
{priority:'event'});" checked="checked">
              <span>${el.labelCb}</span>
            </label>
          </div>
        </div>
      </div>`;
      }

      $(`<div class="row attachment-line"><div class="col-sm-6"><button class="btn btn-default bt-icon" id="btRemoveAttachment_${id[i]}" type="button" onclick="Miro.removeAttachment(${id[i]})"><i class="fa fa-circle-xmark" role="presentation" aria-label="Remove attachment"></i></button><a href="#" onclick="Miro.downloadAttachment(${id[i]})"> ${name[i]}</a></div>${checkBoxHTML}</div>`).insertBefore('#endAttachList');
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
  Shiny.addCustomMessageHandler('gms-updateSortable', (data) => {
    $(`#${data.id}`).html(data.children);
    Shiny.setInputValue(
      `${data.id}:sortablejs.rank_list`,
      $.map(
        document.getElementById(data.id).children,
        (child) => $(child).attr('data-rank-id') || $.trim(child.innerText),
      ),
    );
  });
  Shiny.addCustomMessageHandler('gms-updateTable', (data) => {
    const tableToUpdate = document.getElementById(data.id);
    const noCols = tableToUpdate.rows[0].cells.length;
    const noNewRows = data.data[0].length;
    const { valCol } = data;
    if (noNewRows === 0) {
      document.getElementById(`${data.id}-wrapper`).style.display = 'none';
      document.getElementById(`${data.id}-noData`).style.display = 'block';
      return;
    }
    document.getElementById(`${data.id}-wrapper`).style.display = '';
    document.getElementById(`${data.id}-noData`).style.display = 'none';

    const $tbody = $(`#${data.id} tbody`);
    const tbody = $tbody[0];
    $tbody.children('tr').remove();

    for (let i = 0; i < noNewRows; i += 1) {
      const tr = document.createElement('tr');
      tr.onclick = function () {
        $(this).toggleClass('selected');
      };
      for (let j = 0; j < noCols; j += 1) {
        const td = document.createElement('td');
        if (j === 0 && valCol != null) {
          td.dataset.val = btoa(data.data[valCol][i]);
        }
        if (i > 0 && data.hierarchical === true
          && Array.isArray(data.hierarchicalCols)
          && data.hierarchicalCols.includes(j)
          && data.data[j][i] === data.data[j][i - 1]) {
          td.appendChild(document.createTextNode(''));
        } else {
          td.appendChild(document.createTextNode(data.data[j][i]));
        }
        tr.appendChild(td);
      }
      tbody.appendChild(tr);
    }
  });
  Shiny.addCustomMessageHandler('gms-populateMiroPivotFilters', (data) => {
    const { ns } = data;
    const idContainerMap = [{ id: 'filter', container: 'filterDropdowns' },
      { id: 'aggregations', container: 'aggregateDropdowns' },
      { id: 'cols', container: 'colDropdowns' }];
    idContainerMap.forEach((filterEl) => {
      Shiny.unbindAll(document.getElementById(ns + filterEl.container));
    });
    idContainerMap.forEach((filterEl) => {
      const dropdownContainer = document.getElementById(ns + filterEl.container);
      const currContent = $(`#${ns + filterEl.container} .shiny-input-container`);
      const newContent = data[filterEl.id];

      if (currContent.length !== newContent.length) {
        $(dropdownContainer).empty();
        Shiny.renderContent(
          dropdownContainer,
          newContent.map((el) => el[0]).join(''),
          'beforeEnd',
        );
        return;
      }
      if (currContent.length === 0) {
        // nothing to do (new content = old content = empty)
        return;
      }
      currContent.each((i, el) => {
        if (newContent[i][2] !== true && newContent[i][1] === $(el).data('hash')) {
          return true;
        }
        $(el).replaceWith(newContent[i][0]);
        return true;
      });
      Shiny.initializeInputs(dropdownContainer);
      Shiny.bindAll(dropdownContainer);
    });
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
    switchTab('input');
  });
  const autoNumericBinding = new Shiny.InputBinding();
  $.extend(autoNumericBinding, {
    find(scope) {
      return $(scope).find('.miro-auto-numeric');
    },
    getValue(el) {
      return $(el).data('autoNumeric').getNumericString();
    },
    setValue(el, value) {
      $(el).data('autoNumeric').set(value);
    },
    receiveMessage(el, data) {
      if (Object.prototype.hasOwnProperty.call(data, 'value')) {
        $(el).data('autoNumeric').set(data.value);
        $(el).trigger('change');
      }
    },
    subscribe(el, callback) {
      $(el).on('change.autoNumericBinding', () => {
        callback(true);
      });
    },
    getRatePolicy() {
      return {
        policy: 'throttle',
        delay: 250,
      };
    },
    initialize(el) {
      $(el).data('autoNumeric', new AutoNumeric(el));
    },
    unsubscribe(el) {
      $(el).data('autoNumeric').remove();
    },
  });
  Shiny.inputBindings.register(autoNumericBinding);
  Shiny.inputBindings.register(colorPickerBinding);
  $('#commandPalette').on('shown.bs.modal', () => {
    $('#cpSearchInput').focus();
  });
  $('#commandPalette').on('hidden.bs.modal', () => {
    shortcutManager.triggerActiveShortcut();
  });
  $('#btShowCommandPalette').on('click', () => {
    shortcutManager.openCommandPalette();
  });
});
