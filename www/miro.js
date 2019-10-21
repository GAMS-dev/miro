var Miro =
/******/ (function(modules) { // webpackBootstrap
/******/ 	// The module cache
/******/ 	var installedModules = {};
/******/
/******/ 	// The require function
/******/ 	function __webpack_require__(moduleId) {
/******/
/******/ 		// Check if module is in cache
/******/ 		if(installedModules[moduleId]) {
/******/ 			return installedModules[moduleId].exports;
/******/ 		}
/******/ 		// Create a new module (and put it into the cache)
/******/ 		var module = installedModules[moduleId] = {
/******/ 			i: moduleId,
/******/ 			l: false,
/******/ 			exports: {}
/******/ 		};
/******/
/******/ 		// Execute the module function
/******/ 		modules[moduleId].call(module.exports, module, module.exports, __webpack_require__);
/******/
/******/ 		// Flag the module as loaded
/******/ 		module.l = true;
/******/
/******/ 		// Return the exports of the module
/******/ 		return module.exports;
/******/ 	}
/******/
/******/
/******/ 	// expose the modules object (__webpack_modules__)
/******/ 	__webpack_require__.m = modules;
/******/
/******/ 	// expose the module cache
/******/ 	__webpack_require__.c = installedModules;
/******/
/******/ 	// define getter function for harmony exports
/******/ 	__webpack_require__.d = function(exports, name, getter) {
/******/ 		if(!__webpack_require__.o(exports, name)) {
/******/ 			Object.defineProperty(exports, name, { enumerable: true, get: getter });
/******/ 		}
/******/ 	};
/******/
/******/ 	// define __esModule on exports
/******/ 	__webpack_require__.r = function(exports) {
/******/ 		if(typeof Symbol !== 'undefined' && Symbol.toStringTag) {
/******/ 			Object.defineProperty(exports, Symbol.toStringTag, { value: 'Module' });
/******/ 		}
/******/ 		Object.defineProperty(exports, '__esModule', { value: true });
/******/ 	};
/******/
/******/ 	// create a fake namespace object
/******/ 	// mode & 1: value is a module id, require it
/******/ 	// mode & 2: merge all properties of value into the ns
/******/ 	// mode & 4: return value when already ns object
/******/ 	// mode & 8|1: behave like require
/******/ 	__webpack_require__.t = function(value, mode) {
/******/ 		if(mode & 1) value = __webpack_require__(value);
/******/ 		if(mode & 8) return value;
/******/ 		if((mode & 4) && typeof value === 'object' && value && value.__esModule) return value;
/******/ 		var ns = Object.create(null);
/******/ 		__webpack_require__.r(ns);
/******/ 		Object.defineProperty(ns, 'default', { enumerable: true, value: value });
/******/ 		if(mode & 2 && typeof value != 'string') for(var key in value) __webpack_require__.d(ns, key, function(key) { return value[key]; }.bind(null, key));
/******/ 		return ns;
/******/ 	};
/******/
/******/ 	// getDefaultExport function for compatibility with non-harmony modules
/******/ 	__webpack_require__.n = function(module) {
/******/ 		var getter = module && module.__esModule ?
/******/ 			function getDefault() { return module['default']; } :
/******/ 			function getModuleExports() { return module; };
/******/ 		__webpack_require__.d(getter, 'a', getter);
/******/ 		return getter;
/******/ 	};
/******/
/******/ 	// Object.prototype.hasOwnProperty.call
/******/ 	__webpack_require__.o = function(object, property) { return Object.prototype.hasOwnProperty.call(object, property); };
/******/
/******/ 	// __webpack_public_path__
/******/ 	__webpack_require__.p = "";
/******/
/******/
/******/ 	// Load entry module and return exports
/******/ 	return __webpack_require__(__webpack_require__.s = "./srcjs/miro.js");
/******/ })
/************************************************************************/
/******/ ({

/***/ "./srcjs/miro.js":
/*!***********************!*\
  !*** ./srcjs/miro.js ***!
  \***********************/
/*! exports provided: showSpinnerIcon, changeTab, slideToggleEl, showNewNameBaseDialog, confirmModalShow, removeAttachment, downloadAttachment, changeDDButtonEvent, showJobsDialog, renderMathJax, validateSname, validateHcubeHash, hcHashImport, jumpToLogMark */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "showSpinnerIcon", function() { return showSpinnerIcon; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "changeTab", function() { return changeTab; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "slideToggleEl", function() { return slideToggleEl; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "showNewNameBaseDialog", function() { return showNewNameBaseDialog; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "confirmModalShow", function() { return confirmModalShow; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "removeAttachment", function() { return removeAttachment; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "downloadAttachment", function() { return downloadAttachment; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "changeDDButtonEvent", function() { return changeDDButtonEvent; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "showJobsDialog", function() { return showJobsDialog; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "renderMathJax", function() { return renderMathJax; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "validateSname", function() { return validateSname; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "validateHcubeHash", function() { return validateHcubeHash; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "hcHashImport", function() { return hcHashImport; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "jumpToLogMark", function() { return jumpToLogMark; });
function asyncGeneratorStep(gen, resolve, reject, _next, _throw, key, arg) { try { var info = gen[key](arg); var value = info.value; } catch (error) { reject(error); return; } if (info.done) { resolve(value); } else { Promise.resolve(value).then(_next, _throw); } }

function _asyncToGenerator(fn) { return function () { var self = this, args = arguments; return new Promise(function (resolve, reject) { var gen = fn.apply(self, args); function _next(value) { asyncGeneratorStep(gen, resolve, reject, _next, _throw, "next", value); } function _throw(err) { asyncGeneratorStep(gen, resolve, reject, _next, _throw, "throw", err); } _next(undefined); }); }; }

/* global $:false Shiny: false HTMLWidgets:false MathJax:false Selectize:false */
var spinnerActive = {};

function sleep(ms) {
  return new Promise(function (resolve) {
    return setTimeout(resolve, ms);
  });
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

function showSpinnerIcon(el) {
  var delay = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 3000;

  if (spinnerActive[$(el).prop('id')]) {
    return;
  }

  spinnerActive[$(el).prop('id')] = true;
  var content = $(el).html();
  $(el).html('<i class="fa fa-refresh fa-spin"></i>');
  setTimeout(function () {
    $(el).html(content);
    spinnerActive[$(el).prop('id')] = false;
  }, delay);
}
function changeTab(object, idActive, idRefer) {
  var tabPane = object.closest('.tabbable');
  tabPane.find("li:nth-of-type(".concat(idActive, ")")).removeClass();
  tabPane.find("li:nth-of-type(".concat(idRefer, ")")).addClass('active');
  tabPane.find(".tab-content div:nth-child(".concat(idActive, ")")).removeClass('active');
  tabPane.find(".tab-content div:nth-child(".concat(idRefer, ")")).addClass('active');
}

function switchTabInTabset(tabsetID, tabValue) {
  var tabset = $("#".concat(tabsetID));
  tabset.find("a[data-value='".concat(tabValue, "']")).tab('show');
}

function removeModal() {
  $('#shiny-modal-wrapper').find('.modal').modal('hide');
}

function slideToggleEl(data) {
  if (data.toggleIconDiv !== undefined) {
    if ($(data.id).is(':visible')) {
      $(data.toggleIconDiv).html('<i class="fa fa-plus"></i>');
    } else {
      $(data.toggleIconDiv).html('<i class="fa fa-minus"></i>');
    }
  }

  var duration = 400;

  if (data.duration === undefined) {
    duration = data.duration;
  }

  $(data.id).slideToggle(duration);
}
function showNewNameBaseDialog() {
  $('#base-overwrite-container').hide();
  $('#loadBase_snameExistsMsg').hide();
  $('#loadBase_newName').show();
  $('#btCheckSnameBase').show();
}
function confirmModalShow(title, desc, cancelTxt) {
  var confirmTxt = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : null;
  var confirmCall = arguments.length > 4 && arguments[4] !== undefined ? arguments[4] : null;
  var btDataDismiss = "<button type=\"button\" class=\"btn btn-default\" data-dismiss=\"modal\">".concat(cancelTxt, "</button>");
  var btDataConfirm = '';

  if (confirmCall !== null) {
    btDataConfirm = '<button type="button" class="btn btn-default bt-highlight-1 bt-gms-confirm" ' + "id=\"\" onclick=\"".concat(confirmCall, "\" data-dismiss=\"modal\">").concat(confirmTxt, "</button>");
  }

  var cModal = $('#confirmModal');
  cModal.find('.modal-title').html(title);
  cModal.find('.modal-body').html(desc);
  cModal.find('.modal-footer').html(btDataDismiss + btDataConfirm);
  cModal.modal('show');
}
function removeAttachment(elId) {
  $("#btRemoveAttachment_".concat(elId)).parent().parent().remove();
  Shiny.setInputValue("btRemoveAttachment_".concat(elId), 1, {
    priority: 'event'
  });
}
function downloadAttachment(elId) {
  Shiny.setInputValue('downloadAttachment', elId, {
    priority: 'event'
  });
  setTimeout(function () {
    $('#downloadAttachmentData')[0].click();
  }, 200);
}
function changeDDButtonEvent(elText, DDBtnID, actionID) {
  $(DDBtnID).attr('onclick', "Shiny.setInputValue('".concat(actionID, "',1,{priority: 'event'});"));
  $(DDBtnID).text(elText);

  if ($(DDBtnID).is(':enabled')) {
    Shiny.setInputValue(actionID, 1, {
      priority: 'event'
    });
  }
}
function showJobsDialog(hcubeMode) {
  removeModal();

  if (hcubeMode) {
    switchTab('importData');
    return;
  }

  switchTab('gamsinter');
  switchTabInTabset('jobListPanel', 'joblist');
}
function renderMathJax() {
  MathJax.Hub.Queue(['Typeset', MathJax.Hub, 'wrapper-documentation']);
}
function validateSname(el) {
  var inputID = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 'btCheckSnameLocalConfirm';

  if (/^[a-f0-9]{64}$/i.test($(el).val()) !== true && /^\s*$/.test($(el).val()) !== true) {
    $(el).removeClass('invalidInput');

    if (inputID === 'internal') {
      return true;
    }

    Shiny.setInputValue(inputID, 1, {
      priority: 'event'
    });
    return true;
  }

  $(el).addClass('invalidInput');
  return false;
}
function validateHcubeHash() {
  var hashVal = $('#hcHashLookup').val();

  if (/^[a-f0-9]{64}$/i.test(hashVal) === true) {
    if (!validateSname('#hcube_newScenName', 'internal')) {
      return;
    }

    $('#hcHashLookup').removeClass('invalidInput');
    Shiny.setInputValue('hcHashLookup', hashVal, {
      priority: 'event'
    });
    return;
  }

  $('#hcHashLookup').addClass('invalidInput');
}
function hcHashImport(sid) {
  if (!validateSname('#hcube_newScenName', 'internal')) {
    return;
  }

  Shiny.setInputValue('loadHcubeHashSid', sid, {
    priority: 'event'
  });
}
function jumpToLogMark(_x) {
  return _jumpToLogMark.apply(this, arguments);
}

function _jumpToLogMark() {
  _jumpToLogMark = _asyncToGenerator(
  /*#__PURE__*/
  regeneratorRuntime.mark(function _callee(id) {
    var el;
    return regeneratorRuntime.wrap(function _callee$(_context) {
      while (1) {
        switch (_context.prev = _context.next) {
          case 0:
            switchTab('gamsinter');
            $('#logFileTabsset [data-value="mirolog"]').tab('show');
            _context.next = 4;
            return sleep(200);

          case 4:
            el = $("#mlogMark_".concat(id));

            if (el !== undefined) {
              el[0].scrollIntoView();
              el.animate({
                backgroundColor: 'yellow'
              }, 400).delay(1000).animate({
                backgroundColor: 'transparent'
              }, 400);
            }

          case 6:
          case "end":
            return _context.stop();
        }
      }
    }, _callee);
  }));
  return _jumpToLogMark.apply(this, arguments);
}

function isInputEl(id) {
  if ($(id).parents('.form-group').length) {
    return true;
  }

  return false;
}

function rerenderDygraph() {
  var delay = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 100;

  try {
    setTimeout(function () {
      HTMLWidgets.getInstance($('.dygraphs:visible').get(0)).dygraph.resize();
    }, delay);
  } catch (e) {// continue regardless of error
  }
}

function rerenderHot() {
  var delay = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 100;
  setTimeout(function () {
    var el = $('.rhandsontable:visible').get(0);

    if (el !== undefined) {
      HTMLWidgets.getInstance(el).hot.render();
    }
  }, delay);
}

function showHideEl(el, delay) {
  var msg = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : null;

  if (msg !== null) {
    $(el).text(msg);
  }

  $(el).show().delay(delay).fadeOut();
}

function scrollDown(id) {
  var delay = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 500;
  setTimeout(function () {
    $(id).animate({
      scrollTop: $(id)[0].scrollHeight - $(id)[0].clientHeight
    }, 300);
  }, delay);
}

$(document).ready(function () {
  $('.dropdown').on('show.bs.dropdown', function (e) {
    var ddButton = $(e.target);
    var dropdown = ddButton.children('.dropdown-menu').first();

    if (dropdown.width() <= ddButton.width() + ddButton.offset().left - ddButton.parent().offset().left) {
      dropdown.addClass('dropdown-menu-right');
    } else {
      dropdown.removeClass('dropdown-menu-right');
    }
  }); // code snippet taken from SwishWez: https://stackoverflow.com/questions/21582558/disable-remove-on-backspace-or-remove-ibeam-entirely

  Selectize.define('no_delete', function () {
    var self = this;

    this.deleteSelection = function () {
      var original = self.deleteSelection;
      return function (e) {
        if (!e || e.keyCode !== 8 && e.keyCode !== 46) {
          for (var _len = arguments.length, args = new Array(_len > 1 ? _len - 1 : 0), _key = 1; _key < _len; _key++) {
            args[_key - 1] = arguments[_key];
          }

          return original.apply(this, args);
        }

        return false;
      };
    }();
  });
  $('body').addClass('fixed'); // besides these updates, gms-switchTab (see below) has always has to be considered as well

  $('#btImport').show();
  $('.btSolve').show();
  $('#btInterrupt').hide();
  $('#btSplitView').hide();
  $('#btCompareScen').hide();
  $('#btLoadScen').hide();
  $('a[data-value="inputData"]').click(function () {
    changeActiveButtons('inputData');
    rerenderHot();
  });
  $('a[data-value="outputData"]').click(function () {
    changeActiveButtons('outputData');
  });
  $('a[data-value="gamsinter"]').click(function () {
    changeActiveButtons('gamsinter');
  });
  $('a[data-value="scenarios"]').click(function () {
    changeActiveButtons('scenarios');
  });
  $('#scenTabset').on('click', 'a[data-toggle="tab"]', function () {
    rerenderDygraph();
  });
  $('a[data-value="advanced"],a[data-value="importData"],a[data-value="loadResults"],a[data-value="hcubeAnalyze"]').click(function () {
    changeActiveButtons('default');
  });
  $('#inputTabset li').click(function () {
    rerenderHot();
  });
  $('#scenTabset').append('<li id="scenTabsetAdd"><a href="#" id="btLoadScen" data-value="scen_add" ' + 'onclick="Shiny.setInputValue(\'btLoadScen\', 1, {priority: \'event\'});">' + '<i class="far fa-plus-square" style="font-size:13pt;"></i></a></li>'); // show/hide buttons after (R triggered) tab switch.

  Shiny.addCustomMessageHandler('gms-switchTab', function (el) {
    switchTab(el);
  });
  $('body').on('click', '.bt-highlight-1, .bt-highlight-2, .bt-highlight-3', function () {
    var btn = $(this);

    if (btn.hasClass('dropdown-toggle')) {
      return;
    }

    btn.prop('disabled', true);
    setTimeout(function () {
      btn.prop('disabled', false);
    }, 1500);
  }); // hide pivot filter boxes when clicked outside of box

  $(document).click(function (e) {
    var target = e.target;

    if (!$(target).is('.pvtAttr') && !$(target).parents('.pvtAttr').length && !$(target).is('.pvtFilterBox') && !$(target).parents('.pvtFilterBox').length) {
      $('.pvtFilterBox').hide();
    }
  });
  $('.sidebar-toggle').click(function () {
    rerenderHot(400);
  });
  window.addEventListener('beforeunload', function (e) {
    if ($('#shiny-disconnected-overlay').length === 0) {
      e.preventDefault();
      e.returnValue = 'Are you sure you want to leave? Unsaved changes will be lost!';
    }
  });
  Shiny.addCustomMessageHandler('gms-showEl', function (id) {
    if (isInputEl(id)) {
      $(id).closest('.shiny-input-container').show();
    } else {
      $(id).show();
    }
  });
  Shiny.addCustomMessageHandler('gms-showElReplaceTxt', function (data) {
    $(data.id).text(data.txt).show();
  });
  Shiny.addCustomMessageHandler('gms-showLogContent', function (data) {
    $(data.id).text(data.content);

    if (data.chunkCount <= 1) {
      return;
    }

    var noChunks = data.noChunks;

    if (noChunks === undefined || noChunks <= 1) {
      return;
    }

    var counter = 1;
    var isLoading = false;
    $(data.id).on('scroll', function () {
      if ($(data.id)[0].scrollHeight - $(data.id).scrollTop() < $(data.id).outerHeight() + 200 && isLoading === false) {
        isLoading = true;
        Shiny.setInputValue('loadTextEntityChunk', {
          jID: data.jID,
          chunkCount: counter,
          type: data.type
        }, {
          priority: 'event'
        });
        counter += 1;

        if (counter === noChunks) {
          $(data.id).off('scroll');
        }
      }
    });
    $(data.id).on('change', function () {
      setTimeout(function () {
        isLoading = false;
      }, 1000);
    });
  });
  Shiny.addCustomMessageHandler('gms-hideEl', function (id) {
    if (isInputEl(id)) {
      $(id).closest('.shiny-input-container').hide();
    } else {
      $(id).hide();
    }
  });
  Shiny.addCustomMessageHandler('gms-showHideEl', function (data) {
    showHideEl(data.id, data.delay, data.msg);
  });
  Shiny.addCustomMessageHandler('gms-enableEl', function (id) {
    $(id).prop('disabled', false);
  });
  Shiny.addCustomMessageHandler('gms-disableEl', function (id) {
    $(id).prop('disabled', true);
  });
  Shiny.addCustomMessageHandler('gms-toggleEl', function (id) {
    if ($(id).is(':visible')) {
      $(id).toggle();
      $(id).trigger('shown');
    } else {
      $(id).fadeToggle(600);
      $(id).trigger('hidden');
    }
  });
  Shiny.addCustomMessageHandler('gms-slideToggleEl', function (data) {
    slideToggleEl(data);
  });
  Shiny.addCustomMessageHandler('gms-addClassEl', function (el) {
    $(el.id).addClass(el.newclass);
  });
  Shiny.addCustomMessageHandler('gms-removeClassEl', function (el) {
    $(el.id).removeClass(el.oldclass);
  });
  Shiny.addCustomMessageHandler('gms-emptyEl', function (id) {
    $(id).empty();
  });
  Shiny.addCustomMessageHandler('gms-appendEl', function (data) {
    var content = data.content;

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
  Shiny.addCustomMessageHandler('gms-scrollDown', function (id) {
    scrollDown(id);
  });
  Shiny.addCustomMessageHandler('gms-startUpdateJobProgress', function (data) {
    var interval = setInterval(function () {
      if ($(data.id).attr('aria-valuenow') === '100' || !$(data.id).is(':visible')) {
        clearInterval(interval);
      }

      Shiny.setInputValue('updateJobProgress', data.jID, {
        priority: 'event'
      });
    }, 5000);
  });
  Shiny.addCustomMessageHandler('gms-updateJobProgress', function (data) {
    if (!$(data.id).is(':visible')) {
      return;
    }

    var percentCompleted = Math.round(parseInt(data.progress.noCompleted, 10) / parseInt(data.progress.noTotal, 10) * 100);
    $(data.id).css('width', "".concat(percentCompleted, "%")).attr('aria-valuenow', percentCompleted).text("".concat(data.progress.noCompleted, "/").concat(data.progress.noTotal));
  });
  Shiny.addCustomMessageHandler('gms-markJobDownloadComplete', function (data) {
    if (data.triggerImport === true && $("#jobImportDlProgressWrapper_".concat(data.id)).is(':visible')) {
      Shiny.setInputValue('importJob', data.id, {
        priority: 'event'
      });
    }

    $("#jobImportDlProgressWrapper_".concat(data.id)).siblings('div:first').text(data.text);
    $("#jobImportDlProgressWrapper_".concat(data.id)).hide();
    $("#btDownloadJob_".concat(data.id)).hide();
    $("#btImportJob_".concat(data.id)).show();
  });
  Shiny.addCustomMessageHandler('gms-hideModal', function (delay) {
    setTimeout(function () {
      $('#shiny-modal-wrapper').find('.modal').modal('hide');
    }, delay * 1000);
  });
  Shiny.addCustomMessageHandler('gms-updateAttachList', function (el) {
    var id = $.makeArray(el.id);
    var name = $.makeArray(el.name);

    for (var i = 0; i < id.length; i += 1) {
      var checkBoxHTML = '';

      if (el.allowExec) {
        checkBoxHTML = "<div class=\"col-sm-6\"><div class=\"form-group shiny-input-container\"><div class=\"checkbox\"><label><input type=\"checkbox\" onchange=\"Shiny.setInputValue('execPermAttachment_".concat(id[i], "', $(this).is(':checked'));\" checked=\"checked\"><span>").concat(el.labelCb, "</span></label></div></div></div>");
      }

      $("<div class=\"row attachment-line\"><div class=\"col-sm-6\"><button class=\"btn btn-default bt-icon\" id=\"btRemoveAttachment_".concat(id[i], "\" type=\"button\" onclick=\"Miro.removeAttachment(").concat(id[i], ")\"><i class=\"fa fa-times-circle\"></i></button><a href=\"#\" onclick=\"Miro.downloadAttachment(").concat(id[i], ")\"> ").concat(name[i], "</a></div>").concat(checkBoxHTML, "</div>")).insertBefore('#endAttachList');
    }
  });
  Shiny.addCustomMessageHandler('gms-fitTitleInBox', function (id) {
    setTimeout(function () {
      var el = $(id);
      var parentEl = el.parent()[0];
      el.css('font-size', '18px');
      var currSize = 18;

      while (parentEl.scrollWidth > parentEl.clientWidth && currSize >= 10) {
        currSize -= 2;
        el.css('font-size', "".concat(currSize, "px"));
      }
    }, 500);
  });
  Shiny.addCustomMessageHandler('gms-showValidationErrors', function (content) {
    var inSyms = Object.keys(content);
    $('.input-validation-error').empty();
    inSyms.forEach(function (key) {
      if (Array.isArray(content[key])) {
        content[key].forEach(function (item) {
          $("#valErr_".concat(key)).append(item);
        });
      } else {
        $("#valErr_".concat(key)).append(content[key]);
      }
    });
    $('.input-validation-error').show();
  });
  var autoNumericBinding = new Shiny.InputBinding();
  $.extend(autoNumericBinding, {
    find: function find(scope) {
      return $(scope).find('.miro-auto-numeric');
    },
    getValue: function getValue(el) {
      return parseFloat($(el).autoNumeric('get'));
    },
    setValue: function setValue(el, value) {
      $(el).autoNumeric('set', value);
    },
    receiveMessage: function receiveMessage(el, data) {
      if (Object.prototype.hasOwnProperty.call(data, 'value')) {
        $(el).autoNumeric('set', data.value);
      }
    },
    subscribe: function subscribe(el, callback) {
      $(el).on('change.autoNumericBinding', function () {
        callback(true);
      });
    },
    getRatePolicy: function getRatePolicy() {
      return {
        policy: 'debounce',
        delay: 250
      };
    },
    initialize: function initialize(el) {
      $(el).autoNumeric('init');
    },
    unsubscribe: function unsubscribe(el) {
      $(el).autoNumeric('destroy');
    }
  });
  Shiny.inputBindings.register(autoNumericBinding);
}); // counter

var count = 1; // maximum number of scenarios that can be loaded in compare view

var maxNumScen = 50;
$(document).keyup(function (event) {
  if (event.keyCode === 13 && !event.ctrlKey) {
    if ($('#shiny-modal').find('.selectize-input.input-active').length > 0 || $('#shiny-modal').find('*[data-dismiss="modal"]').is(':focus')) {
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
      priority: 'event'
    });
    return;
  } // SAVE shortcut: CTRL + ALT + S


  if (event.keyCode === 13) {
    $('#btSolve:visible:enabled').click();
    return;
  } // Solve shortcut: CTRL + ALT + ENTER


  if (event.keyCode === 82) {
    Shiny.setInputValue('btDelete', 1, {
      priority: 'event'
    });
    return;
  } // Remove shortcut: CTRL + ALT + R


  if (event.keyCode === 67) {
    $('.btRemove:visible').click();
    return;
  } // Close shortcut (remove button in input sheet): CTRL + ALT + C


  if (event.keyCode === 67) {
    for (var i = 2; i <= maxNumScen; i += 1) {
      $("#close_".concat(i, ":visible")).click();
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
    var tab = $('a[href="#shiny-tab-outputData"]');

    if (tab.length > 0) {
      tab.click();
    } else {
      $('a[href="#shiny-tab-importData"]').click();
    }

    return;
  } // Select output menu shortcut: CTRL + ALT + 2


  if (event.keyCode === 51) {
    var _tab = $('a[href="#shiny-tab-gamsinter"]');

    if (_tab.length > 0) {
      _tab.click();
    } else {
      $('a[href="#shiny-tab-loadResults"]').click();
    }

    return;
  } // Select gams interaction menu shortcut: CTRL + ALT + 3


  if (event.keyCode === 52) {
    $('a[href="#shiny-tab-scenarios"]').click();
    return;
  } // Select scenario menu shortcut: CTRL + ALT + 4


  if (event.keyCode === 53) {
    var _tab2 = $('a[href="#shiny-tab-hcubeAnalyze"]');

    if (_tab2.length > 0) {
      _tab2.click();
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

    for (var _i = 2; _i <= maxNumScen + 3; _i += 1) {
      $("#table_".concat(_i, ":visible")).click();
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
  } // Activate/deactivate scenario comparison mode: CTRL + ALT + space

});

/***/ })

/******/ });
//# sourceMappingURL=miro.js.map