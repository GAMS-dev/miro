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
/*! exports provided: showSpinnerIcon, changeTab, showNewNameBaseDialog, confirmModalShow, removeAttachment, showHypercubeLog, importHypercubeJob, discardHypercubeJob, renderMathJax, validateSname, validateHcubeHash, hcHashImport */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "showSpinnerIcon", function() { return showSpinnerIcon; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "changeTab", function() { return changeTab; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "showNewNameBaseDialog", function() { return showNewNameBaseDialog; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "confirmModalShow", function() { return confirmModalShow; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "removeAttachment", function() { return removeAttachment; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "showHypercubeLog", function() { return showHypercubeLog; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "importHypercubeJob", function() { return importHypercubeJob; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "discardHypercubeJob", function() { return discardHypercubeJob; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "renderMathJax", function() { return renderMathJax; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "validateSname", function() { return validateSname; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "validateHcubeHash", function() { return validateHcubeHash; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "hcHashImport", function() { return hcHashImport; });
/* global $:false Shiny: false HTMLWidgets:false MathJax:false */
var spinnerActive = {};
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
function showHypercubeLog(jID) {
  Shiny.setInputValue('showHypercubeLog', jID, {
    priority: 'event'
  });
}
function importHypercubeJob(jID) {
  Shiny.setInputValue('importHypercubeJob', jID, {
    priority: 'event'
  });
}
function discardHypercubeJob(jID) {
  Shiny.setInputValue('discardHypercubeJob', jID, {
    priority: 'event'
  });
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

  try {
    setTimeout(function () {
      HTMLWidgets.getInstance($('.rhandsontable:visible').get(0)).hot.render();
    }, delay);
  } catch (e) {// continue regardless of error
  }
}

function showHideEl(el, delay) {
  var msg = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : null;

  if (msg !== null) {
    $(el).text(msg);
  }

  $(el).show().delay(delay).fadeOut();
}

function changeActiveButtons(tabId) {
  switch (tabId) {
    case 'inputData':
      $('#btImport').show();
      $('#btSolve').show();
      $('#btInterrupt').hide();
      $('#btSplitView').hide();
      $('#btCompareScen').hide();
      break;

    case 'outputData':
      $('#btImport').hide();
      $('#btSolve').hide();
      $('#btInterrupt').hide();
      $('#btSplitView').hide();
      $('#btCompareScen').hide();
      break;

    case 'gamsinter':
      $('#btImport').hide();
      $('#btSolve').hide();
      $('#btInterrupt').show();
      $('#btSplitView').hide();
      $('#btCompareScen').hide();
      break;

    case 'scenarios':
      $('#btImport').hide();
      $('#btSolve').hide();
      $('#btInterrupt').hide();
      $('#btSplitView').show();
      $('#btCompareScen').show();
      break;

    default:
      $('#btImport').hide();
      $('#btSolve').hide();
      $('#btInterrupt').hide();
      $('#btSplitView').hide();
      $('#btCompareScen').hide();
  }
}

$(document).ready(function () {
  $('body').addClass('fixed'); // besides these updates, gms-switchTab (see below) has always has to be considered as well

  $('#btImport').show();
  $('#btSolve').show();
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
    switch (el) {
      case 'input':
        changeActiveButtons('inputData');
        break;

      case 'output':
        changeActiveButtons('outputData');
        break;

      case 'gamsinter':
        changeActiveButtons('gamsinter');
        break;

      case 'hcubeAna':
        changeActiveButtons('default');
        break;

      case 'scenComp':
        changeActiveButtons('scenarios');
        break;

      default:
        break;
    }
  });
  $('body').on('click', '.bt-highlight-1, .bt-highlight-2, .bt-highlight-3', function () {
    var btn = $(this);
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
  Shiny.addCustomMessageHandler('gms-addClassEl', function (el) {
    $(el.id).addClass(el.newclass);
  });
  Shiny.addCustomMessageHandler('gms-removeClassEl', function (el) {
    $(el.id).removeClass(el.oldclass);
  });
  Shiny.addCustomMessageHandler('gms-scrollDown', function (id) {
    setTimeout(function () {
      $(id).animate({
        scrollTop: $(id)[0].scrollHeight - $(id)[0].clientHeight
      }, 300);
    }, 500);
  });
  Shiny.addCustomMessageHandler('gms-hideModal', function (delay) {
    setTimeout(function () {
      $('#shiny-modal-wrapper').find('.modal').modal('hide');
    }, delay * 1000);
  });
  Shiny.addCustomMessageHandler('gms-updateAttachList', function (el) {
    var _$$makeArray = $.makeArray(el.id),
        id = _$$makeArray.id;

    var _$$makeArray2 = $.makeArray(el.name),
        name = _$$makeArray2.name;

    for (var i = 0; i < id.length; i += 1) {
      var checkBoxHTML = '';

      if (el.allowExec) {
        checkBoxHTML = "<div class=\"col-sm-6\"><div class=\"form-group shiny-input-container\"><div class=\"checkbox\"><label><input type=\"checkbox\" onchange=\"Shiny.setInputValue('execPermAttachment_".concat(id[i], "', $(this).is(':checked'));\" checked=\"checked\"><span>").concat(el.labelCb, "</span></label></div></div></div>");
      }

      $("<div class=\"row attachment-line\"><div class=\"col-sm-6\"><button class=\"btn btn-default bt-icon\" id=\"btRemoveAttachment_".concat(id[i], "\" type=\"button\" onclick=\"Miro.removeAttachment(").concat(id[i], ")\"><i class=\"fa fa-times-circle\"></i></button> ").concat(name[i], "</div>").concat(checkBoxHTML, "</div>")).insertBefore('#endAttachList');
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