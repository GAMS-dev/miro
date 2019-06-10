import 'jquery-slimscroll';

function changeTab(object, idActive, idRefer) {
  var tabPane = object.closest(".tabbable");
  tabPane.find("li:nth-of-type(" + idActive + ")").removeClass();
  tabPane.find("li:nth-of-type(" + idRefer + ")").addClass("active");
  tabPane.find(".tab-content div:nth-child(" + idActive + ")").removeClass("active");
  tabPane.find(".tab-content div:nth-child(" + idRefer + ")").addClass("active");
}
function showNewNameBaseDialog(){
  $("#base-overwrite-container").hide();
  $("#loadBase_snameExistsMsg").hide();
  $("#loadBase_newName").show();
  $("#btCheckSnameBase").show();
}
function isInputEl(id) {
  if ($(id).parents(".form-group").length) {
    return true;
  } else {
    return false;
  }
}
function rerenderDygraph(){
  var delay = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 100;

  try {
    setTimeout(function () {
      HTMLWidgets.getInstance($(".dygraphs:visible").get(0)).dygraph.resize();
    }, delay);
  } catch (e) {}
}
function rerenderHot() {
  var delay = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 100;

  try {
    setTimeout(function () {
      HTMLWidgets.getInstance($(".rhandsontable:visible").get(0)).hot.render();
    }, delay);
  } catch (e) {}
}

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

function changeActiveButtons(tabId) {
  switch (tabId) {
    case 'inputData':
      $("#btImport").show();
      $("#btSolve").show();
      $("#btInterrupt").hide();
      $("#btSplitView").hide();
      $("#btCompareScen").hide();
      break;

    case 'outputData':
      $("#btImport").hide();
      $("#btSolve").hide();
      $("#btInterrupt").hide();
      $("#btSplitView").hide();
      $("#btCompareScen").hide();
      break;

    case 'gamsinter':
      $("#btImport").hide();
      $("#btSolve").hide();
      $("#btInterrupt").show();
      $("#btSplitView").hide();
      $("#btCompareScen").hide();
      break;

    case 'scenarios':
      $("#btImport").hide();
      $("#btSolve").hide();
      $("#btInterrupt").hide();
      $("#btSplitView").show();
      $("#btCompareScen").show();
      break;

    default:
      $("#btImport").hide();
      $("#btSolve").hide();
      $("#btInterrupt").hide();
      $("#btSplitView").hide();
      $("#btCompareScen").hide();
  }
}

function confirmModalShow(title, desc, cancelTxt) {
  var confirmTxt = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : null;
  var confirmCall = arguments.length > 4 && arguments[4] !== undefined ? arguments[4] : null;
  var btDataDismiss = '<button type="button" class="btn btn-default" data-dismiss="modal">' + cancelTxt + '</button>';
  var btDataConfirm = '';

  if (confirmCall !== null) {
    btDataConfirm = '<button type="button" class="btn btn-default bt-highlight-1 bt-gms-confirm" id="" onclick="' + confirmCall + '" data-dismiss="modal">' + confirmTxt + '</button>';
  }

  btData = btDataDismiss + btDataConfirm;
  cModal = $('#confirmModal');
  cModal.find('.modal-title').html(title);
  cModal.find('.modal-body').html(desc);
  cModal.find('.modal-footer').html(btData);
  cModal.modal('show');
}

function removeAttachment(elId) {
  $('#btRemoveAttachment_' + elId).parent().parent().remove();
  Shiny.setInputValue("btRemoveAttachment_" + elId, 1, {
    priority: "event"
  });
}

function showHypercubeLog(jID) {
  Shiny.setInputValue("showHypercubeLog", jID, {
    priority: "event"
  });
}

function importHypercubeJob(jID) {
  Shiny.setInputValue("importHypercubeJob", jID, {
    priority: "event"
  });
}

function discardHypercubeJob(jID) {
  Shiny.setInputValue("discardHypercubeJob", jID, {
    priority: "event"
  });
}

function renderMathJax() {
  MathJax.Hub.Queue(["Typeset", MathJax.Hub, "wrapper-documentation"]);
}

function validateSname(el) {
  var inputID = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : "btCheckSnameLocalConfirm";
  if (/^[a-f0-9]{64}$/i.test($(el).val()) !== true && /^\s*$/.test($(el).val()) !== true) {
    $(el).removeClass('invalidInput');
    if(inputID === "internal"){
      return true;
    }else{
      Shiny.setInputValue(inputID, 1, {
        priority: "event"
      });
      return;
    }
  } else {
    $(el).addClass('invalidInput');
    return false;
  }
}

function validateHcubeHash() {
  var hashVal = $('#hcHashLookup').val();

  if (/^[a-f0-9]{64}$/i.test(hashVal) === true) {
    if(!validateSname("#hcube_newScenName", "internal")){
      return;
    }
    $('#hcHashLookup').removeClass('invalidInput');
    Shiny.setInputValue("hcHashLookup", hashVal, {
      priority: "event"
    });
    return;
  }

  $('#hcHashLookup').addClass('invalidInput');
}

function hcHashImport(sid) {
  if(!validateSname("#hcube_newScenName", "internal")){
    return;
  }
  Shiny.setInputValue("loadHcubeHashSid", sid, {
    priority: "event"
  });
}

function showHideEl(el, delay) {
  $(el).show().delay(delay).fadeOut();
}

$(document).ready(function () {
  $("body").addClass("fixed"); // besides these updates, gms-switchTab (see below) has always has to be considered as well

  $("#btImport").show();
  $("#btSolve").show();
  $("#btInterrupt").hide();
  $("#btSplitView").hide();
  $("#btCompareScen").hide();
  $("#btLoadScen").hide();
  $("a[data-value='inputData']").click(function () {
    changeActiveButtons('inputData');
    rerenderHot();
  });
  $("a[data-value='outputData']").click(function () {
    changeActiveButtons('outputData');
  });
  $("a[data-value='gamsinter']").click(function () {
    changeActiveButtons('gamsinter');
  });
  $("a[data-value='scenarios']").click(function () {
    changeActiveButtons('scenarios');
  });
  $("#scenTabset").on("click", "a[data-toggle='tab']", function () {
    rerenderDygraph();
  });
  $("a[data-value='advanced'],a[data-value='importData'],a[data-value='loadResults'],a[data-value='hcubeAnalyze']").click(function () {
    changeActiveButtons('default');
  });
  $("#inputTabset li").click(function () {
    rerenderHot();
  });
  $("#scenTabset").append("<li id=\"scenTabsetAdd\"><a href=\"#\" id=\"btLoadScen\" data-value=\"scen_add\" " + "onclick=\"Shiny.setInputValue('btLoadScen', 1, {priority: 'event\'});\">" + "<i class=\"far fa-plus-square\" style=\"font-size:13pt;\"></i></a></li>"); // show/hide buttons after (R triggered) tab switch.

  Shiny.addCustomMessageHandler('gms-switchTab', function (el) {
    switch (el) {
      case "input":
        changeActiveButtons('inputData');
        break;

      case "output":
        changeActiveButtons('outputData');
        break;

      case "gamsinter":
        changeActiveButtons('gamsinter');
        break;

      case "hcubeAna":
        changeActiveButtons('default');
        break;

      case "scenComp":
        changeActiveButtons('scenarios');
        break;
    }
  });
  $("body").on("click", ".bt-highlight-1, .bt-highlight-2, .bt-highlight-3", function () {
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
  $(".sidebar-toggle").click(function (e) {
    rerenderHot(400);
  });
  $(window).on('beforeunload', function () {
    if ($("#shiny-disconnected-overlay").length === 0) {
      return "Are you sure you want to leave? Unsaved changes will be lost!";
    }
  });
  Shiny.addCustomMessageHandler('gms-showEl', function (id) {
    if (isInputEl(id)) {
      $(id).closest(".shiny-input-container").show();
    } else {
      $(id).show();
    }
  });
  Shiny.addCustomMessageHandler('gms-showElReplaceTxt', function (data) {
    $(data.id).text(data.txt).show();
  });
  Shiny.addCustomMessageHandler('gms-hideEl', function (id) {
    if (isInputEl(id)) {
      $(id).closest(".shiny-input-container").hide();
    } else {
      $(id).hide();
    }
  });
  Shiny.addCustomMessageHandler('gms-showHideEl', function (data) {
    showHideEl(data.id, data.delay);
  });
  Shiny.addCustomMessageHandler('gms-enableEl', function (id) {
    $(id).prop("disabled", false);
  });
  Shiny.addCustomMessageHandler('gms-disableEl', function (id) {
    $(id).prop("disabled", true);
  });
  Shiny.addCustomMessageHandler('gms-toggleEl', function (id) {
    if ($(id).is(":visible")) {
      $(id).toggle();
      $(id).trigger("shown");
    } else {
      $(id).fadeToggle(600);
      $(id).trigger("hidden");
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
    el.id = $.makeArray(el.id);
    el.name = $.makeArray(el.name);

    for (i = 0; i < el.id.length; i++) {
      if (el.allowExec) {
        checkBoxHTML = '<div class="col-sm-6"><div class="form-group shiny-input-container"><div class="checkbox"><label><input type="checkbox" onchange="Shiny.setInputValue(\'execPermAttachment_' + el.id[i] + '\', $(this).is(\':checked\'));" checked="checked"><span>' + el.labelCb + '</span></label></div></div></div>';
      } else {
        checkBoxHTML = '';
      }

      $('<div class="row attachment-line"><div class="col-sm-6"><button class="btn btn-default bt-icon" id="btRemoveAttachment_' + el.id[i] + '" type="button" onclick="removeAttachment(' + el.id[i] + ')"><i class="fa fa-times-circle"></i></button> ' + el.name[i] + '</div>' + checkBoxHTML + '</div>').insertBefore('#endAttachList');
    }
  });
  Shiny.addCustomMessageHandler('gms-fitTitleInBox', function (id) {
    setTimeout(function () {
      var el = $(id);
      var parentEl = el.parent()[0];
      el.css("font-size", "18px");
      var currSize = 18;

      while (parentEl.scrollWidth > parentEl.clientWidth && currSize >= 10) {
        currSize -= 2;
        el.css("font-size", currSize + "px");
      }
    }, 500);
  });
});

// counter
let count = 1; // maximum number of scenarios that can be loaded in compare view

let maxNumScen = 50;
$(document).keyup(function (event) {
  // ENTER will confirm modal dialogues
  if (event.keyCode == 13 && !event.ctrlKey) {
    if ($("#shiny-modal").find(".selectize-input.input-active").length > 0 || $("#shiny-modal").find("*[data-dismiss='modal']").is(":focus")) {
      return;
    }

    $(".bt-gms-confirm:visible:enabled").click();
  } // ESC will close modal dialogues


  if (event.keyCode === 27) {
    $('.modal').modal('hide');
  } // Import shortcut: CTRL + ALT + I


  if (event.ctrlKey && event.altKey && event.keyCode === 73) {
    if ($("#btImport").is(":visible")) {
      $("#btImport").click();
    } else if ($("#btLoadScen").is(":visible")) {
      $("#btLoadScen").click();
    }
  } // SAVE shortcut: CTRL + ALT + S


  if (event.ctrlKey && event.altKey && event.keyCode === 83) {
    Shiny.setInputValue("btSave", 1, {
      priority: "event"
    });
  } // Solve shortcut: CTRL + ALT + ENTER


  if (event.ctrlKey && event.altKey && event.keyCode === 13 && $("#btSolve").is(":enabled") && $("#btSolve").is(":visible")) {
    $("#btSolve").click();
  } // Remove shortcut: CTRL + ALT + R


  if (event.ctrlKey && event.altKey && event.keyCode === 82) {
    Shiny.setInputValue("btDelete", 1, {
      priority: "event"
    });
  } // Close shortcut (remove button in input sheet): CTRL + ALT + C


  if (event.ctrlKey && event.altKey && event.keyCode === 67 && $(".btRemove").is(":visible")) {
    $(".btRemove:visible").click();
  } // Close shortcut (remove button in output sheet): CTRL + ALT + C


  if (event.ctrlKey && event.altKey && event.keyCode === 67) {
    for (i = 2; i <= maxNumScen; i++) {
      id = '#close_' + i;

      if ($(id).is(":visible")) {
        $(id).click();
      }
    }
  } // Fullscreen mode (hide sidebar) shortcut: CTRL + ALT + F


  if (event.ctrlKey && event.altKey && event.keyCode === 70) {
    $("body").toggleClass("sidebar-collapse");
    rerenderHot(400);
  } // Select input menu shortcut: CTRL + ALT + 1


  if (event.ctrlKey && event.altKey && event.keyCode === 49) {
    $("a[href='#shiny-tab-inputData']").click();
  } // Select output menu shortcut: CTRL + ALT + 2


  if (event.ctrlKey && event.altKey && event.keyCode === 50) {
    tab = $("a[href='#shiny-tab-outputData']");

    if (tab.length > 0) {
      tab.click();
    } else {
      $("a[href='#shiny-tab-importData']").click();
    }
  } // Select gams interaction menu shortcut: CTRL + ALT + 3


  if (event.ctrlKey && event.altKey && event.keyCode === 51) {
    tab = $("a[href='#shiny-tab-gamsinter']");

    if (tab.length > 0) {
      tab.click();
    } else {
      $("a[href='#shiny-tab-loadResults']").click();
    }
  } // Select scenario menu shortcut: CTRL + ALT + 4


  if (event.ctrlKey && event.altKey && event.keyCode === 52) {
    tab = $("a[href='#shiny-tab-scenarios']").click();
  }

  if (event.ctrlKey && event.altKey && event.keyCode === 53) {
    tab = $("a[href='#shiny-tab-hcubeAnalyze']");

    if (tab.length > 0) {
      tab.click();
    }
  } // Table view (scenario compare mode) shortcut: CTRL + ALT + T


  if (event.ctrlKey && event.altKey && event.keyCode === 84) {
    id = '#btGraphIn';

    if ($(id).is(":visible") && $(id).is(":enabled")) {
      $(id).click();
    }

    id = '#outputTableView';

    if ($(id).is(":visible")) {
      $(id).click();
    }

    for (i = 2; i <= maxNumScen + 3; i++) {
      id = '#table_' + i;

      if ($(id).is(":visible")) {
        $(id).click();
      }
    }
  } // Select next tab shortcut: CTRL + ALT + arrow right


  if (event.ctrlKey && event.altKey && event.keyCode === 39) {
    Shiny.onInputChange("tabsetShortcutNext", count);
    count++;
  } // Select previous tab shortcut: CTRL + ALT + arrow left


  if (event.ctrlKey && event.altKey && event.keyCode === 37) {
    Shiny.onInputChange("tabsetShortcutPrev", count);
    count++;
  } // Nest to next lower tabset shortcut: CTRL + ALT + arrow down


  if (event.ctrlKey && event.altKey && event.keyCode === 40) {
    Shiny.onInputChange("tabsetShortcutNest", count);
    count++;
  } // Unnest to next higher tabset shortcut: CTRL + ALT + arrow up


  if (event.ctrlKey && event.altKey && event.keyCode === 38) {
    Shiny.onInputChange("tabsetShortcutUnnest", count);
    count++;
  } // Activate/deactivate scenario comparison mode: CTRL + ALT + space


  if (event.ctrlKey && event.altKey && event.keyCode === 32 && $("#btCompareScen").is(":enabled") && $("#btCompareScen").is(":visible")) {
    $("#btCompareScen").click();
  }
});