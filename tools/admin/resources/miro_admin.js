/*\
|*|
|*|  Polyfill which enables the passage of arbitrary arguments to the
|*|  callback functions of JavaScript timers (HTML5 standard syntax).
|*|
|*|  https://developer.mozilla.org/en-US/docs/DOM/window.setInterval
|*|
|*|  Syntax:
|*|  var timeoutID = window.setTimeout(func, delay[, param1, param2, ...]);
|*|  var timeoutID = window.setTimeout(code, delay);
|*|  var intervalID = window.setInterval(func, delay[, param1, param2, ...]);
|*|  var intervalID = window.setInterval(code, delay);
|*|
\*/

(function() {
  setTimeout(function(arg1) {
    if (arg1 === 'test') {
      // feature test is passed, no need for polyfill
      return;
    }
    var __nativeST__ = window.setTimeout;
    window.setTimeout = function(vCallback, nDelay /*, argumentToPass1, argumentToPass2, etc. */ ) {
      var aArgs = Array.prototype.slice.call(arguments, 2);
      return __nativeST__(vCallback instanceof Function ? function() {
        vCallback.apply(null, aArgs);
      } : vCallback, nDelay);
    };
  }, 0, 'test');

  var interval = setInterval(function(arg1) {
    clearInterval(interval);
    if (arg1 === 'test') {
      // feature test is passed, no need for polyfill
      return;
    }
    var __nativeSI__ = window.setInterval;
    window.setInterval = function(vCallback, nDelay /*, argumentToPass1, argumentToPass2, etc. */ ) {
      var aArgs = Array.prototype.slice.call(arguments, 2);
      return __nativeSI__(vCallback instanceof Function ? function() {
        vCallback.apply(null, aArgs);
      } : vCallback, nDelay);
    };
  }, 0, 'test');
}());


var indices            = [];
var indexAliases       = [];
var scalarIndices      = [];
var scalarIndexAliases = [];
var elInArrayCounter   = {};
var elInArray          = {};


function addBarDataEl(){
  var arrayID      = 'chart_ydatabar';
  var elements     = {'chart_ydata' : ['select', 'What should be plotted on the y axis?', indices, indexAliases], 
  'chart_ylabel' : ['text', 'What label should be used?', 'label'],
  'marker_color' : ['text', 'Select bar color', 'black'],
  'marker_line_width': ['numeric', 'Select bar outline width', 0, 0],
  'marker_line_color' : ['text', 'Select bar outline color']
  };
  
  addArrayEl(arrayID, elements);
}
function addScatterDataEl(){
  var arrayID      = 'chart_ydatascatter';
  var elements     = {'chart_ydata' : ['select', 'What should be plotted on the y axis?', scalarIndices, scalarIndexAliases], 
  'chart_ylabel' : ['text', 'What label should be used?', 'label'],
  'marker_symbol': ['select', 'Select marker symbol', ["circle", "circle-open", "circle-dot", 
                                                        "circle-open-dot", "square", "square-open",
                                                        "square-dot", "square-open-dot", "diamond",
                                                        "diamond-open", "diamond-dot", "diamond-open-dot",
                                                        "cross", "cross-open", "cross-dot", "cross-open-dot",
                                                        "x", "x-open", "x-dot", "x-open-dot", "triangle-up",
                                                        "triangle-up-open", "triangle-up-dot", "triangle-up-open-dot",
                                                        "triangle-down", "triangle-down-open", "triangle-down-dot",
                                                        "triangle-down-open-dot", "triangle-left", "triangle-left-open", 
                                                        "triangle-left-dot", "triangle-left-open-dot", "triangle-right",
                                                        "triangle-right-open", "triangle-right-dot", "triangle-right-open-dot",
                                                        "triangle-ne", "triangle-ne-open", "triangle-ne-dot", "triangle-ne-open-dot",
                                                        "triangle-se", "triangle-se-open", "triangle-se-dot", "triangle-se-open-dot",
                                                        "triangle-sw", "triangle-sw-open", "triangle-sw-dot", "triangle-sw-open-dot",
                                                        "triangle-nw", "triangle-nw-open", "triangle-nw-dot", "triangle-nw-open-dot",
                                                        "pentagon", "pentagon-open", "pentagon-dot", "pentagon-open-dot", "hexagon",
                                                        "hexagon-open", "hexagon-dot", "hexagon-open-dot", "hexagon2", "hexagon2-open", 
                                                        "hexagon2-dot", "hexagon2-open-dot", "octagon", "octagon-open", "octagon-dot",
                                                        "octagon-open-dot", "star", "star-open", "star-dot", "star-open-dot", "hexagram",
                                                        "hexagram-open", "hexagram-dot", "hexagram-open-dot", "star-triangle-up",
                                                        "star-triangle-up-open", "star-triangle-up-dot", "star-triangle-up-open-dot",
                                                        "star-triangle-down", "star-triangle-down-open", "star-triangle-down-dot", "star-triangle-down-open-dot",
                                                        "star-square", "star-square-open", "star-square-dot", "star-square-open-dot", "star-diamond", 
                                                        "star-diamond-open", "star-diamond-dot", "star-diamond-open-dot", "diamond-tall",
                                                        "diamond-tall-open", "diamond-tall-dot", "diamond-tall-open-dot", "diamond-wide",
                                                        "diamond-wide-open", "diamond-wide-dot", "diamond-wide-open-dot", "hourglass",
                                                        "hourglass-open", "bowtie", "bowtie-open", "circle-cross", "circle-cross-open", 
                                                        "circle-x", "circle-x-open", "square-cross", "square-cross-open", "square-x", "square-x-open",
                                                        "diamond-cross", "diamond-cross-open", "diamond-x", "diamond-x-open", "cross-thin",
                                                        "cross-thin-open", "x-thin", "x-thin-open", "asterisk", "asterisk-open", "hash", "hash-open",
                                                        "hash-dot", "hash-open-dot", "y-up", "y-up-open", "y-down", "y-down-open", "y-left", 
                                                        "y-left-open", "y-right", "y-right-open", "line-ew", "line-ew-open", "line-ns", 
                                                        "line-ns-open", "line-ne", "line-ne-open", "line-nw", "line-nw-open"]],
  'marker_color' : ['text', 'Select marker color', 'black'],
  'marker_opacity' : ['numeric', 'Select marker opacity', 1, 0, 1],
  'marker_size' : ['numeric', 'Select marker size', 6, 0],
  'marker_line_width': ['numeric', 'Select marker outline width', 0, 0],
  'marker_line_color' : ['text', 'Select marker outline color']
  };
  addArrayEl(arrayID, elements);
}
function addLineDataEl(){
  var arrayID      = 'chart_ydataline';
  var elements     = {'chart_ydata' : ['select', 'What should be plotted on the y axis?', scalarIndices, scalarIndexAliases], 
  'chart_ylabel' : ['text', 'What label should be used?', 'label'],
  'line_color' : ['text', 'Select line color', 'black'],
  'line_width' : ['numeric', 'Select line width', 2, 0],
  'line_shape' : ['select', 'Select line shape', ["linear", "spline", "hv", "vh", "hvh", "vhv"]],
  'line_dash' : ['select', 'Select line dash type', ["solid", "dot", "dash", "longdash", "dashdot", "longdashdot"]]
  };
  addArrayEl(arrayID, elements);
}
function addHistDataEl(){
  var arrayID      = 'hist_data';
  var elements     = {'hist_data' : ['select', 'What should be plotted?', scalarIndices, scalarIndexAliases], 
  'hist_label' : ['text', 'What label should be used?', 'label'],
  'hist_color' : ['text', 'Select bar color', 'black'],
  'hist_alpha' : ['numeric', 'Choose bar transparency', 1, 0, 1]
  };
  addArrayEl(arrayID, elements);
}
function addDyDataEl(){
  var arrayID      = 'dy_ydata';
  var elements     = {'chart_ydata' : ['select', 'What index do you want to plot on the y-axis?', scalarIndices, scalarIndexAliases], 
  'dyser_color' : ['text', 'What color should be used for this series?'],
  'dyopt_stepPlot' : ['checkbox', 'Do you want the series to be a step plot?'],
  'dyopt_stemPlot' : ['checkbox', 'Do you want the series to be a stem plot?'],
  'dyopt_fillGraph' : ['checkbox', 'Should the area underneath the graph be filled?'],
  'dyopt_drawPoints' : ['checkbox', 'Should points be drawn?'],
  'dyopt_pointShape' : ['select', 'What shape should points have?',["dot", "triangle", "square", "diamond", "pentagon", "hexagon", 
                              "circle", "star", "plus", "ex"]],
  'dyopt_pointSize' : ['numeric', 'What size should points be?', 2, 0]
  };
  addArrayEl(arrayID, elements);
}
function addArrayDataEl(arrayID){
  switch(arrayID){
    case "chart_ydatabar":
      addBarDataEl();
    break;
    case "chart_ydatascatter":
      addScatterDataEl();
    break;
    case "chart_ydataline":
      addLineDataEl();
    break;
    case "hist_data":
      addHistDataEl();
    break;
    case "dy_ydata":
      addDyDataEl();
    break;
  }
}
function addArrayDataElWrapper(arrayID){
  if($('#' + arrayID + '_wrapper').is(':visible')){
    addArrayDataEl(arrayID);
  }else{
    setTimeout(addArrayDataElWrapper, 200, arrayID);
  }
}

function incElCount(arrayID){
  var count = $('#' + arrayID + '_wrapper .array-wrapper').children(".config-array-el").length;
  if(count === 0){
    delete elInArray[arrayID];
  }
  elInArrayCounter[arrayID] = count + 1;
  return(elInArrayCounter[arrayID]);
}
function addLabelEl(arrayID, label){
  if(arrayID in elInArray){
    if($.inArray(label, elInArray[arrayID]) !== -1){
      throw "Label: " + label + " already in use.";
    }
    elInArray[arrayID].push(label);
    return;
  }
  elInArray[arrayID] = [label];
}
  
function addArrayEl(arrayID, elements){
  var elID         = incElCount(arrayID);
  var label        = '';
  var arrayContent = '<div id="' + arrayID + elID + '_wrapper" class="config-array-el">';
  var idx          = 0;
  $.each(elements, function( k, v ) {
    switch(v[0]){
      case 'select':
        var selected = undefined;
        if(idx === 0){
          // as labels need to be unique, set default to option that is not already in use
          var noElInArray = (elInArray[arrayID] !== undefined? elInArray[arrayID].length: 0);
          if(v[2].length - 1 === noElInArray){
            $('#' + arrayID + '_wrapper .btn-add-array-el').prop('disabled', true);
          }
          for(var i = 0; i < v[2].length; i++){
            if($.inArray(v[2][i], elInArray[arrayID]) === -1){
              selected = v[2][i];
              label    = v[3][i];
              break;
            }
          }
          addLabelEl(arrayID, selected);
          // tell R that new element was addded to array: (ID, label of element)
          Shiny.setInputValue(k, [elID, selected], {priority: "event"});
        }
        arrayContent += createSelectInput(k, elID, v[1], v[2], v[3], selected);
      break;
      case 'text':
        var value = v[2]; 
        if(value === 'label'){
          value = label;
        }
        arrayContent += createTextInput(k, elID, v[1], value);
      break;
      case 'numeric':
        arrayContent += createNumericInput(k, elID, v[1], v[2], v[3], v[4]);
      break;
      case 'checkbox':
        arrayContent += createCheckboxInput(k, elID, v[1], v[2]);
        break;
      default:
        throw "Unknown element type: " + v[0];
    }
    idx++;
  });
  arrayContent += (elID > 1? '<button type="button" onclick="removeArrayEl(\'' + arrayID + '\',\'' + elID +
  '\')" class="btn btn-default bt-icon"><i class="far fa-minus-square"></i></button>\n': '') + '<hr></div>';
  
  $('#' + arrayID + '_wrapper .array-wrapper').append(arrayContent);
  
  $.each(elements, function( k, v ) {
    if(v[0] === 'select'){
        $('#' + k + elID).selectize({
      	  onChange: function(value) {
      	    console.log(value);
      	    Shiny.setInputValue(k, [elID, value], {priority: "event"});
          }
      	});
    }else if(v[0] === 'checkbox'){
      $('#' + k + elID).on('change', function(){
        Shiny.setInputValue(k, [elID, $(this).prop('checked')], {priority: "event"});
      });
    }else{
      $('#' + k + elID).on('change', function(){
        Shiny.setInputValue(k, [elID, $(this).val()], {priority: "event"});
      });
    }
  });
}

function createSelectInput(arrayID, elID, label, choices){
  var id = arrayID + elID;
  var aliases  = arguments.length > 4 && arguments[4] !== undefined ? arguments[4] : choices;
  var selected = arguments.length > 5 && arguments[5] !== undefined ? arguments[5] : '';
  optionsHTML  = '';
  
  for (var i = 0, itemLen = choices.length; i < itemLen; 
  optionsHTML += '<option value="' + choices[i] + '"' + (choices[i] === selected? ' selected' : '') + 
  '>' + aliases[i++] + '</option>\n');
  
  return('<div class="form-group">\n' +
  '<label class="control-label" for="' +
  id + '">' + label + '</label>\n' +
  '<div><select id="' + id + '">' + optionsHTML + '</select>\n</div>\n</div>');
}
function createTextInput(arrayID, elID, label){
  var id = arrayID + elID;
  var value = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : '';
  Shiny.setInputValue(arrayID, [elID, value], {priority: "event"});
  
  return('<div class="form-group">\n' +
  '<label for="' + id + '">' + label + '</label>\n' +
  '<input id="' + id + '" type="text" class="form-control" value="' + value + '"/>\n' +
'</div>');
}
function createNumericInput(arrayID, elID, label, value){
  var id = arrayID + elID;
  var min   = arguments.length > 4 && arguments[4] !== undefined ? arguments[4] : null;
  var max   = arguments.length > 5 && arguments[5] !== undefined ? arguments[5] : null;
  Shiny.setInputValue(arrayID, [elID, value], {priority: "event"});
  
  return('<div class="form-group">\n' +
  '<label for="' + id + '">' + label + '</label>\n' +
  '<input id="' + id + '" type="number" class="form-control" value="' + value + '"' + 
  (min === null ? ' min="' + min + '"': '') + (max === null ? ' max="' + max + '"': '') + '/>\n' +
'</div>');
}
function createCheckboxInput(arrayID, elID, label){
  var id = arrayID + elID;
  var value = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : false;
  Shiny.setInputValue(arrayID, [elID, value], {priority: "event"});
  
  return('<div class="form-group shiny-input-container">\n' +
  '<div class="checkbox">\n' +
    '<label>\n' +
      '<input id="' + id + '" type="checkbox"' + (value === true? ' checked="checked"': '') + '/>\n' +
      '<span>' + label + '</span>\n' +
    '</label>\n' +
  '</div>\n' +
'</div>');
}
function removeArrayEl(arrayID, elID){
  var arrayLabel = '';
  var el    = null;
  var idx   = 0;
  $.each($('#' + arrayID + elID + '_wrapper .form-group'), function(k,v){
    if($(v).children('input[type="number"]').length){
      el = $(v).children('input[type="number"]');
      if(idx === 0){
        arrayLabel = el.val();
      }
      el.remove();
    }else if($(v).children('input[type="text"]').length){
      el = $(v).children('input[type="text"]');
      if(idx === 0){
        arrayLabel = el.val();
      }
      el.remove();
    }else if($(v).children('input[type="checkbox"]').length){
      el = $(v).children('input[type="checkbox"]');
      if(idx === 0){
        arrayLabel = $(el).prop('checked');
      }
      el.remove();
    }else if($(v).find('select').length){
      el         = $(v).find('select').get(0);
      if(idx === 0){
        arrayLabel = $(el).val();
      }
      el.selectize.destroy();
    }else{
      throw "Could not remove array element.";
    }
    idx++;
  });
  if(arrayLabel.length === 0){
    throw "Failed to remove array element: Could not find identifier for array element: " +
    arrayID + ' (label ID: ' + arrayID + elID + ').';
  }
  elInArray[arrayID].splice($.inArray(arrayLabel, elInArray[arrayID]), 1);
  $('#' + arrayID + '_wrapper .btn-add-array-el:disabled').prop('disabled', false);
  Shiny.setInputValue('remove_array_el', [arrayID, arrayLabel, elID], {
    priority: "event"
  });
  
  $('#' + arrayID + elID + '_wrapper').remove();
}

$(document).ready(function () {
  Shiny.addCustomMessageHandler('gms-setIndices', function (indicesFromR) {
    $.each(indicesFromR, function(key, val) {
      if(!$.isArray(val)){
        indicesFromR[key] = [val];
      }
    });
    indices = indicesFromR.indices;
    indexAliases = indicesFromR.aliases;
    scalarIndices = indicesFromR.scalarIndices;
    scalarIndexAliases = indicesFromR.scalarAliases;
  });
  Shiny.addCustomMessageHandler('gms-addArrayEl', function(arrayID){
    setTimeout(addArrayDataElWrapper, 200, arrayID);
  });
});