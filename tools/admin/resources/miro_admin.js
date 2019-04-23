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

/*
 * jQuery throttle / debounce - v1.1 - 3/7/2010
 * http://benalman.com/projects/jquery-throttle-debounce-plugin/
 * 
 * Copyright (c) 2010 "Cowboy" Ben Alman
 * Dual licensed under the MIT and GPL licenses.
 * http://benalman.com/about/license/
 */
(function(b,c){var $=b.jQuery||b.Cowboy||(b.Cowboy={}),a;$.throttle=a=function(e,f,j,i){var h,d=0;if(typeof f!=="boolean"){i=j;j=f;f=c}function g(){var o=this,m=+new Date()-d,n=arguments;function l(){d=+new Date();j.apply(o,n)}function k(){h=c}if(i&&!h){l()}h&&clearTimeout(h);if(i===c&&m>e){l()}else{if(f!==true){h=setTimeout(i?k:l,i===c?e-m:e)}}}if($.guid){g.guid=j.guid=j.guid||$.guid++}return g};$.debounce=function(d,e,f){return f===c?a(d,e,false):a(d,f,e!==false)}})(this);


var indices            = [];
var indexAliases       = [];
var scalarIndices      = [];
var scalarIndexAliases = [];
var outputScalars      = [];
var outputScalarAliases = [];
var elInArrayCounter   = {};
var elInArray          = {};

function addDyEvent(){
  var arrayID      = 'dy_dyEvent';
  var elements     = {'dy_dyEvent' : ['select', 'Which symbol shall be plotted?', outputScalars, outputScalarAliases],
  'dyEvent_label': ['text', 'What label should be used?'],
  'dyEvent_labelLoc': ['select', 'Select marker symbol', ['top', 'bottom']],
  'dyEvent_color': ['color', 'What color should the event line have?', 'rgb(0,0,0)'],
  'dyEvent_strokePattern': ['select', 'Select marker symbol', ['dashed', 'dotted', 'dotdash', 'solid']]
  };
  addArrayEl(arrayID, elements, {elRequired: false});
}
function addDyAnnotation(){
  var arrayID      = 'dy_dyAnnotation';
  var elements     = {'dy_dyAnnotation' : ['select', 'What shall be annotated?', outputScalars, outputScalarAliases],
  'dyAnnotation_text': ['text', 'What text should be used?', outputScalarAliases[1]],
  'dyAnnotation_tooltip': ['text', 'What tooltip should be used?'],
  'dyAnnotation_width': ['numeric', 'Specify annotation width', 0, 0],
  'dyAnnotation_height': ['numeric', 'Specify annotation height', 0, 0],
  'dyAnnotation_attachAtBottom': ['checkbox', 'Should annotation be attached to x axis?']
  };
  addArrayEl(arrayID, elements, {elRequired: false});
}
function addDyShading(){
  var arrayID      = 'dy_dyShading';
  var elements     = {'dy_dyShading' : ['select', 'Which symbol shall be used as lower bound for shading?', outputScalars, outputScalarAliases],
  'dyShading_up' : ['select', 'Which symbol shall be used as upper bound for shading?', outputScalars, outputScalarAliases],
  'dyShading_axis': ['select', 'Select axis where shading shall be applied to', ['x', 'y']],
  'dyShading_color': ['color', 'What color should the event line have?', '#EFEFEF']
  };
  addArrayEl(arrayID, elements, {elRequired: false});
}

function addBarDataEl(){
  var arrayID      = 'chart_ydatabar';
  var elements     = {'chart_ydata' : ['select', 'What should be plotted on the y axis?', indices, indexAliases], 
  'chart_ylabel' : ['text', 'What label should be used?', 'label'],
  'marker_color' : ['color', 'Select bar color', 'rgb(0,0,0)'],
  'marker_line_width': ['numeric', 'Select bar outline width', 0, 0],
  'marker_line_color' : ['color', 'Select bar outline color']
  };
  
  addArrayEl(arrayID, elements);
}
function addScatterDataEl(){
  var arrayID      = 'chart_ydatascatter';
  var elements     = {'chart_ydata' : ['select', 'What should be plotted on the y axis?', scalarIndices, scalarIndexAliases], 
  'chart_ylabel' : ['text', 'What label should be used?', 'label'],
  'marker_symbol': ['select', 'Select marker symbol', ['circle', 'circle-open', 'circle-dot', 
                                                        'circle-open-dot', 'square', 'square-open',
                                                        'square-dot', 'square-open-dot', 'diamond',
                                                        'diamond-open', 'diamond-dot', 'diamond-open-dot',
                                                        'cross', 'cross-open', 'cross-dot', 'cross-open-dot',
                                                        'x', 'x-open', 'x-dot', 'x-open-dot', 'triangle-up',
                                                        'triangle-up-open', 'triangle-up-dot', 'triangle-up-open-dot',
                                                        'triangle-down', 'triangle-down-open', 'triangle-down-dot',
                                                        'triangle-down-open-dot', 'triangle-left', 'triangle-left-open', 
                                                        'triangle-left-dot', 'triangle-left-open-dot', 'triangle-right',
                                                        'triangle-right-open', 'triangle-right-dot', 'triangle-right-open-dot',
                                                        'triangle-ne', 'triangle-ne-open', 'triangle-ne-dot', 'triangle-ne-open-dot',
                                                        'triangle-se', 'triangle-se-open', 'triangle-se-dot', 'triangle-se-open-dot',
                                                        'triangle-sw', 'triangle-sw-open', 'triangle-sw-dot', 'triangle-sw-open-dot',
                                                        'triangle-nw', 'triangle-nw-open', 'triangle-nw-dot', 'triangle-nw-open-dot',
                                                        'pentagon', 'pentagon-open', 'pentagon-dot', 'pentagon-open-dot', 'hexagon',
                                                        'hexagon-open', 'hexagon-dot', 'hexagon-open-dot', 'hexagon2', 'hexagon2-open', 
                                                        'hexagon2-dot', 'hexagon2-open-dot', 'octagon', 'octagon-open', 'octagon-dot',
                                                        'octagon-open-dot', 'star', 'star-open', 'star-dot', 'star-open-dot', 'hexagram',
                                                        'hexagram-open', 'hexagram-dot', 'hexagram-open-dot', 'star-triangle-up',
                                                        'star-triangle-up-open', 'star-triangle-up-dot', 'star-triangle-up-open-dot',
                                                        'star-triangle-down', 'star-triangle-down-open', 'star-triangle-down-dot', 'star-triangle-down-open-dot',
                                                        'star-square', 'star-square-open', 'star-square-dot', 'star-square-open-dot', 'star-diamond', 
                                                        'star-diamond-open', 'star-diamond-dot', 'star-diamond-open-dot', 'diamond-tall',
                                                        'diamond-tall-open', 'diamond-tall-dot', 'diamond-tall-open-dot', 'diamond-wide',
                                                        'diamond-wide-open', 'diamond-wide-dot', 'diamond-wide-open-dot', 'hourglass',
                                                        'hourglass-open', 'bowtie', 'bowtie-open', 'circle-cross', 'circle-cross-open', 
                                                        'circle-x', 'circle-x-open', 'square-cross', 'square-cross-open', 'square-x', 'square-x-open',
                                                        'diamond-cross', 'diamond-cross-open', 'diamond-x', 'diamond-x-open', 'cross-thin',
                                                        'cross-thin-open', 'x-thin', 'x-thin-open', 'asterisk', 'asterisk-open', 'hash', 'hash-open',
                                                        'hash-dot', 'hash-open-dot', 'y-up', 'y-up-open', 'y-down', 'y-down-open', 'y-left', 
                                                        'y-left-open', 'y-right', 'y-right-open', 'line-ew', 'line-ew-open', 'line-ns', 
                                                        'line-ns-open', 'line-ne', 'line-ne-open', 'line-nw', 'line-nw-open']],
  'marker_color' : ['color', 'Select marker color', 'rgb(0,0,0)'],
  'marker_opacity' : ['numeric', 'Select marker opacity', 1, 0, 1],
  'marker_size' : ['numeric', 'Select marker size', 6, 0],
  'marker_line_width': ['numeric', 'Select marker outline width', 0, 0],
  'marker_line_color' : ['color', 'Select marker outline color']
  };
  addArrayEl(arrayID, elements);
}
function addLineDataEl(){
  var arrayID      = 'chart_ydataline';
  var elements     = {'chart_ydata' : ['select', 'What should be plotted on the y axis?', scalarIndices, scalarIndexAliases], 
  'chart_ylabel' : ['text', 'What label should be used?', 'label'],
  'line_color' : ['text', 'Select line color', 'rgb(0,0,0)'],
  'line_width' : ['numeric', 'Select line width', 2, 0],
  'line_shape' : ['select', 'Select line shape', ['linear', 'spline', 'hv', 'vh', 'hvh', 'vhv']],
  'line_dash' : ['select', 'Select line dash type', ['solid', 'dot', 'dash', 'longdash', 'dashdot', 'longdashdot']]
  };
  addArrayEl(arrayID, elements);
}
function addHistDataEl(){
  var arrayID      = 'hist_xdata';
  var elements     = {'hist_xdata' : ['select', 'What should be plotted?', scalarIndices, scalarIndexAliases], 
  'hist_label' : ['text', 'What label should be used?', 'label'],
  'hist_color' : ['color', 'Select bar color', 'rgb(0,0,0)'],
  'hist_alpha' : ['numeric', 'Choose bar transparency', 1, 0, 1]
  };
  addArrayEl(arrayID, elements);
}
function addDyDataEl(){
  var arrayID      = 'dy_ydata';
  var elements     = {'chart_ydata' : ['select', 'What index do you want to plot on the y-axis?', scalarIndices, scalarIndexAliases], 
  'dyser_color' : ['color', 'What color should be used for this series?'],
  'dyopt_stepPlot' : ['checkbox', 'Do you want the series to be a step plot?'],
  'dyopt_stemPlot' : ['checkbox', 'Do you want the series to be a stem plot?'],
  'dyopt_fillGraph' : ['checkbox', 'Should the area underneath the graph be filled?'],
  'dyopt_drawPoints' : ['checkbox', 'Should points be drawn?'],
  'dyopt_pointShape' : ['select', 'What shape should points have?',['dot', 'triangle', 'square', 'diamond', 'pentagon', 'hexagon', 
                              'circle', 'star', 'plus', 'ex']],
  'dyopt_pointSize' : ['numeric', 'What size should points be?', 2, 0]
  };
  addArrayEl(arrayID, elements);
}
function addArrayDataEl(arrayID){
  if($('#' + arrayID + '_wrapper .btn-add-array-el').is(':disabled')){
    return;
  }
  switch(arrayID){
    case 'chart_ydatabar':
      addBarDataEl();
    break;
    case 'chart_ydatascatter':
      addScatterDataEl();
    break;
    case 'chart_ydataline':
      addLineDataEl();
    break;
    case 'hist_xdata':
      addHistDataEl();
    break;
    case 'dy_ydata':
      addDyDataEl();
    break;
    case 'dy_dyEvent':
      addDyEvent();
    break;
    case 'dy_dyAnnotation':
      addDyAnnotation();
    break;
    case 'dy_dyShading':
      addDyShading();
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
  var count = $('#' + arrayID + '_wrapper .array-wrapper').children('.config-array-el').length;
  if(count === 0){
    delete elInArray[arrayID];
  }
  elInArrayCounter[arrayID] = count + 1;
  return(elInArrayCounter[arrayID]);
}
function addLabelEl(arrayID, label){
  if(arrayID in elInArray){
    if($.inArray(label, elInArray[arrayID]) !== -1){
      throw 'Label: ' + label + ' already in use.';
    }
    elInArray[arrayID].push(label);
    return;
  }
  elInArray[arrayID] = [label];
}
  
function addArrayEl(arrayID, elements, options){
  if(options === undefined) options = {};
  if(options.elRequired === undefined) options.elRequired = true;
  if(options.updateTxtWithLabel === undefined) options.updateTxtWithLabel = [];
  
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
          Shiny.setInputValue("add_array_el", [elID, selected, k], {priority: "event"});
        }
        arrayContent += createSelectInput(k, elID, v[1], v[2], v[3], selected);
      break;
      case 'text':
        var value = v[2]; 
        if(value === 'label'){
          value = label;
          options.updateTxtWithLabel.push('#' + k + elID);
        }
        arrayContent += createTextInput(k, elID, v[1], value);
      break;
      case 'numeric':
        arrayContent += createNumericInput(k, elID, v[1], v[2], v[3], v[4]);
      break;
      case 'checkbox':
        arrayContent += createCheckboxInput(k, elID, v[1], v[2]);
        break;
      case 'color':
        arrayContent += createColorPickerInput(k, elID, v[1], v[2]);
        break;
      default:
        throw "Unknown element type: " + v[0];
    }
    idx++;
  });
  arrayContent += ((!options.elRequired || elID > 1)? '<button type="button" onclick="removeArrayEl(\'' + arrayID + '\',\'' + elID +
  '\')" class="btn btn-default bt-icon"><i class="far fa-minus-square"></i></button>\n': '') + '<hr></div>';
  
  $('#' + arrayID + '_wrapper .array-wrapper').append(arrayContent);
  
  idx = 0;
  $.each(elements, function( k, v ) {
    if(v[0] === 'select'){
      if(idx === 0){
        $('#' + k + elID).selectize({
      	  onChange: function(value) {
      	    if(options.updateTxtWithLabel.length){
      	      var alias = outputScalarAliases[outputScalars.findIndex(function(val){
                            return val === value;
                          })];
      	      for(var i = 0; i < options.updateTxtWithLabel.length; i++){
        	      $(options.updateTxtWithLabel[i]).val(alias);
        	    }
      	    }
      	    
      	    Shiny.setInputValue("add_array_el", [elID, value, k, "change"], {priority: "event"});
          }
      	});
      }else{
        $('#' + k + elID).selectize({
      	  onChange: function(value) {
      	    Shiny.setInputValue(k, [elID, value], {priority: "event"});
          }
      	});
      }
    }else if(v[0] === 'checkbox'){
      $('#' + k + elID).on('change', function(){
        Shiny.setInputValue(k, [elID, $(this).prop('checked')], {priority: "event"});
      });
    }else if(v[0] === 'color'){
       $('#' + k + elID).colorpicker({
         align: "left"
       });
       $('#' + k + elID).on('change', $.debounce(500, function(){
        Shiny.setInputValue(k, [elID, $(this).val()], {priority: "event"});
      }));
    }else{
      $('#' + k + elID).on('change', $.debounce(250, function(){
        Shiny.setInputValue(k, [elID, $(this).val()], {priority: "event"});
      }));
    }
    idx++;
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
  
  return('<div class="form-group">\n' +
  '<div class="checkbox">\n' +
    '<label>\n' +
      '<input id="' + id + '" type="checkbox"' + (value === true? ' checked="checked"': '') + '/>\n' +
      '<span>' + label + '</span>\n' +
    '</label>\n' +
  '</div>\n' +
'</div>');
}
function createColorPickerInput(arrayID, elID, label){
  var id = arrayID + elID;
  var value = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : '';
  Shiny.setInputValue(arrayID, [elID, value], {priority: "event"});

  return('<div class="form-group shiny-input-container">\n' +
  '<label for="' + id + '">' + label + '</label>\n' +
  '<input id="' + id + '" type="text" class="form-control miro-color-picker" value="' + value + '"/>\n' +
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
    }else if($(v).children('.miro-color-picker').length){
      el = $(v).children('.miro-color-picker');
      if(idx === 0){
        arrayLabel = el.val();
      }
      el.colorpicker('destroy');
    }else if($(v).children('input[type="text"]').length){
      el = $(v).children('input[type="text"]');
      if(idx === 0){
        arrayLabel = el.val();
      }
      el.remove();
    }else if($(v).find('input[type="checkbox"]').length){
      el = $(v).find('input[type="checkbox"]');
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
  Shiny.addCustomMessageHandler('gms-setScalarOutputs', function (scalarData) {
    $.each(scalarData, function(key, val) {
      if(!$.isArray(val)){
        scalarData[key] = [val];
      }
    });
    outputScalars       = scalarData.indices;
    outputScalarAliases = scalarData.aliases;
  });
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
  var colorPickerBinding = new Shiny.InputBinding();
  $.extend(colorPickerBinding, {
    find: function(scope) {
      return $(scope).find(".miro-color-picker");
    },
    getValue: function(el) {
      return $(el).val();
    },
    setValue: function(el, value) {
      $(el).setColor(value);
    },
    subscribe: function(el, callback) {
      $(el).on("change.colorPickerBinding", function(e) {
        callback(true);
      });
    },
    getRatePolicy: function() {
      return {
        policy: 'debounce',
        delay: 250
      };
    },
    initialize: function(el) {
      $(el).colorpicker({
        align: "left"
      });
    },
    unsubscribe: function(el) {
      $(el).colorpicker('destroy');
      $(el).off(".colorPickerBinding");
    }
  });
  
  Shiny.inputBindings.register(colorPickerBinding);
});