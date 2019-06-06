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
/* https://stackoverflow.com/questions/1187518/how-to-get-the-difference-between-two-arrays-in-javascript/11353526 */
function arr_diff (a1, a2) {
    var a = [], diff = [];
    for (var i = 0; i < a1.length; i++) {
        a[a1[i]] = true;
    }
    for (var i = 0; i < a2.length; i++) {
        if (a[a2[i]]) {
            delete a[a2[i]];
        } else {
            a[a2[i]] = true;
        }
    }
    for (var k in a) {
        diff.push(k);
    }
    return diff;
}

var lang = {};

var indices            = [];
var indexAliases       = [];
var scalarIndices      = [];
var scalarIndexAliases = [];
var nonScalarIndices   = [];
var nonScalarIndexAliases = [];
var outputScalars       = [];
var outputScalarAliases = [];
var inputSymbols        = [];
var inputSymbolsAliases = [];
var outputSymbols       = [];
var outputSymbolsAliases = [];
var freeElIDs          = {};
var elInArray          = {};

function addInputGroup(defaults){
  var name, members;
  if(defaults !== undefined){
    name = defaults.name;
    members = defaults.members;
  }
  console.log(members);
  var arrayID      = 'symbol_inputGroups';
  var elements     = {'symbol_inputGroups' : ['text', lang.addInputGroup.symbolInputgroups, name],
  'group_memberIn': ['select', lang.addInputGroup.groupMemberIn, inputSymbols, inputSymbolsAliases, members, true ]
  };
  addArrayEl(arrayID, elements, {elRequired: false}, 'general');
}
function addOutputGroup(defaults){
  var name, members;
  if(defaults !== undefined){
    name = defaults.name;
    members = defaults.members;
  }
  var arrayID      = 'symbol_outputGroups';
  var elements     = {'symbol_outputGroups' : ['text', lang.addOutputGroup.symbolOutputgroups, name],
  'group_memberOut': ['select', lang.addOutputGroup.groupMemberOut, outputSymbols, outputSymbolsAliases, members, true ]
  };
  addArrayEl(arrayID, elements, {elRequired: false}, 'general');
}
function addDyEvent(){
  var arrayID      = 'dy_dyEvent';
  var elements     = {'dy_dyEvent' : ['select', lang.addDyEvent.dyDyEvent, outputScalars, outputScalarAliases],
  'dyEvent_label': ['text', lang.addDyEvent.label],
  'dyEvent_labelLoc': ['select', lang.addDyEvent.labelLoc, ['top', 'bottom'], lang.addDyEvent.labelLocChoices],
  'dyEvent_color': ['color', lang.addDyEvent.color, 'rgb(0,0,0)'],
  'dyEvent_strokePattern': ['select', lang.addDyEvent.strokePattern, ['dashed', 'dotted', 'dotdash', 'solid'], lang.addDyEvent.strokePatternChoices]
  };
  addArrayEl(arrayID, elements, {elRequired: false});
}
function addDyLimit(){
  var arrayID      = 'dy_dyLimit';
  var elements     = {'dy_dyLimit' : ['selectDep', [lang.addDyEvent.dyDyLimitCheck, 'numeric', lang.addDyEvent.dyDyLimitTrue, 0, 0], lang.addDyEvent.dyDyLimitFalse, outputScalars, outputScalarAliases],
  'dyLimit_limit': ['numeric', lang.addDyEvent.limit, 0, 0],
  'dyLimit_label': ['text', lang.addDyEvent.label],
  'dyLimit_labelLoc': ['select', lang.addDyEvent.labelLoc, ['left', 'right'], lang.addDyEvent.labelLocChoices],
  'dyLimit_color': ['color', lang.addDyEvent.color, 'rgb(0,0,0)'],
  'dyLimit_strokePattern': ['select', lang.addDyEvent.strokePattern, ['dashed', 'dotted', 'dotdash', 'solid'], lang.addDyEvent.strokePatternChoices]
  };
  addArrayEl(arrayID, elements, {elRequired: false});
}
function addDyAnnotation(){
  var arrayID      = 'dy_dyAnnotation';
  var elements     = {'dy_dyAnnotation' : ['select', lang.addDyAnnotation.dyDyAnnotation, outputScalars, outputScalarAliases],
  'dyAnnotation_text': ['text', lang.addDyAnnotation.text, outputScalarAliases[1]],
  'dyAnnotation_tooltip': ['text', lang.addDyAnnotation.tooltip],
  'dyAnnotation_width': ['numeric', lang.addDyAnnotation.width, 0, 0],
  'dyAnnotation_height': ['numeric', lang.addDyAnnotation.height, 0, 0],
  'dyAnnotation_attachAtBottom': ['checkbox', lang.addDyAnnotation.attachAtBottom]
  };
  addArrayEl(arrayID, elements, {elRequired: false});
}

function addDyShading(){
  var arrayID      = 'dy_dyShading';
  var elements     = {'dy_dyShading' : ['select', lang.addDyShading.dyDyShading, outputScalars, outputScalarAliases],
  'dyShading_up' : ['select', lang.addDyShading.up, outputScalars, outputScalarAliases],
  'dyShading_axis': ['select', lang.addDyShading.axis, ['x', 'y'], lang.addDyShading.axisChoices],
  'dyShading_color': ['color', lang.addDyShading.color, '#EFEFEF']
  };
  addArrayEl(arrayID, elements, {elRequired: false});
}

function addLeafletMarkers(){
  var arrayID      = 'leaflet_markers';
  var elements     = {'leaflet_markers' : ['select', lang.addLeafletMarkers.leafletMarkers, scalarIndices, scalarIndexAliases],
  'leafMark_lng': ['select', lang.addLeafletMarkers.lng, scalarIndices, scalarIndexAliases],
  'leafMark_groupName': ['text', lang.addLeafletMarkers.groupName],
  'leafMark_label': ['text', lang.addLeafletMarkers.label],
  'optionsStart': ['optionsStart', lang.addLeafletMarkers.options],
  'leafMark_labelPermanent': ['checkbox', lang.addLeafletMarkers.labelPermanent, true],
  'leafMark_labelcolor': ['color', lang.addLeafletMarkers.labelcolor],
  'leafMark_labelbgcolor': ['color', lang.addLeafletMarkers.labelbgcolor],
  'leafMark_labelsize': ['numeric', lang.addLeafletMarkers.labelsize, 12, 0],
  'optionsEnd': ['optionsEnd']
  };
  addArrayEl(arrayID, elements, {elRequired: false});
}
function addLeafletFlows(){
  var arrayID      = 'leaflet_flows';
  var elements     = {'leaflet_flows' : ['select', lang.addLeafletFlows.leafletFlows, scalarIndices, scalarIndexAliases],
  'leafFlow_lng': ['select', lang.addLeafletFlows.lng, scalarIndices, scalarIndexAliases],
  'leafFlow_lat1': ['select', lang.addLeafletFlows.lat1, scalarIndices, scalarIndexAliases],
  'leafFlow_lng1': ['select', lang.addLeafletFlows.lng1, scalarIndices, scalarIndexAliases],
  'leafFlow_flow': ['select', lang.addLeafletFlows.flow, scalarIndices, scalarIndexAliases],
  'leafFlow_time': ['select', lang.addLeafletFlows.time, ['_'].concat(nonScalarIndices), ['_'].concat(nonScalarIndexAliases)],
  'leafFlow_label': ['text', lang.addLeafletFlows.label],
  'optionsStart': ['optionsStart', lang.addLeafletFlows.options],
  'leafFlow_color': ['color', lang.addLeafletFlows.color, '#0000ff'],
  'leafFlow_minThickness': ['numeric', lang.addLeafletFlows.minThickness, 1, 0],
  'leafFlow_maxThickness': ['numeric', lang.addLeafletFlows.maxThickness, 20, 0],
  'optionsEnd': ['optionsEnd']
  };
  addArrayEl(arrayID, elements, {elRequired: false});
}
function addLeafletMinicharts(){
  var arrayID      = 'leaflet_minicharts';
  var elements     = {'leaflet_minicharts' : ['select', lang.addLeafletMinicharts.leafletMinicharts, scalarIndices, scalarIndexAliases],
  'leafChart_lng': ['select', lang.addLeafletMinicharts.lng, scalarIndices, scalarIndexAliases],
  'leafChart_chartdata': ['select', lang.addLeafletMinicharts.chartdata, scalarIndices, scalarIndexAliases, scalarIndices[0], true],
  'leafChart_time': ['select', lang.addLeafletMinicharts.time, ['_'].concat(nonScalarIndices), ['_'].concat(nonScalarIndexAliases)],
  'leafChart_type': ['select', lang.addLeafletMinicharts.type, ['bar', 'pie', 'polar-area', 'polar-radius', 'auto'], lang.addLeafletMinicharts.typeChoices], 
  'optionsStart': ['optionsStart', lang.addLeafletMinicharts.options],
  'leafChart_width': ['numeric', lang.addLeafletMinicharts.width, 30, 0],
  'leafChart_height': ['numeric', lang.addLeafletMinicharts.height, 30, 0],
  'leafChart_opacity': ['numeric', lang.addLeafletMinicharts.opacity, 1, 0, 1, 0.1],
  'leafChart_showlabels': ['checkbox', lang.addLeafletMinicharts.showlabels],
  'leafChart_transitionTime': ['numeric', lang.addLeafletMinicharts.transitionTime, 750, 0],
  'leafChart_layerId': ['select', lang.addLeafletMinicharts.layerId, ['_'].concat(nonScalarIndices), ['_'].concat(nonScalarIndexAliases)],
  'leafChart_legend': ['checkbox', lang.addLeafletMinicharts.legend, true],
  'leafChart_legendPosition': ['select', lang.addLeafletMinicharts.legendPosition, ['topright', 'topleft', 'bottomright', 'bottomleft'], lang.addLeafletMinicharts.legendPositionChoices],
  'optionsEnd': ['optionsEnd']
  };
  addArrayEl(arrayID, elements, {elRequired: false});
}
function addBarDataEl(){
  var arrayID      = 'chart_ydatabar';
  var elements     = {'chart_ydata' : ['select', lang.addBarDataEl.chartYdatabar, indices, indexAliases], 
  'chart_ylabel' : ['text', lang.addBarDataEl.chartYlabel, 'label'],
  'marker_color' : ['color', lang.addBarDataEl.color, 'rgb(0,0,0)'],
  'marker_line_width': ['numeric', lang.addBarDataEl.lineWidth, 0, 0],
  'marker_line_color' : ['color', lang.addBarDataEl.lineColor]
  };
  
  addArrayEl(arrayID, elements);
}
function addScatterDataEl(){
  var arrayID      = 'chart_ydatascatter';
  var elements     = {'chart_ydata' : ['select', lang.addScatterDataEl.chartYdata, scalarIndices, scalarIndexAliases], 
  'chart_ylabel' : ['text', lang.addScatterDataEl.chartYlabel, 'label'],
  'marker_symbol': ['select', lang.addScatterDataEl.symbol, ['circle', 'circle-open', 'circle-dot', 
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
                                                        'star-triangle-down', 'star-triangle-down-open', 'star-triangle-down-dot', 
                                                        'star-triangle-down-open-dot',
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
                                                        'line-ns-open', 'line-ne', 'line-ne-open', 'line-nw', 'line-nw-open'], lang.addScatterDataEl.symbolChoices],
  'marker_color' : ['color', lang.addScatterDataEl.color, 'rgb(0,0,0)'],
  'marker_size' : ['numeric', lang.addScatterDataEl.size, 6, 0],
  'marker_line_width': ['numeric', lang.addScatterDataEl.lineWidth, 0, 0],
  'marker_line_color' : ['color', lang.addScatterDataEl.lineColor],
  'trace_legend' : ['checkbox', lang.addScatterDataEl.legend],
  'trace_frame' : ['select', lang.addScatterDataEl.frame, ['_'].concat(indices), ['_'].concat(indexAliases)]
  };
  addArrayEl(arrayID, elements);
}
function addLineDataEl(){
  var arrayID      = 'chart_ydataline';
  var elements     = {'chart_ydata' : ['select', lang.addLineDataEl.chartYdata, scalarIndices, scalarIndexAliases], 
  'chart_ylabel' : ['text', lang.addLineDataEl.chartYlabel, 'label'],
  'line_color' : ['color', lang.addLineDataEl.color, 'rgb(0,0,0)'],
  'line_width' : ['numeric', lang.addLineDataEl.width, 2, 0],
  'line_shape' : ['select', lang.addLineDataEl.shape, ['linear', 'spline', 'hv', 'vh', 'hvh', 'vhv'], lang.addLineDataEl.shapeChoices],
  'line_dash' : ['select', lang.addLineDataEl.dash, ['solid', 'dot', 'dash', 'longdash', 'dashdot', 'longdashdot'], lang.addLineDataEl.dashChoices],
  'line_fill' : ['select', lang.addLineDataEl.fill,['none', 'tozeroy', 'tozerox', 'tonexty', 'tonextx', 'toself', 'tonext'], lang.addLineDataEl.fillChoices],
  'trace_legend' : ['checkbox', lang.addLineDataEl.legend],
  'trace_frame' : ['select', lang.addLineDataEl.frame, ['_'].concat(indices), ['_'].concat(indexAliases)]
  };
  addArrayEl(arrayID, elements);
}
function addBubbleDataEl(){
  var arrayID      = 'chart_ydatabubble';
  var elements     = {'chart_ydata' : ['select', lang.addBubbleDataEl.chartYdata,  scalarIndices, scalarIndexAliases], 
  'chart_ylabel' : ['text', lang.addBubbleDataEl.chartYlabel, 'label'],
  'marker_symbol': ['select', lang.addBubbleDataEl.symbol, ['circle', 'circle-open', 'circle-dot', 
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
                                                        'star-triangle-down', 'star-triangle-down-open', 'star-triangle-down-dot', 
                                                        'star-triangle-down-open-dot',
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
                                                        'line-ns-open', 'line-ne', 'line-ne-open', 'line-nw', 'line-nw-open'], lang.addBubbleDataEl.symbolChoices],
  'marker_color' : ['color', lang.addBubbleDataEl.color, 'rgb(0,0,0)'],
  'marker_size' : ['select', lang.addBubbleDataEl.size, scalarIndices, scalarIndexAliases],
  'marker_maxsize': ['numeric', lang.addBubbleDataEl.maxsize, 0, 0],
  'marker_line_width': ['numeric', lang.addBubbleDataEl.lineWidth, 0, 0],
  'marker_line_color' : ['color', lang.addBubbleDataEl.lineColor],
  'trace_legend' : ['checkbox', lang.addBubbleDataEl.legend],
  'trace_frame' : ['select', lang.addBubbleDataEl.frame, ['_'].concat(indices), ['_'].concat(indexAliases)]
  };
  addArrayEl(arrayID, elements);
}
function addHistDataEl(){
  var arrayID      = 'hist_xdata';
  var elements     = {'hist_xdata' : ['select', lang.addHistDataEl.histXdata, scalarIndices, scalarIndexAliases], 
  'hist_label' : ['text', lang.addHistDataEl.label, 'label'],
  'hist_color' : ['color', lang.addHistDataEl.color, '#000000']
  };
  addArrayEl(arrayID, elements);
}
function addDyDataEl(){
  var arrayID      = 'dy_ydata';
  var elements     = {'chart_ydata' : ['select', lang.addDyDataEl.chartYdata, scalarIndices, scalarIndexAliases], 
  'dyser_color' : ['color', lang.addDyDataEl.color],
  'dyopt_stepPlot' : ['checkbox', lang.addDyDataEl.stepPlot],
  'dyopt_stemPlot' : ['checkbox', lang.addDyDataEl.stemPlot],
  'dyopt_fillGraph' : ['checkbox', lang.addDyDataEl.fillGraph],
  'dyopt_drawPoints' : ['checkbox', lang.addDyDataEl.drawPoints],
  'dyopt_pointShape' : ['select', lang.addDyDataEl.pointShape,['dot', 'triangle', 'square', 'diamond', 'pentagon', 'hexagon', 
                              'circle', 'star', 'plus', 'ex'], lang.addDyDataEl.pointShapeChoices],
  'dyopt_pointSize' : ['numeric', lang.addDyDataEl.pointSize, 2, 0]
  };
  addArrayEl(arrayID, elements);
}
function addTimevisDataEl(){
  var arrayID      = 'timevis_series';
  var elements     = {'timevis_series' : ['select', lang.addTimevisDataEl.timevisSeries, indices, indexAliases],
  'timedata_start' : ['select', lang.addTimevisDataEl.start, indices, indexAliases],
  'timedata_end' : ['select', lang.addTimevisDataEl.end, ['_'].concat(indices), ['_'].concat(indexAliases)],
  'timedata_type' : ['select', lang.addTimevisDataEl.type, ['box', 'point', 'range', 'background'], lang.addTimevisDataEl.typeChoices],
  'timedata_title' : ['select', lang.addTimevisDataEl.title, ['_'].concat(indices), ['_'].concat(indexAliases)],
  'timedata_group' : ['select', lang.addTimevisDataEl.group, ['_'].concat(indices), ['_'].concat(indexAliases)],
  'timedata_subgroup' : ['select', lang.addTimevisDataEl.subgroup, ['_'].concat(indices), ['_'].concat(indexAliases)],
  'timedata_grouptitle' : ['select', lang.addTimevisDataEl.grouptitle, ['_'].concat(indices), ['_'].concat(indexAliases)],
  'timedata_subgrouporder' : ['select', lang.addTimevisDataEl.subgrouporder, ['_'].concat(indices), ['_'].concat(indexAliases)]
  };
  addArrayEl(arrayID, elements);
}
function addCustomTimeEl(){
  var arrayID      = 'timevis_custom';
  var elements     = {'timevis_custom' : ['text', lang.addCustomTimeEl.timevisCustom, "", lang.addCustomTimeEl.placeholder]
  };
  addArrayEl(arrayID, elements, {elRequired: false});
}
function toggleDepContainer(el, arrayID, elID, rAddID){
  var depGroup = $(el).closest('.dep-group');
  depGroup.children('.dep-el').toggle();
  var activeEl = depGroup.children('.dep-el:visible').first().children('.form-group');
  var value = undefined;
  
  $.each(activeEl, function(k,v){
    if($(v).children('input[type="number"]').length){
      value = $(v).children('input[type="number"]').val();
    }else if($(v).children('.miro-color-picker').length){
      value = $(v).children('.miro-color-picker').val();
    }else if($(v).children('input[type="text"]').length){
      value = $(v).children('input[type="text"]').val();
    }else if($(v).find('input[type="checkbox"]').length){
      value = $(v).find('input[type="checkbox"]').val();
    }else if($(v).find('select').length){
      value  = $(v).find('select').get(0).selectize.items[0];
    }
  });
  Shiny.setInputValue(rAddID, [elID, value, arrayID, "change"], {priority: "event"});
}
function addArrayDataEl(arrayID, defaults){
  if($('#' + arrayID + '_wrapper .btn-add-array-el').is(':disabled')){
    return;
  }
  if(defaults === undefined){
    defaults = [undefined];
  }
  $.each(defaults, function( i, l ) {
    switch(arrayID){
      case 'symbol_inputGroups':
        addInputGroup(l);
      break;
      case 'symbol_outputGroups':
        addOutputGroup(l);
      break;
      case 'chart_ydatabar':
        addBarDataEl(l);
      break;
      case 'chart_ydatascatter':
        addScatterDataEl(l);
      break;
      case 'chart_ydataline':
        addLineDataEl(l);
      break;
      case 'chart_ydatabubble':
        addBubbleDataEl(l);
      break;
      case 'hist_xdata':
        addHistDataEl(l);
      break;
      case 'dy_ydata':
        addDyDataEl(l);
      break;
      case 'dy_dyEvent':
        addDyEvent(l);
      break;
      case 'dy_dyLimit':
        addDyLimit(l);
      break;
      case 'dy_dyAnnotation':
        addDyAnnotation(l);
      break;
      case 'dy_dyShading':
        addDyShading(l);
      break;
      case 'leaflet_markers':
        addLeafletMarkers(l);
      break;
      case 'leaflet_flows':
        addLeafletFlows(l);
      break;
      case 'leaflet_minicharts':
        addLeafletMinicharts(l);
      break;
      case 'timevis_series':
        addTimevisDataEl(l);
      break;
      case 'timevis_custom':
        addCustomTimeEl(l);
      break;
    }
  });
}
function addArrayDataElWrapper(arrayID, defaults){
  if($('#' + arrayID + '_wrapper').is(':visible')){
    addArrayDataEl(arrayID, defaults);
  }else{
    setTimeout(addArrayDataElWrapper, 200, arrayID, defaults);
  }
}

function incElCount(arrayID){
  var count = $('#' + arrayID + '_wrapper .array-wrapper').children('.config-array-el').length;
  if(count === 0){
    delete elInArray[arrayID];
  }
  if(freeElIDs[arrayID] !== undefined && freeElIDs[arrayID].length){
    return(freeElIDs[arrayID].pop());
  }
  return(count + 1);
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

function registerChangeHandlers(elements, rAddID, elID, options){
  var idx = 0;
  var htmlID = arguments.length > 4 && arguments[4] !== undefined ? arguments[4] : elID;
  
  $.each(elements, function( k, v ) {
    if(v[0] === 'select' || v[0] === 'selectDep'){
      if(idx === 0){
        $('#' + k + htmlID).selectize({
      	  onChange: function(value) {
      	    if(options.updateTxtWithLabel.length){
      	      var alias = outputScalarAliases[outputScalars.findIndex(function(val){
                            return val === value;
                          })];
      	      for(var i = 0; i < options.updateTxtWithLabel.length; i++){
        	      $(options.updateTxtWithLabel[i]).val(alias);
        	    }
      	    }
      	    
      	    Shiny.setInputValue(rAddID, [elID, value, k, "change"], {priority: "event"});
          }
      	});
      }else{
        $('#' + k + htmlID).selectize({
      	  onChange: function(value) {
      	    Shiny.setInputValue(k, [elID, value], {priority: "event"});
          }
      	});
      }
      if(v[0] === 'selectDep'){
    	  var alt_elements = {};
    	  alt_elements[k] = v[2];
    	  
    	  registerChangeHandlers(alt_elements, rAddID, elID, options, '_alt' + elID);
    	}
    }else if(v[0] === 'checkbox'){
      if(idx === 0){
        $('#' + k + htmlID).on('change', function(){
          Shiny.setInputValue(rAddID, [elID, $(this).prop('checked'), k, "change"], {priority: "event"});
        });
      }else{
        $('#' + k + htmlID).on('change', function(){
          Shiny.setInputValue(k, [elID, $(this).prop('checked')], {priority: "event"});
        });
      }
    }else if(v[0] === 'color'){
      $('#' + k + htmlID).colorpicker({
        align: "left"
      });
      if(idx === 0){
        $('#' + k + htmlID).on('change', $.debounce(500, function(){
          Shiny.setInputValue(rAddID, [elID, $(this).val(), k, "change"], {priority: "event"});
        }));
      }else{
        $('#' + k + htmlID).on('change', $.debounce(500, function(){
          Shiny.setInputValue(k, [elID, $(this).val()], {priority: "event"});
        }));
      }
    }else{
      if(idx === 0){
        $('#' + k + htmlID).on('change', $.debounce(250, function(){
          Shiny.setInputValue(rAddID, [elID, $(this).val(), k, "change"], {priority: "event"});
        }));
      }else{
        $('#' + k + htmlID).on('change', $.debounce(250, function(){
          Shiny.setInputValue(k, [elID, $(this).val()], {priority: "event"});
        }));
      }
    }
    idx++;
  });
}

function addArrayEl(arrayID, elements, options){
  if(options === undefined) options = {};
  if(options.elRequired === undefined) options.elRequired = true;
  if(options.updateTxtWithLabel === undefined) options.updateTxtWithLabel = [];
  var rObserveID = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : 'array_el';
  var rAddID     = 'add_' + rObserveID;
  
  var elID         = incElCount(arrayID);
  var label        = '';
  var arrayContent = '<div id="' + arrayID + elID + '_wrapper" class="config-array-el">';
  var idx          = 0;
  
  $.each(elements, function( k, v ) {
    switch(v[0]){
      case 'select':
        if(idx === 0){
          var selected = undefined;
          if(options.elRequired === true){
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
          }else{
            selected = v[2][0];
            label    = v[3][0];
          }
          
          // tell R that new element was addded to array: (ID, label of element)
          Shiny.setInputValue(rAddID, [elID, selected, k], {priority: "event"});
        }else{
          var selected = v[4];
        }
        arrayContent += createSelectInput(k, elID, v[1], v[2], v[3], selected, v[5]);
      break;
      case 'selectDep':
        var selected = v[3][0];
        if(v[5] !== undefined){
          selected = v[5];
        }
        if(idx === 0){
          // tell R that new element was addded to array: (ID)
          Shiny.setInputValue(rAddID, [elID, selected, k], {priority: "event"});
        }
        arrayContent += createSelectDepInput(k, elID, rAddID, v[1], v[2], v[3], v[4], selected, v[6]);
      break;
      case 'text':
        var value = v[2]; 
        if(idx === 0 && options.elRequired === false){
          // tell R that new element was addded to array: (ID)
          Shiny.setInputValue(rAddID, [elID, value, k], {priority: "event"});
        }
        if(value === 'label'){
          value = label;
          options.updateTxtWithLabel.push('#' + k + elID);
        }
        arrayContent += createTextInput(k, elID, v[1], value, v[3]);
      break;
      case 'numeric':
        if(idx === 0 && options.elRequired === false){
          selected = v[2];
          
          // tell R that new element was addded to array: (ID)
          Shiny.setInputValue(rAddID, [elID, selected, k], {priority: "event"});
        }
        arrayContent += createNumericInput(k, elID, v[1], v[2], v[3], v[4], v[5]);
      break;
      case 'checkbox':
        if(idx === 0 && options.elRequired === false){
          // tell R that new element was addded to array: (ID)
          Shiny.setInputValue(rAddID, [elID, v[2], k], {priority: "event"});
        }
        arrayContent += createCheckboxInput(k, elID, v[1], v[2]);
        break;
      case 'color':
        arrayContent += createColorPickerInput(k, elID, v[1], v[2]);
        break;
      case 'optionsStart':
        arrayContent += '<div class="form-group" style="min-height:30px;">' + 
        '<h4 class="box-title option-section-header" style="cursor:pointer;font-weight:bold;" onclick="$(this).next().toggle();">' + 
        v[1] + ' <i class="fa fa-plus"></i></h4><div class="option-section"' + ((v[2] === false)? '': ' style="display:none;"') + '>';
        break;
      case 'optionsEnd':
        arrayContent += '</div></div>';
      break;
      default:
        throw "Unknown element type: " + v[0];
    }
    idx++;
  });
  arrayContent += ((!options.elRequired || elID > 1)? '<button type="button" onclick="removeArrayEl(\'' + arrayID + '\',\'' + elID + '\',\'' + rObserveID +
  '\')" class="btn btn-default bt-icon"><i class="far fa-minus-square"></i></button>\n': '') + '<hr></div>';
  
  $('#' + arrayID + '_wrapper .array-wrapper').append(arrayContent);
  
  registerChangeHandlers(elements, rAddID, elID, options);
}

function createSelectInput(arrayID, elID, label, choices){
  var id = arrayID + elID;
  var aliases  = arguments.length > 4 && arguments[4] !== undefined ? arguments[4] : choices;
  var selected = arguments.length > 5 && arguments[5] !== undefined ? arguments[5] : '';
  var multiple = arguments.length > 6 && arguments[6] !== undefined ? arguments[6] : false;
  if(!$.isArray(selected)){
    selected = [selected];
  }
  optionsHTML  = '';
  for (var i = 0, itemLen = choices.length; i < itemLen; 
  optionsHTML += '<option value="' + choices[i] + '"' + ($.inArray(choices[i], selected) !== -1 ? ' selected' : '') + 
  '>' + aliases[i++] + '</option>\n');
  
  return('<div class="form-group">\n' +
  '<label class="control-label" for="' +
  id + '">' + label + '</label>\n' +
  '<div><select id="' + id + '"' + (multiple === true? ' multiple="multiple"' : '') + '>' + optionsHTML + '</select>\n</div>\n</div>');
}
function createSelectDepInput(arrayID, elID, rAddID, altEl, label, choices){
  var id = arrayID + elID;
  
  var aliases  = arguments.length > 6 && arguments[6] !== undefined ? arguments[6] : choices;
  var selected = arguments.length > 7 && arguments[7] !== undefined ? arguments[7] : '';
  
  var firstInput = createSelectInput(arrayID, elID, label, choices, aliases, selected);
  switch(altEl[1]){
    case 'select':
      var secondInput = createSelectInput(arrayID + '_alt', elID, altEl[2], 
      altEl[3], altEl[4], altEl[5]);
    break;
    case 'text':
      var secondInput = createTextInput(arrayID + '_alt', elID, altEl[2], 
      altEl[3]);
    break;
    case 'numeric':
      var secondInput = createNumericInput(arrayID + '_alt', elID, altEl[2], 
      altEl[3], altEl[4], altEl[5]);
    break;
    case 'checkbox':
      var secondInput = createCheckboxInput(arrayID + '_alt', elID, altEl[2], 
      altEl[3]);
      break;
    case 'color':
      var secondInput = createColorPickerInput(arrayID + '_alt', elID, altEl[2], 
      altEl[3]);
      break;
    default:
      throw "Unknown element type: " + altEl[1];
  }
  var checkboxInput = '<div class="form-group col-sm-4">\n' +
  '<label class="cb-label" for="depCb_' + arrayID + elID + '">' + altEl[0] + '</label>\n' +
  '<div>\n<label class="checkbox-material" for="depCb_' + arrayID + elID + 
  '">\n<div class="checkbox">\n<label>\n' +
      '<input id="depCb_' + arrayID + elID + '" type="checkbox" onclick="toggleDepContainer(this, \'' 
      + arrayID + '\', \'' + elID + '\', \'' + rAddID + '\');"/>\n' +
      '<span></span>\n' +
    '</label>\n' +
  '</div>\n' +
'</div>\n</label>\n</div>'
  
  return('<div class="form-group dep-group" style="width:100%;display:inline-block;">\n<div class="dep-el col-sm-8">\n' +
  firstInput + '\n</div>\n<div class="dep-el col-sm-8" style="display:none">\n' + secondInput + 
  '\n</div>\n<div>\n'+ checkboxInput + '\n</div>\n</div>');
}
function createTextInput(arrayID, elID, label){
  var id = arrayID + elID;
  var value = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : '';
  var placeholder = arguments.length > 4 && arguments[4] !== undefined ? arguments[4] : null;
  Shiny.setInputValue(arrayID, [elID, value], {priority: "event"});
  
  return('<div class="form-group">\n' +
  '<label for="' + id + '">' + label + '</label>\n' +
  '<input id="' + id + '" type="text" class="form-control" value="' + value + '"' + (placeholder !== null ? ' placeholder="' + placeholder + '"': '') + '/>\n' +
'</div>');
}
function createNumericInput(arrayID, elID, label, value){
  var id = arrayID + elID;
  var min   = arguments.length > 4 && arguments[4] !== undefined ? arguments[4] : null;
  var max   = arguments.length > 5 && arguments[5] !== undefined ? arguments[5] : null;
  var step  = arguments.length > 6 && arguments[6] !== undefined ? arguments[6] : null;
  
  Shiny.setInputValue(arrayID, [elID, value], {priority: "event"});
  
  return('<div class="form-group">\n' +
  '<label for="' + id + '">' + label + '</label>\n' +
  '<input id="' + id + '" type="number" class="form-control" value="' + value + '"' + 
  (min !== null ? ' min="' + min + '"': '') + (max !== null ? ' max="' + max + '"': '') + 
  (step !== null ? ' step="' + step + '"': '') + '/>\n' +
'</div>');
}
function createCheckboxInput(arrayID, elID, label){
  var id = arrayID + elID;
  var value = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : false;
  Shiny.setInputValue(arrayID, [elID, value], {priority: "event"});
  
  return('<div class="form-group">\n' +
  '<label class="cb-label" for="' + id + '">' + label + '</label>\n' +
  '<div>\n<label class="checkbox-material" for="' + id + '">\n<div class="checkbox">\n' +
    '<label>\n' +
      '<input id="' + id + '" type="checkbox"' + (value === true? ' checked="checked"': '') + '/>\n' +
      '<span></span>\n' +
    '</label>\n' +
  '</div>\n' +
'</div>\n</label>\n</div>');
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
function removeElAtomic(element, idx, elID){
  var el = null;
  var arrayLabel = null;
  
  $.each(element, function(k,v){
    if($(v).children('input[type="number"]').length){
      el = $(v).children('input[type="number"]');
      if(idx === 0){
        arrayLabel = elID;
      }
      el.remove();
    }else if($(v).children('.miro-color-picker').length){
      el = $(v).children('.miro-color-picker');
      if(idx === 0){
        arrayLabel = elID;
      }
      el.colorpicker('destroy');
    }else if($(v).children('.option-section').length){
      removeElAtomic($(v).children('.option-section').first().children(".form-group"), idx, elID);
    }else if($(v).children('input[type="text"]').length){
      el = $(v).children('input[type="text"]');
      if(idx === 0){
        arrayLabel = elID;
      }
      el.remove();
    }else if($(v).children('.dep-el').length){
      var depEl = $(v).children('.dep-el');
      if(idx === 0){
        arrayLabel = elID;
      }
      removeElAtomic(depEl.eq(0).children(".form-group"), idx, elID);
      removeElAtomic(depEl.eq(1).children(".form-group"), idx, elID);
    }else if($(v).find('input[type="checkbox"]').length){
      el = $(v).find('input[type="checkbox"]');
      if(idx === 0){
        arrayLabel = elID;
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
  return arrayLabel;
}
function removeArrayEl(arrayID, elID, rObserveID){
  
  
  var idx   = 0;
  var arrayLabel = removeElAtomic($('#' + arrayID + elID + '_wrapper').children(".form-group"), idx, elID);
  
  if(arrayLabel.length === 0){
    throw "Failed to remove array element: Could not find identifier for array element: " +
    arrayID + ' (label ID: ' + arrayID + elID + ').';
  }
  
  if(elInArray[arrayID] !== undefined){
    elInArray[arrayID].splice($.inArray(arrayLabel, elInArray[arrayID]), 1);
  }
  $('#' + arrayID + '_wrapper .btn-add-array-el:disabled').prop('disabled', false);
  Shiny.setInputValue('remove_' + rObserveID, [arrayID, arrayLabel, elID], {
    priority: "event"
  });
  if(freeElIDs[arrayID] === undefined){
    freeElIDs[arrayID] = [];
  }
  freeElIDs[arrayID].push(elID);
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
    nonScalarIndices = arr_diff(indices, scalarIndices);
    nonScalarIndexAliases = arr_diff(indexAliases, scalarIndexAliases);
  });
  Shiny.addCustomMessageHandler('gms-setGAMSSymbols', function (data) {
    var symData = data.gamsSymbols;
    lang = data.lang;
    $.each(symData, function(key, val) {
      if(!$.isArray(val)){
        symData[key] = [val];
      }
    });
    inputSymbols         = symData.inSym;
    inputSymbolsAliases  = symData.inAlias;
    outputSymbols        = symData.outSym;
    outputSymbolsAliases = symData.outAlias;
  });
  Shiny.addCustomMessageHandler('gms-addArrayEl', function(data){
    setTimeout(addArrayDataElWrapper, 200, data.arrayID, data.defaults);
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
