import "@babel/polyfill";
import InputArrayFactory from './input_array';

export { confirmModalShow } from './miro';

/* global $:false Shiny:false */

let lang = {};
let indices = [];
let indexAliases = [];
let scalarIndices = [];
let scalarIndexAliases = [];
let nonScalarIndices = [];
let nonScalarIndexAliases = [];
let outputScalars = [];
let outputScalarAliases = [];
let inputSymbols = [];
let inputSymbolsAliases = [];
let outputSymbols = [];
let outputSymbolsAliases = [];

const inputArrayFactory = new InputArrayFactory();


export function removeArrayEl(arrayID, elID) {
  inputArrayFactory.remove(arrayID, elID);
}
export function updateArrayEl(arrayID, elID, newVal) {
  inputArrayFactory.update(arrayID, elID, newVal);
}

export function toggleDepContainer(el, arrayID, elID, rAddID) {
  const depGroup = $(el).closest('.dep-group');
  depGroup.children('.dep-el').toggle();
  const activeEl = depGroup.children('.dep-el:visible').first().children('.form-group');
  let value;

  $.each(activeEl, (k, v) => {
    if ($(v).children('input[type="number"]').length) {
      value = $(v).children('input[type="number"]').val();
    } else if ($(v).children('.miro-color-picker').length) {
      value = $(v).children('.miro-color-picker').val();
    } else if ($(v).children('input[type="text"]').length) {
      value = $(v).children('input[type="text"]').val();
    } else if ($(v).find('input[type="checkbox"]').length) {
      value = $(v).find('input[type="checkbox"]').val();
    } else if ($(v).find('select').length) {
      [value] = $(v).find('select').get(0).selectize.items;
    }
  });
  if (rAddID.length) {
    Shiny.setInputValue(rAddID, [elID, value, arrayID, 'change'], { priority: 'event' });
  } else {
    Shiny.setInputValue(arrayID, [elID, value], { priority: 'event' });
  }
}
const arrayTypes = {
  symbol_inputGroups(defaults) {
    let name;
    let members;
    if (defaults !== undefined) {
      ({ name, members } = defaults);
    }
    const elements = {
      symbol_inputGroups: ['text', lang.addInputGroup.symbolInputgroups, name],
      group_memberIn: ['select', lang.addInputGroup.groupMemberIn, inputSymbols, inputSymbolsAliases, members, true],
    };
    return ([elements, { elRequired: false, myopicDefaults: true }, 'general']);
  },
  symbol_outputGroups(defaults) {
    let name;
    let members;
    if (defaults !== undefined) {
      ({ name, members } = defaults);
    }
    const elements = {
      symbol_outputGroups: ['text', lang.addOutputGroup.symbolOutputgroups, name],
      group_memberOut: ['select', lang.addOutputGroup.groupMemberOut, outputSymbols, outputSymbolsAliases, members, true],
    };
    return ([elements, { elRequired: false, myopicDefaults: true }, 'general']);
  },
  dy_dyEvent() {
    const elements = {
      dy_dyEvent: ['select', lang.addDyEvent.dyDyEvent, outputScalars, outputScalarAliases],
      dyEvent_label: ['text', lang.addDyEvent.label],
      dyEvent_labelLoc: ['select', lang.addDyEvent.labelLoc, ['top', 'bottom'], lang.addDyEvent.labelLocChoices],
      dyEvent_color: ['color', lang.addDyEvent.color, 'rgb(0,0,0)'],
      dyEvent_strokePattern: ['select', lang.addDyEvent.strokePattern, ['dashed', 'dotted', 'dotdash', 'solid'], lang.addDyEvent.strokePatternChoices],
    };
    return ([elements, { elRequired: false }]);
  },
  dy_dyLimit() {
    const elements = {
      dy_dyLimit: ['selectDep', [lang.addDyLimit.dyDyLimitCheck, 'numeric', lang.addDyLimit.dyDyLimitTrue, 0, 0], lang.addDyLimit.dyDyLimitFalse, outputScalars, outputScalarAliases],
      dyLimit_label: ['text', lang.addDyLimit.label],
      dyLimit_labelLoc: ['select', lang.addDyLimit.labelLoc, ['left', 'right'], lang.addDyLimit.labelLocChoices],
      dyLimit_color: ['color', lang.addDyLimit.color, 'rgb(0,0,0)'],
      dyLimit_strokePattern: ['select', lang.addDyLimit.strokePattern, ['dashed', 'dotted', 'dotdash', 'solid'], lang.addDyLimit.strokePatternChoices],
    };
    return ([elements, { elRequired: false }]);
  },
  dy_dyAnnotation() {
    const elements = {
      dy_dyAnnotation: ['select', lang.addDyAnnotation.dyDyAnnotation, outputScalars, outputScalarAliases],
      dyAnnotation_text: ['text', lang.addDyAnnotation.text, 'label'],
      dyAnnotation_tooltip: ['text', lang.addDyAnnotation.tooltip],
      dyAnnotation_width: ['numeric', lang.addDyAnnotation.width, 0, 0],
      dyAnnotation_height: ['numeric', lang.addDyAnnotation.height, 0, 0],
      dyAnnotation_attachAtBottom: ['checkbox', lang.addDyAnnotation.attachAtBottom],
    };
    return ([elements, { elRequired: false }]);
  },
  dy_dyShading() {
    const elements = {
      dy_dyShading: ['selectDep', [lang.addDyShading.dyDyShadingCheck, 'text', lang.addDyShading.dyDyShadingTrue], lang.addDyShading.dyDyShadingFalse, outputScalars, outputScalarAliases],
      dyShading_up: ['selectDep', [lang.addDyShading.dyDyShadingUpCheck, 'text', lang.addDyShading.dyDyShadingUpTrue], lang.addDyShading.dyDyShadingUpFalse, outputScalars, outputScalarAliases],
      dyShading_axis: ['select', lang.addDyShading.axis, ['x', 'y'], lang.addDyShading.axisChoices],
      dyShading_color: ['color', lang.addDyShading.color, '#EFEFEF'],
    };
    return ([elements, { elRequired: false }]);
  },
  leaflet_markers() {
    const elements = {
      leaflet_markers: ['select', lang.addLeafletMarkers.leafletMarkers, scalarIndices, scalarIndexAliases],
      leafMark_lng: ['select', lang.addLeafletMarkers.lng, scalarIndices, scalarIndexAliases],
      leafMark_groupName: ['text', lang.addLeafletMarkers.groupName],
      leafMark_label: ['text', lang.addLeafletMarkers.label],
      optionsStart: ['optionsStart', lang.addLeafletMarkers.options],
      leafMark_labelPermanent: ['checkbox', lang.addLeafletMarkers.labelPermanent, true],
      leafMark_labelcolor: ['color', lang.addLeafletMarkers.labelcolor],
      leafMark_labelbgcolor: ['color', lang.addLeafletMarkers.labelbgcolor],
      leafMark_labelsize: ['numeric', lang.addLeafletMarkers.labelsize, 12, 0],
      optionsEnd: ['optionsEnd'],
    };
    return ([elements, { elRequired: false }]);
  },
  leaflet_flows() {
    const elements = {
      leaflet_flows: ['select', lang.addLeafletFlows.leafletFlows, scalarIndices, scalarIndexAliases],
      leafFlow_lng: ['select', lang.addLeafletFlows.lng, scalarIndices, scalarIndexAliases],
      leafFlow_lat1: ['select', lang.addLeafletFlows.lat1, scalarIndices, scalarIndexAliases],
      leafFlow_lng1: ['select', lang.addLeafletFlows.lng1, scalarIndices, scalarIndexAliases],
      leafFlow_flow: ['select', lang.addLeafletFlows.flow, scalarIndices, scalarIndexAliases],
      leafFlow_time: ['select', lang.addLeafletFlows.time, ['_'].concat(nonScalarIndices), ['_'].concat(nonScalarIndexAliases)],
      leafFlow_label: ['text', lang.addLeafletFlows.label],
      optionsStart: ['optionsStart', lang.addLeafletFlows.options],
      leafFlow_color: ['color', lang.addLeafletFlows.color, '#0000ff'],
      leafFlow_minThickness: ['numeric', lang.addLeafletFlows.minThickness, 1, 0],
      leafFlow_maxThickness: ['numeric', lang.addLeafletFlows.maxThickness, 20, 0],
      optionsEnd: ['optionsEnd'],
    };
    return ([elements, { elRequired: false }]);
  },
  leaflet_minicharts() {
    const elements = {
      leaflet_minicharts: ['select', lang.addLeafletMinicharts.leafletMinicharts, scalarIndices, scalarIndexAliases],
      leafChart_lng: ['select', lang.addLeafletMinicharts.lng, scalarIndices, scalarIndexAliases],
      leafChart_chartdata: ['select', lang.addLeafletMinicharts.chartdata, scalarIndices, scalarIndexAliases, scalarIndices[0], true],
      leafChart_time: ['select', lang.addLeafletMinicharts.time, ['_'].concat(nonScalarIndices), ['_'].concat(nonScalarIndexAliases)],
      leafChart_type: ['select', lang.addLeafletMinicharts.type, ['bar', 'pie', 'polar-area', 'polar-radius', 'auto'], lang.addLeafletMinicharts.typeChoices],
      optionsStart: ['optionsStart', lang.addLeafletMinicharts.options],
      leafChart_width: ['numeric', lang.addLeafletMinicharts.width, 30, 0],
      leafChart_height: ['numeric', lang.addLeafletMinicharts.height, 30, 0],
      leafChart_opacity: ['numeric', lang.addLeafletMinicharts.opacity, 1, 0, 1, 0.1],
      leafChart_showlabels: ['checkbox', lang.addLeafletMinicharts.showlabels],
      leafChart_transitionTime: ['numeric', lang.addLeafletMinicharts.transitionTime, 750, 0],
      leafChart_layerId: ['select', lang.addLeafletMinicharts.layerId, ['_'].concat(nonScalarIndices), ['_'].concat(nonScalarIndexAliases)],
      leafChart_legend: ['checkbox', lang.addLeafletMinicharts.legend, true],
      leafChart_legendPosition: ['select', lang.addLeafletMinicharts.legendPosition, ['topright', 'topleft', 'bottomright', 'bottomleft'], lang.addLeafletMinicharts.legendPositionChoices],
      optionsEnd: ['optionsEnd'],
    };
    return ([elements, { elRequired: false }]);
  },
  chart_ydatabar() {
    const elements = {
      chart_ydata: ['select', lang.addBarDataEl.chartYdatabar, indices, indexAliases],
      chart_ylabel: ['text', lang.addBarDataEl.chartYlabel, 'label'],
      marker_color: ['color', lang.addBarDataEl.color],
      marker_line_width: ['numeric', lang.addBarDataEl.lineWidth, 0, 0],
      marker_line_color: ['color', lang.addBarDataEl.lineColor],
    };
    return ([elements]);
  },
  chart_ydatascatter() {
    const elements = {
      chart_ydata: ['select', lang.addScatterDataEl.chartYdata, scalarIndices, scalarIndexAliases],
      chart_ylabel: ['text', lang.addScatterDataEl.chartYlabel, 'label'],
      marker_symbol: ['select', lang.addScatterDataEl.symbol, ['circle', 'circle-open', 'circle-dot',
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
      marker_color: ['color', lang.addScatterDataEl.color],
      marker_size: ['numeric', lang.addScatterDataEl.size, 6, 0],
      marker_line_width: ['numeric', lang.addScatterDataEl.lineWidth, 0, 0],
      marker_line_color: ['color', lang.addScatterDataEl.lineColor],
      trace_legend: ['checkbox', lang.addScatterDataEl.legend],
      trace_frame: ['select', lang.addScatterDataEl.frame, ['_'].concat(indices), ['_'].concat(indexAliases)],
    };
    return ([elements]);
  },
  chart_ydataline() {
    const elements = {
      chart_ydata: ['select', lang.addLineDataEl.chartYdata, scalarIndices, scalarIndexAliases],
      chart_ylabel: ['text', lang.addLineDataEl.chartYlabel, 'label'],
      line_color: ['color', lang.addLineDataEl.color],
      line_width: ['numeric', lang.addLineDataEl.width, 2, 0],
      line_shape: ['select', lang.addLineDataEl.shape, ['linear', 'spline', 'hv', 'vh', 'hvh', 'vhv'], lang.addLineDataEl.shapeChoices],
      line_dash: ['select', lang.addLineDataEl.dash, ['solid', 'dot', 'dash', 'longdash', 'dashdot', 'longdashdot'], lang.addLineDataEl.dashChoices],
      line_fill: ['select', lang.addLineDataEl.fill, ['none', 'tozeroy', 'tozerox', 'tonexty', 'tonextx', 'toself', 'tonext'], lang.addLineDataEl.fillChoices],
      trace_legend: ['checkbox', lang.addLineDataEl.legend],
      trace_frame: ['select', lang.addLineDataEl.frame, ['_'].concat(indices), ['_'].concat(indexAliases)],
    };
    return ([elements]);
  },
  chart_ydatabubble() {
    const elements = {
      chart_ydata: ['select', lang.addBubbleDataEl.chartYdata, scalarIndices, scalarIndexAliases],
      chart_ylabel: ['text', lang.addBubbleDataEl.chartYlabel, 'label'],
      marker_symbol: ['select', lang.addBubbleDataEl.symbol, ['circle', 'circle-open', 'circle-dot',
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
      marker_colorDep: ['selectDep', [lang.addBubbleDataEl.colorCheck, 'color', lang.addBubbleDataEl.colorCheckTrue], lang.addBubbleDataEl.colorCheckFalse, scalarIndices, scalarIndexAliases, '.index'],
      marker_size: ['select', lang.addBubbleDataEl.size, scalarIndices, scalarIndexAliases, '.index'],
      marker_maxsize: ['numeric', lang.addBubbleDataEl.maxsize, 0, 0],
      marker_line_width: ['numeric', lang.addBubbleDataEl.lineWidth, 0, 0],
      marker_line_color: ['color', lang.addBubbleDataEl.lineColor],
      trace_legend: ['checkbox', lang.addBubbleDataEl.legend],
      trace_frame: ['select', lang.addBubbleDataEl.frame, ['_'].concat(indices), ['_'].concat(indexAliases)],
    };
    return ([elements]);
  },
  hist_xdata() {
    const elements = {
      hist_xdata: ['select', lang.addHistDataEl.histXdata, scalarIndices, scalarIndexAliases],
      hist_label: ['text', lang.addHistDataEl.label, 'label'],
      hist_color: ['color', lang.addHistDataEl.color, '#000000'],
    };
    return ([elements]);
  },
  dy_ydata() {
    const elements = {
      chart_ydata: ['select', lang.addDyDataEl.chartYdata, scalarIndices, scalarIndexAliases],
      chart_ylabel: ['text', lang.addDyDataEl.label, 'label'],
      dyser_color: ['color', lang.addDyDataEl.color],
      dyopt_stepPlot: ['checkbox', lang.addDyDataEl.stepPlot],
      dyopt_stemPlot: ['checkbox', lang.addDyDataEl.stemPlot],
      dyopt_fillGraph: ['checkbox', lang.addDyDataEl.fillGraph],
      dyopt_drawPoints: ['checkbox', lang.addDyDataEl.drawPoints],
      dyopt_pointShape: ['select', lang.addDyDataEl.pointShape, ['dot', 'triangle', 'square', 'diamond', 'pentagon', 'hexagon',
        'circle', 'star', 'plus', 'ex'], lang.addDyDataEl.pointShapeChoices],
      dyopt_pointSize: ['numeric', lang.addDyDataEl.pointSize, 2, 0],
    };
    return ([elements]);
  },
  timevis_series() {
    const elements = {
      timevis_series: ['select', lang.addTimevisDataEl.timevisSeries, indices, indexAliases],
      timedata_start: ['select', lang.addTimevisDataEl.start, indices, indexAliases],
      timedata_end: ['select', lang.addTimevisDataEl.end, ['_'].concat(indices), ['_'].concat(indexAliases)],
      timedata_type: ['select', lang.addTimevisDataEl.type, ['box', 'point', 'range', 'background'], lang.addTimevisDataEl.typeChoices],
      timedata_title: ['select', lang.addTimevisDataEl.title, ['_'].concat(indices), ['_'].concat(indexAliases)],
      timedata_group: ['select', lang.addTimevisDataEl.group, ['_'].concat(indices), ['_'].concat(indexAliases)],
      timedata_subgroup: ['select', lang.addTimevisDataEl.subgroup, ['_'].concat(indices), ['_'].concat(indexAliases)],
      timedata_grouptitle: ['select', lang.addTimevisDataEl.grouptitle, ['_'].concat(indices), ['_'].concat(indexAliases)],
      timedata_subgrouporder: ['select', lang.addTimevisDataEl.subgrouporder, ['_'].concat(indices), ['_'].concat(indexAliases)],
    };
    return ([elements]);
  },
  timevis_custom() {
    const elements = { timevis_custom: ['text', lang.addCustomTimeEl.timevisCustom, '', lang.addCustomTimeEl.placeholder] };
    return ([elements, { elRequired: false }]);
  },
};

export function addArrayDataEl(arrayID, defaultsRaw, reinitialize = false) {
  if ($(`#${arrayID}_wrapper .btn-add-array-el`).is(':disabled')) {
    return;
  }
  let defaults = defaultsRaw;
  if (defaults == null) {
    defaults = [undefined];
  }
  defaults.forEach((def) => {
    if (typeof (arrayTypes[arrayID]) === 'undefined') {
      throw new ReferenceError(`Array ID: ${arrayID} not defined.`);
    }
    const [elements, options, rObserveID] = arrayTypes[arrayID](def);
    inputArrayFactory.add(arrayID, elements, options, rObserveID, reinitialize);
  });
}
function addArrayDataElWrapper(arrayID, defaults, cnt = 1) {
  if ($(`#${arrayID}_wrapper`).is(':visible')) {
    addArrayDataEl(arrayID, defaults, true);
  } else if (cnt <= 6) {
    setTimeout(addArrayDataElWrapper, 200, arrayID, defaults, cnt + 1);
  }
}

$(document).ready(() => {
  Shiny.addCustomMessageHandler('gms-setScalarOutputs', (scalarDataRaw) => {
    const scalarData = scalarDataRaw;

    $.each(scalarData, (key, val) => {
      if (!$.isArray(val)) {
        scalarData[key] = [val];
      }
    });
    outputScalars = scalarData.indices;
    outputScalarAliases = scalarData.aliases;
  });
  Shiny.addCustomMessageHandler('gms-setIndices', (indicesFromRRaw) => {
    const indicesFromR = indicesFromRRaw;

    $.each(indicesFromR, (key, val) => {
      if (!$.isArray(val)) {
        indicesFromR[key] = [val];
      }
    });

    ({
      indices, aliases: indexAliases, scalarIndices, scalarAliases: scalarIndexAliases,
    } = indicesFromR);

    nonScalarIndices = indices.filter(index => scalarIndices.indexOf(index) === -1);
    nonScalarIndexAliases = indexAliases.filter(index => scalarIndexAliases.indexOf(index) === -1);
  });
  Shiny.addCustomMessageHandler('gms-setGAMSSymbols', (data) => {
    const symData = data.gamsSymbols;
    ({ lang } = data);
    $.each(symData, (key, val) => {
      if (!$.isArray(val)) {
        symData[key] = [val];
      }
    });

    ({
      inSym: inputSymbols,
      inAlias: inputSymbolsAliases,
      outSym: outputSymbols,
      outAlias: outputSymbolsAliases,
    } = symData);
  });
  Shiny.addCustomMessageHandler('gms-addArrayEl', (data) => {
    setTimeout(addArrayDataElWrapper, 200, data.arrayID, data.defaults);
  });
  const colorPickerBinding = new Shiny.InputBinding();
  $.extend(colorPickerBinding, {
    find(scope) {
      return $(scope).find('.miro-color-picker');
    },
    getValue(el) {
      return $(el).val();
    },
    setValue(el, value) {
      $(el).setColor(value);
    },
    subscribe(el, callback) {
      $(el).on('change.colorPickerBinding', () => {
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
      $(el).colorpicker({
        align: 'left',
      });
    },
    unsubscribe(el) {
      $(el).colorpicker('destroy');
      $(el).off('.colorPickerBinding');
    },
  });

  Shiny.inputBindings.register(colorPickerBinding);
});
