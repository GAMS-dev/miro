import InputArrayFactory from './input_array';

export { confirmModalShow, slideToggleEl } from './miro';

/* global $:false Shiny:false showdown:false renderMathInElement:false */


const converter = new showdown.Converter({
  tables: true,
  tasklists: true,
  strikethrough: true,
  noHeaderId: true,
  openLinksInNewWindow: true,
});
const converterMath = new showdown.Converter({
  tables: true,
  tasklists: true,
  strikethrough: true,
  noHeaderId: true,
  openLinksInNewWindow: true,
  extensions: ['mathjax'],
});
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
    let sameTab = false;
    if (defaults !== undefined) {
      if (defaults.sameTab !== undefined) {
        ({ sameTab } = defaults);
      }
      ({ name, members } = defaults);
    }
    const elements = {
      symbol_inputGroups: ['text', lang.addInputGroup.symbolInputgroups, name],
      group_memberIn: ['select', lang.addInputGroup.groupMemberIn, inputSymbols, inputSymbolsAliases, members, true],
      group_sameTabIn: ['checkbox', lang.addInputGroup.groupSameTabIn, sameTab],
    };
    return ([elements, { elRequired: false }, 'general']);
  },
  symbol_outputGroups(defaults) {
    let name;
    let members;
    let sameTab = false;
    if (defaults !== undefined) {
      if (defaults.sameTab !== undefined) {
        ({ sameTab } = defaults);
      }
      ({ name, members } = defaults);
    }
    const elements = {
      symbol_outputGroups: ['text', lang.addOutputGroup.symbolOutputgroups, name],
      group_memberOut: ['select', lang.addOutputGroup.groupMemberOut, outputSymbols, outputSymbolsAliases, members, true],
      group_sameTabOut: ['checkbox', lang.addOutputGroup.groupSameTabOut, sameTab],
    };
    return ([elements, { elRequired: false }, 'general']);
  },
  symbol_links(defaults) {
    let source;
    let target;

    if (defaults !== undefined) {
      ({ source, target } = defaults);
    }
    const scalarOutputSym = outputSymbols.map((sym) => ['_scalars_out', '_scalarsve_out'].find((sSym) => sym === sSym));
    const scalarInputSym = inputSymbols.map((sym) => ['_scalars', '_scalarsve'].find((sSym) => sym === sSym));

    const elements = {
      symbol_links: ['select', lang.addSymlink.source,
        outputSymbols.filter((sym, id) => scalarOutputSym[id] === undefined),
        outputSymbolsAliases.filter((sym, id) => scalarOutputSym[id] === undefined), source],
      symlink_target: ['select', lang.addSymlink.target,
        inputSymbols.filter((sym, id) => scalarInputSym[id] === undefined),
        inputSymbolsAliases.filter((sym, id) => scalarInputSym[id] === undefined), target],
    };
    return ([elements, { uniqueItems: true }, 'symlink']);
  },
  scripts_base(defaults) {
    let id; let tabTitle; let command; let args; let outputFile; let
      timeout;
    if (defaults !== undefined) {
      ({
        id, tabTitle, command, args, outputFile, timeout,
      } = defaults);
    }
    const elements = {
      scripts_base: ['text', lang.addScript.id, id],
      scriptsB_title: ['text', lang.addScript.title, tabTitle],
      scriptsB_cmd: ['text', lang.addScript.cmd, command],
      scriptsB_args: ['select', lang.addScript.args, args, args, args, true, true],
      scriptsB_outFile: ['text', lang.addScript.outFile, outputFile],
      scriptsB_timeout: ['numeric', lang.addScript.timeout, timeout, -1, Infinity, 1],
    };
    return ([elements, { elRequired: false }, 'script']);
  },
  scripts_hcube(defaults) {
    let id; let title; let command; let args; let outputFile; let
      timeout;
    if (defaults !== undefined) {
      ({
        id, title, command, args, outputFile, timeout,
      } = defaults);
    }
    const elements = {
      scripts_hcube: ['text', lang.addScript.id, id],
      scriptsH_title: ['text', lang.addScript.title, title],
      scriptsH_cmd: ['text', lang.addScript.cmd, command],
      scriptsH_args: ['select', lang.addScript.args, args, args, args, true, true],
      scriptsH_outFile: ['text', lang.addScript.outFile, outputFile],
      scriptsH_timeout: ['numeric', lang.addScript.timeout, timeout, -1, Infinity, 1],
    };
    return ([elements, { elRequired: false }, 'script']);
  },
  general_output_attach(defaults) {
    let filename; let execPerm; let throwError;
    if (defaults !== undefined) {
      ({
        filename, execPerm, throwError,
      } = defaults);
    }
    const elements = {
      general_output_attach: ['text', lang.addOutAttach.file, filename],
      outAttach_exec: ['checkbox', lang.addOutAttach.exec, execPerm === true],
      outAttach_error: ['checkbox', lang.addOutAttach.error, throwError !== false],
    };
    return ([elements, { elRequired: false }, 'attach']);
  },
  dy_ydata(defaults) {
    let key; let label; let yaxis; let color;
    let stepPlot; let stemPlot; let fillGraph;
    let drawPoints; let pointShape; let pointSize;
    if (defaults != null) {
      [key, {
        label, yaxis, color, stepPlot, stemPlot, fillGraph,
        drawPoints, pointShape, pointSize,
      }] = defaults;
    }
    const elements = {
      chart_ydata: ['select', lang.addDyDataEl.chartYdata, scalarIndices, scalarIndexAliases, key],
      chart_ylabel: ['text', lang.addDyDataEl.label, label == null ? 'label' : label],
      dyser_yaxis: ['select', lang.addDyDataEl.yaxis, ['y', 'y2'], lang.addDyDataEl.yaxisChoices, yaxis],
      dyser_color: ['color', lang.addDyDataEl.color, color == null ? '' : color],
      dyopt_stepPlot: ['checkbox', lang.addDyDataEl.stepPlot, stepPlot],
      dyopt_stemPlot: ['checkbox', lang.addDyDataEl.stemPlot, stemPlot],
      dyopt_fillGraph: ['checkbox', lang.addDyDataEl.fillGraph, fillGraph],
      dyopt_drawPoints: ['checkbox', lang.addDyDataEl.drawPoints, drawPoints],
      dyopt_pointShape: ['select', lang.addDyDataEl.pointShape, ['dot', 'triangle', 'square', 'diamond', 'pentagon', 'hexagon',
        'circle', 'star', 'plus', 'ex'], lang.addDyDataEl.pointShapeChoices, pointShape],
      dyopt_pointSize: ['numeric', lang.addDyDataEl.pointSize, pointSize == null ? 2 : pointSize, 0],
    };
    return ([elements]);
  },
  dy_dyEvent(defaults) {
    let key; let staticEvent = false; let label; let
      labelLoc; let color; let strokePattern;
    if (defaults != null) {
      [key, {
        staticEvent, label, labelLoc, color, strokePattern,
      }] = defaults;
      staticEvent = staticEvent === true;
    }
    const elements = {
      dy_dyEvent: ['selectDep', [lang.addDyEvent.dyDyEventCheck, 'text', lang.addDyEvent.dyDyEventTrue, staticEvent ? key : undefined], lang.addDyEvent.dyDyEventFalse, outputScalars, outputScalarAliases, staticEvent ? undefined : key, staticEvent],
      dyEvent_label: ['text', lang.addDyEvent.label, label == null ? '' : label],
      dyEvent_labelLoc: ['select', lang.addDyEvent.labelLoc, ['top', 'bottom'], lang.addDyEvent.labelLocChoices, labelLoc],
      dyEvent_color: ['color', lang.addDyEvent.color, color == null ? '' : color],
      dyEvent_strokePattern: ['select', lang.addDyEvent.strokePattern, ['dashed', 'dotted', 'dotdash', 'solid'], lang.addDyEvent.strokePatternChoices, strokePattern == null ? 'dashed' : strokePattern],
    };
    return ([elements, { elRequired: false }]);
  },
  dy_dyLimit(defaults) {
    let limit; let staticLimit = false; let label;
    let labelLoc; let color; let strokePattern;
    if (defaults != null) {
      [, {
        limit, staticLimit, label, labelLoc, color, strokePattern,
      }] = defaults;
      staticLimit = staticLimit === true;
    }
    const elements = {
      dy_dyLimit: ['selectDep', [lang.addDyLimit.dyDyLimitCheck, 'numeric', lang.addDyLimit.dyDyLimitTrue, staticLimit ? limit : undefined, 0], lang.addDyLimit.dyDyLimitFalse, outputScalars, outputScalarAliases, staticLimit ? undefined : limit, staticLimit],
      dyLimit_label: ['text', lang.addDyLimit.label, label == null ? '' : label],
      dyLimit_labelLoc: ['select', lang.addDyLimit.labelLoc, ['left', 'right'], lang.addDyLimit.labelLocChoices, labelLoc],
      dyLimit_color: ['color', lang.addDyLimit.color, color == null ? '' : color],
      dyLimit_strokePattern: ['select', lang.addDyLimit.strokePattern, ['dashed', 'dotted', 'dotdash', 'solid'], lang.addDyLimit.strokePatternChoices, strokePattern == null ? 'dashed' : strokePattern],
    };
    return ([elements, { elRequired: false }]);
  },
  dy_dyAnnotation(defaults) {
    let key; let text; let tooltip; let
      width; let height; let attachAtBottom;
    if (defaults != null) {
      [key, {
        text, tooltip, width, height, attachAtBottom,
      }] = defaults;
    }
    const elements = {
      dy_dyAnnotation: ['select', lang.addDyAnnotation.dyDyAnnotation, outputScalars, outputScalarAliases, key],
      dyAnnotation_text: ['text', lang.addDyAnnotation.text, text == null ? 'label' : text],
      dyAnnotation_tooltip: ['text', lang.addDyAnnotation.tooltip, tooltip == null ? '' : tooltip],
      dyAnnotation_width: ['numeric', lang.addDyAnnotation.width, width == null ? 0 : width, 0],
      dyAnnotation_height: ['numeric', lang.addDyAnnotation.height, height == null ? 0 : height, 0],
      dyAnnotation_attachAtBottom: ['checkbox', lang.addDyAnnotation.attachAtBottom, attachAtBottom],
    };
    return ([elements, { elRequired: false }]);
  },
  dy_dyShading(defaults) {
    let from; let to; let staticFrom = false;
    let staticTo = false; let axis; let color;
    if (defaults != null) {
      [, {
        from, to, staticFrom, staticTo, axis, color,
      }] = defaults;
      staticFrom = staticFrom === true;
      staticTo = staticTo === true;
    }
    const elements = {
      dy_dyShading: ['selectDep', [lang.addDyShading.dyDyShadingCheck, 'text', lang.addDyShading.dyDyShadingTrue, staticFrom ? from : undefined], lang.addDyShading.dyDyShadingFalse, outputScalars, outputScalarAliases, staticFrom ? undefined : from, staticFrom],
      dyShading_up: ['selectDep', [lang.addDyShading.dyDyShadingUpCheck, 'text', lang.addDyShading.dyDyShadingUpTrue, staticTo ? to : undefined], lang.addDyShading.dyDyShadingUpFalse, outputScalars, outputScalarAliases, staticTo ? undefined : to, staticTo],
      dyShading_axis: ['select', lang.addDyShading.axis, ['x', 'y'], lang.addDyShading.axisChoices, axis],
      dyShading_color: ['color', lang.addDyShading.color, color == null ? '' : color],
    };
    return ([elements, { elRequired: false }]);
  },
  leaflet_markers(defaults) {
    let lng; let lat; let group;
    let label; let rest; let iconIcon; let color;
    let marker; let labelTextsize; let labelPermanent;
    let labelColor; let labelBgColor;
    if (defaults != null) {
      [, {
        lng, lat, group, label, ...rest
      }] = defaults;
    }
    try { iconIcon = rest.iconOptions.icon; } catch (error) { iconIcon = null; }
    try { color = rest.iconOptions.iconColor; } catch (error) { color = ''; }
    try { marker = rest.iconOptions.markerColor; } catch (error) { marker = 'blue'; }
    try {
      labelTextsize = rest.labelOptions.textsize;
      labelTextsize = labelTextsize.replace('px', '');
      labelTextsize = Number(labelTextsize);
    } catch (error) { labelTextsize = 12; }
    try { labelPermanent = rest.labelOptions.permanent; } catch (error) { labelPermanent = true; }
    try { labelColor = rest.labelOptions.style.color; } catch (error) { labelColor = ''; }
    try { labelBgColor = rest.labelOptions.style['background-color']; } catch (error) { labelBgColor = ''; }
    const elements = {
      leaflet_markers: ['select', lang.addLeafletMarkers.leafletMarkers, scalarIndices, scalarIndexAliases, lat],
      leafMark_lng: ['select', lang.addLeafletMarkers.lng, scalarIndices, scalarIndexAliases, lng],
      leafMark_groupName: ['text', lang.addLeafletMarkers.groupName, group == null ? '' : group],
      leafMark_label: ['text', lang.addLeafletMarkers.label, label == null ? '' : label],
      iconOptionsStart: ['optionsStart', lang.addLeafletMarkers.iconOptions],
      leafMark_icon: ['select', lang.addLeafletMarkers.icon, ['circle', 'circle-notch', 'dot-circle', 'square', 'times-circle', 'user-circle', 'users', 'plus-circle', 'minus-circle', 'exclamation-circle', 'question-circle', 'play-circle', 'check-circle', 'home', 'cog', 'asterisk', 'ban', 'heart', 'leaf', 'lightbulb', 'smile', 'star'], lang.addLeafletMarkers.iconChoices, iconIcon],
      leafMark_iconColor: ['color', lang.addLeafletMarkers.iconColor, color],
      leafMark_markerColor: ['select', lang.addLeafletMarkers.markerColor, ['red', 'darkred', 'lightred', 'orange', 'beige', 'green', 'darkgreen', 'lightgreen', 'blue', 'darkblue', 'lightblue', 'purple', 'darkpurple', 'pink', 'cadetblue', 'white', 'gray', 'lightgray', 'black'], lang.addLeafletMarkers.markerColorChoices, marker],
      iconOptionsEnd: ['optionsEnd'],
      optionsStart: ['optionsStart', lang.addLeafletMarkers.options],
      leafMark_labelPermanent: ['checkbox', lang.addLeafletMarkers.labelPermanent, labelPermanent !== true],
      leafMark_labelcolor: ['color', lang.addLeafletMarkers.labelcolor, labelColor],
      leafMark_labelbgcolor: ['color', lang.addLeafletMarkers.labelbgcolor, labelBgColor],
      leafMark_labelsize: ['numeric', lang.addLeafletMarkers.labelsize, labelTextsize, 0],
      optionsEnd: ['optionsEnd'],
    };
    return ([elements, { elRequired: false }]);
  },
  leaflet_flows(defaults) {
    let lng0; let lat0; let lng1; let lat1; let flow; let color;
    let minThickness; let maxThickness; let time; let
      layerId;
    if (defaults != null) {
      [, {
        lng0, lat0, lng1, lat1, flow, color, minThickness, maxThickness, time, layerId,
      }] = defaults;
    }
    const elements = {
      leaflet_flows: ['select', lang.addLeafletFlows.leafletFlows, scalarIndices, scalarIndexAliases, lat0],
      leafFlow_lng: ['select', lang.addLeafletFlows.lng, scalarIndices, scalarIndexAliases, lng0],
      leafFlow_lat1: ['select', lang.addLeafletFlows.lat1, scalarIndices, scalarIndexAliases, lat1],
      leafFlow_lng1: ['select', lang.addLeafletFlows.lng1, scalarIndices, scalarIndexAliases, lng1],
      leafFlow_flow: ['select', lang.addLeafletFlows.flow, scalarIndices, scalarIndexAliases, flow],
      leafFlow_time: ['select', lang.addLeafletFlows.time, ['_'].concat(nonScalarIndices), ['_'].concat(nonScalarIndexAliases), time],
      leafFlow_label: ['text', `${lang.addLeafletFlows.label}<a title="${lang.addLeafletFlows.labelTooltip}" class="info-wrapper" href="https://gams.com/miro/charts.html#unique-flow-label" target="_blank"><span class="fas fa-info-circle info-icon"></span></a>`, layerId == null ? '' : layerId],
      optionsStart: ['optionsStart', lang.addLeafletFlows.options],
      leafFlow_color: ['color', lang.addLeafletFlows.color, color == null ? '' : color],
      leafFlow_minThickness: ['numeric', lang.addLeafletFlows.minThickness, minThickness == null ? 1 : minThickness, 0],
      leafFlow_maxThickness: ['numeric', lang.addLeafletFlows.maxThickness, maxThickness == null ? 20 : maxThickness, 0],
      optionsEnd: ['optionsEnd'],
    };
    return ([elements, { elRequired: false }]);
  },
  leaflet_minicharts(defaults) {
    let lng; let lat; let chartdata; let type; let width; let height;
    let opacity; let showLabels; let transitionTime; let legend;
    let legendPosition; let time; let
      layerId;
    if (defaults != null) {
      [, {
        lng, lat, chartdata, type, width, height, opacity, showLabels,
        transitionTime, legend, legendPosition, time, layerId,
      }] = defaults;
    }
    const elements = {
      leaflet_minicharts: ['select', lang.addLeafletMinicharts.leafletMinicharts, scalarIndices, scalarIndexAliases, lat],
      leafChart_lng: ['select', lang.addLeafletMinicharts.lng, scalarIndices, scalarIndexAliases, lng],
      leafChart_chartdata: ['select', lang.addLeafletMinicharts.chartdata, scalarIndices, scalarIndexAliases, chartdata == null ? scalarIndices[0] : chartdata, true],
      leafChart_time: ['select', lang.addLeafletMinicharts.time, ['_'].concat(nonScalarIndices), ['_'].concat(nonScalarIndexAliases), time],
      leafChart_type: ['select', lang.addLeafletMinicharts.type, ['bar', 'pie', 'polar-area', 'polar-radius', 'auto'], lang.addLeafletMinicharts.typeChoices, type],
      optionsStart: ['optionsStart', lang.addLeafletMinicharts.options],
      leafChart_width: ['numeric', lang.addLeafletMinicharts.width, width == null ? 30 : width, 0],
      leafChart_height: ['numeric', lang.addLeafletMinicharts.height, height == null ? 30 : height, 0],
      leafChart_opacity: ['numeric', lang.addLeafletMinicharts.opacity, opacity == null ? 1 : opacity, 0, 1, 0.1],
      leafChart_showlabels: ['checkbox', lang.addLeafletMinicharts.showlabels, showLabels],
      leafChart_transitionTime: ['numeric', lang.addLeafletMinicharts.transitionTime, transitionTime == null ? 750 : transitionTime, 0],
      leafChart_layerId: ['select', lang.addLeafletMinicharts.layerId, ['_'].concat(nonScalarIndices), ['_'].concat(nonScalarIndexAliases), layerId],
      leafChart_legend: ['checkbox', lang.addLeafletMinicharts.legend, legend == null ? true : legend],
      leafChart_legendPosition: ['select', lang.addLeafletMinicharts.legendPosition, ['topright', 'topleft', 'bottomright', 'bottomleft'], lang.addLeafletMinicharts.legendPositionChoices, legendPosition],
      optionsEnd: ['optionsEnd'],
    };
    return ([elements, { elRequired: false }]);
  },
  chart_piedata(defaults) {
    let values; let labels; let hole; let
      name;
    if (defaults != null) {
      [, {
        values, labels, hole, name,
      }] = defaults;
    }
    const elements = {
      chart_piedata: ['select', lang.addPieDataEl.chartValues, scalarIndices, scalarIndexAliases, values],
      chart_pielabel: ['select', lang.addPieDataEl.chartLabels, nonScalarIndices, nonScalarIndexAliases, labels],
      chart_piehole: ['numeric', lang.addPieDataEl.holeSize, hole == null ? 0 : hole, 0, 1, 0.1],
      chart_piename: ['text', lang.addPieDataEl.chartName, name == null ? 'label' : name],
    };
    return ([elements, { elRequired: true }, 'piedata']);
  },
  chart_ydatabar(defaults) {
    let key; let label; let rest; let makerLineWidth; let makerLineColor; let markerColor;
    if (defaults != null) {
      [key, {
        label, ...rest
      }] = defaults;
    }
    try { makerLineWidth = rest.marker.line.width; } catch (error) { makerLineWidth = 0; }
    try { makerLineColor = rest.marker.line.color; } catch (error) { makerLineColor = ''; }
    try { markerColor = rest.marker.color; } catch (error) { markerColor = ''; }
    const elements = {
      chart_ydata: ['select', lang.addBarDataEl.chartYdatabar, indices, indexAliases, key],
      chart_ylabel: ['text', lang.addBarDataEl.chartYlabel, label == null ? 'label' : label],
      marker_color: ['color', lang.addBarDataEl.color, markerColor],
      marker_line_width: ['numeric', lang.addBarDataEl.lineWidth, makerLineWidth, 0],
      marker_line_color: ['color', lang.addBarDataEl.lineColor, makerLineColor],
    };
    return ([elements]);
  },
  chart_ydatascatter(defaults) {
    let key; let label; let showlegend; let yaxis; let frame; let rest;
    let markerSize; let markerSymbol; let markerColor;
    let makerLineWidth; let makerLineColor;
    if (defaults != null) {
      [key, {
        label, showlegend, yaxis, frame, ...rest
      }] = defaults;
    }
    try { markerSize = rest.marker.size; } catch (error) { markerSize = 6; }
    try { markerSymbol = rest.marker.symbol; } catch (error) { markerSymbol = '_'; }
    try { markerColor = rest.marker.color; } catch (error) { markerColor = ''; }
    try { makerLineWidth = rest.marker.line.width; } catch (error) { makerLineWidth = 0; }
    try { makerLineColor = rest.marker.line.color; } catch (error) { makerLineColor = ''; }
    const elements = {
      chart_ydata: ['select', lang.addScatterDataEl.chartYdata, scalarIndices, scalarIndexAliases, key],
      chart_ylabel: ['text', lang.addScatterDataEl.chartYlabel, label == null ? 'label' : label],
      trace_yaxis: ['select', lang.addLineDataEl.yaxis, ['y', 'y2'], lang.addLineDataEl.yaxisChoices, yaxis],
      marker_symbol: ['select', lang.addScatterDataEl.symbol, ['_', 'circle', 'circle-open', 'square',
        'square-open', 'diamond', 'diamond-open', 'cross', 'cross-open', 'x', 'x-open', 'triangle-up',
        'triangle-up-open', 'triangle-down', 'triangle-down-open', 'pentagon', 'pentagon-open',
        'hexagon', 'hexagon-open', 'octagon', 'octagon-open', 'star', 'star-open', 'hexagram',
        'hexagram-open', 'asterisk', 'asterisk-open', 'hash', 'hash-open', 'line-ew', 'line-ew-open',
        'line-ns', 'line-ns-open'], lang.addScatterDataEl.symbolChoices, markerSymbol],
      marker_color: ['color', lang.addScatterDataEl.color, markerColor],
      marker_size: ['numeric', lang.addScatterDataEl.size, markerSize, 0],
      marker_line_width: ['numeric', lang.addScatterDataEl.lineWidth, makerLineWidth, 0],
      marker_line_color: ['color', lang.addScatterDataEl.lineColor, makerLineColor],
      trace_legend: ['checkbox', lang.addScatterDataEl.legend, showlegend == null ? true : showlegend],
      trace_frame: ['select', lang.addScatterDataEl.frame, ['_'].concat(indices), ['_'].concat(indexAliases), frame],
    };
    return ([elements]);
  },
  chart_ydataline(defaults) {
    let key; let label; let fill; let showlegend; let yaxis; let frame; let rest;
    let lineWidth; let lineShape; let lineDash;
    let lineColor;
    if (defaults != null) {
      [key, {
        label, fill, showlegend, yaxis, frame, ...rest
      }] = defaults;
    }
    try { lineWidth = rest.line.width; } catch (error) { lineWidth = 2; }
    try { lineShape = rest.line.shape; } catch (error) { lineShape = null; }
    try { lineDash = rest.line.dash; } catch (error) { lineDash = null; }
    try { lineColor = rest.line.color; } catch (error) { lineColor = ''; }
    const elements = {
      chart_ydata: ['select', lang.addLineDataEl.chartYdata, scalarIndices, scalarIndexAliases, key],
      chart_ylabel: ['text', lang.addLineDataEl.chartYlabel, label == null ? 'label' : label],
      trace_yaxis: ['select', lang.addLineDataEl.yaxis, ['y', 'y2'], lang.addLineDataEl.yaxisChoices, yaxis],
      line_color: ['color', lang.addLineDataEl.color, lineColor],
      line_width: ['numeric', lang.addLineDataEl.width, lineWidth, 0],
      line_shape: ['select', lang.addLineDataEl.shape, ['linear', 'spline', 'hv', 'vh', 'hvh', 'vhv'], lang.addLineDataEl.shapeChoices, lineShape],
      line_dash: ['select', lang.addLineDataEl.dash, ['solid', 'dot', 'dash', 'longdash', 'dashdot', 'longdashdot'], lang.addLineDataEl.dashChoices, lineDash],
      line_fill: ['select', lang.addLineDataEl.fill, ['none', 'tozeroy', 'tozerox', 'tonexty', 'tonextx', 'toself', 'tonext'], lang.addLineDataEl.fillChoices, fill],
      trace_legend: ['checkbox', lang.addLineDataEl.legend, showlegend == null ? true : showlegend],
      trace_frame: ['select', lang.addLineDataEl.frame, ['_'].concat(indices), ['_'].concat(indexAliases), frame],
    };
    return ([elements]);
  },
  chart_ydatabubble(defaults) {
    let key; let label; let showlegend; let frame; let rest;
    let markerSize; let markerSymbol; let markerColor;
    let staticColor = false;
    let markerSizemode; let markerMaxsize;
    let makerLineWidth; let makerLineColor;
    if (defaults != null) {
      [key, {
        label, showlegend, frame, staticColor, ...rest
      }] = defaults;
      staticColor = staticColor === true;
    }
    try { markerSize = rest.marker.size; } catch (error) { markerSize = '.index'; }
    try { markerMaxsize = rest.marker.maxsize; } catch (error) { markerMaxsize = 0; }
    try { markerSizemode = rest.marker.sizemode; } catch (error) { markerSizemode = null; }
    try { markerSymbol = rest.marker.symbol; } catch (error) { markerSymbol = '_'; }
    try { markerColor = rest.marker.color; } catch (error) { markerColor = ''; }
    try { makerLineWidth = rest.marker.line.width; } catch (error) { makerLineWidth = 0; }
    try { makerLineColor = rest.marker.line.color; } catch (error) { makerLineColor = ''; }
    const elements = {
      chart_ydata: ['select', lang.addBubbleDataEl.chartYdata, scalarIndices, scalarIndexAliases, key],
      chart_ylabel: ['text', lang.addBubbleDataEl.chartYlabel, label == null ? 'label' : label],
      marker_symbol: ['select', lang.addBubbleDataEl.symbol, ['_', 'circle', 'circle-open', 'square',
        'square-open', 'diamond', 'diamond-open', 'cross', 'cross-open', 'x', 'x-open', 'triangle-up',
        'triangle-up-open', 'triangle-down', 'triangle-down-open', 'pentagon', 'pentagon-open',
        'hexagon', 'hexagon-open', 'octagon', 'octagon-open', 'star', 'star-open', 'hexagram',
        'hexagram-open', 'asterisk', 'asterisk-open', 'hash', 'hash-open', 'line-ew', 'line-ew-open',
        'line-ns', 'line-ns-open'], lang.addScatterDataEl.symbolChoices, markerSymbol],
      marker_colorDep: ['selectDep', [lang.addBubbleDataEl.colorCheck, 'color', lang.addBubbleDataEl.colorCheckTrue, staticColor ? markerColor : undefined], lang.addBubbleDataEl.colorCheckFalse, scalarIndices, scalarIndexAliases, staticColor ? undefined : markerColor, staticColor],
      marker_size: ['select', lang.addBubbleDataEl.size, scalarIndices, scalarIndexAliases, markerSize],
      marker_maxsize: ['numeric', lang.addBubbleDataEl.maxsize, markerMaxsize, 0],
      marker_sizemode: ['select', lang.addBubbleDataEl.sizemode, ['area', 'diameter'], lang.addBubbleDataEl.sizemodeChoices, markerSizemode],
      marker_line_width: ['numeric', lang.addBubbleDataEl.lineWidth, makerLineWidth, 0],
      marker_line_color: ['color', lang.addBubbleDataEl.lineColor, makerLineColor],
      trace_legend: ['checkbox', lang.addBubbleDataEl.legend, showlegend == null ? true : showlegend],
      trace_frame: ['select', lang.addBubbleDataEl.frame, ['_'].concat(indices), ['_'].concat(indexAliases), frame],
    };
    return ([elements]);
  },
  hist_xdata(defaults) {
    let key; let labels; let color;
    if (defaults != null) {
      [key, {
        labels, color,
      }] = defaults;
    }
    const elements = {
      hist_xdata: ['select', lang.addHistDataEl.histXdata, scalarIndices, scalarIndexAliases, key],
      hist_label: ['text', lang.addHistDataEl.label, labels == null ? 'label' : labels],
      hist_color: ['color', lang.addHistDataEl.color, color == null ? '' : color],
    };
    return ([elements]);
  },
  timevis_series(defaults) {
    let content; let start; let end; let type; let title;
    let group; let subgroup; let groupTitle; let subgroupOrder;
    if (defaults != null) {
      [, {
        content, start, end, type, title, group, subgroup, groupTitle, subgroupOrder,
      }] = defaults;
    }
    const elements = {
      timevis_series: ['select', lang.addTimevisDataEl.timevisSeries, indices, indexAliases, content],
      timedata_start: ['select', lang.addTimevisDataEl.start, indices, indexAliases, start],
      timedata_end: ['select', lang.addTimevisDataEl.end, ['_'].concat(indices), ['_'].concat(indexAliases), end],
      timedata_type: ['select', lang.addTimevisDataEl.type, ['box', 'point', 'range', 'background'], lang.addTimevisDataEl.typeChoices, type],
      timedata_title: ['select', lang.addTimevisDataEl.title, ['_'].concat(indices), ['_'].concat(indexAliases), title],
      timedata_group: ['select', lang.addTimevisDataEl.group, ['_'].concat(indices), ['_'].concat(indexAliases), group],
      timedata_subgroup: ['select', lang.addTimevisDataEl.subgroup, ['_'].concat(indices), ['_'].concat(indexAliases), subgroup],
      timedata_grouptitle: ['select', lang.addTimevisDataEl.grouptitle, ['_'].concat(indices), ['_'].concat(indexAliases), groupTitle],
      timedata_subgrouporder: ['select', lang.addTimevisDataEl.subgrouporder, ['_'].concat(indices), ['_'].concat(indexAliases), subgroupOrder],
    };
    return ([elements]);
  },
  timevis_custom(defaults) {
    let time;
    if (defaults != null) {
      [, {
        time,
      }] = defaults;
    }
    const elements = { timevis_custom: ['text', lang.addCustomTimeEl.timevisCustom, time == null ? '' : time, lang.addCustomTimeEl.placeholder] };
    return ([elements, { elRequired: false }]);
  },
};
export function mdToHTML(mdContent, destId, useKatex) {
  if (useKatex === true) {
    $(destId).html(converterMath.makeHtml(mdContent));
    renderMathInElement($(destId)[0], {
      throwOnError: false,
      delimiters: [{
        left: '$$',
        right: '$$',
        display: true,
      }, { left: '$', right: '$', display: false }],
    });
  } else {
    $(destId).html(converter.makeHtml(mdContent));
  }
}

export function mdSave(mdContentId) {
  Shiny.setInputValue('btMdSave', $(mdContentId).val(),
    { priority: 'event' });
}

export function addArrayDataEl(arrayID, defaultsRaw) {
  if ($(`#${arrayID}_wrapper .btn-add-array-el`).is(':disabled')) {
    return;
  }
  let defaults = defaultsRaw;
  let newEl = false;
  if (defaults == null) {
    defaults = [undefined];
    newEl = true;
  } else if (!Array.isArray(defaults)) {
    defaults = Object.entries(defaults);
  }
  defaults.forEach((def) => {
    if (typeof (arrayTypes[arrayID]) === 'undefined') {
      throw new ReferenceError(`Array ID: ${arrayID} not defined.`);
    }
    const [elements, options, rObserveID] = arrayTypes[arrayID](def);
    inputArrayFactory.add(arrayID, elements, options, rObserveID, false, newEl);
  });
}
function addArrayDataElWrapper(arrayID, defaults, symbol, cnt = 1) {
  let arrayWrapper;
  if (symbol) {
    arrayWrapper = $(`#${arrayID}_wrapper[data-symbol="${symbol}"]`);
  } else {
    arrayWrapper = $(`#${arrayID}_wrapper`);
  }
  if (arrayWrapper.is(':visible') || cnt === 8) {
    addArrayDataEl(arrayID, defaults);
  } else if (cnt <= 8) {
    setTimeout(addArrayDataElWrapper, 200, arrayID, defaults, symbol, cnt + 1);
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

    nonScalarIndices = indices.filter((index) => scalarIndices.indexOf(index) === -1);
    nonScalarIndexAliases = indexAliases.filter((index) => scalarIndexAliases
      .indexOf(index) === -1);
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
    if (data.destroy === true && data.arrayID) {
      inputArrayFactory.destroy(data.arrayID);
    }
    setTimeout(addArrayDataElWrapper, 300, data.arrayID, data.defaults, data.symbol);
  });
  Shiny.addCustomMessageHandler('gms-destroyArray', (arrayID) => {
    if (arrayID) {
      inputArrayFactory.destroy(arrayID);
    }
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
