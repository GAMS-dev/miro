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
/******/ 	return __webpack_require__(__webpack_require__.s = "./srcjs/miro_admin.js");
/******/ })
/************************************************************************/
/******/ ({

/***/ "./srcjs/input_array.js":
/*!******************************!*\
  !*** ./srcjs/input_array.js ***!
  \******************************/
/*! exports provided: default */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "default", function() { return InputArrayFactory; });
function _slicedToArray(arr, i) { return _arrayWithHoles(arr) || _iterableToArrayLimit(arr, i) || _nonIterableRest(); }

function _nonIterableRest() { throw new TypeError("Invalid attempt to destructure non-iterable instance"); }

function _iterableToArrayLimit(arr, i) { var _arr = []; var _n = true; var _d = false; var _e = undefined; try { for (var _i = arr[Symbol.iterator](), _s; !(_n = (_s = _i.next()).done); _n = true) { _arr.push(_s.value); if (i && _arr.length === i) break; } } catch (err) { _d = true; _e = err; } finally { try { if (!_n && _i["return"] != null) _i["return"](); } finally { if (_d) throw _e; } } return _arr; }

function _arrayWithHoles(arr) { if (Array.isArray(arr)) return arr; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } }

function _createClass(Constructor, protoProps, staticProps) { if (protoProps) _defineProperties(Constructor.prototype, protoProps); if (staticProps) _defineProperties(Constructor, staticProps); return Constructor; }

/* global $:false Shiny:false Miro:false */

/*eslint-disable */

/*
 * jQuery throttle / debounce - v1.1 - 3/7/2010
 * http://benalman.com/projects/jquery-throttle-debounce-plugin/
 * 
 * Copyright (c) 2010 "Cowboy" Ben Alman
 * Dual licensed under the MIT and GPL licenses.
 * http://benalman.com/about/license/
 */
(function (b, c) {
  var $ = b.jQuery || b.Cowboy || (b.Cowboy = {}),
      a;

  $.throttle = a = function a(e, f, j, i) {
    var h,
        d = 0;

    if (typeof f !== "boolean") {
      i = j;
      j = f;
      f = c;
    }

    function g() {
      var o = this,
          m = +new Date() - d,
          n = arguments;

      function l() {
        d = +new Date();
        j.apply(o, n);
      }

      function k() {
        h = c;
      }

      if (i && !h) {
        l();
      }

      h && clearTimeout(h);

      if (i === c && m > e) {
        l();
      } else {
        if (f !== true) {
          h = setTimeout(i ? k : l, i === c ? e - m : e);
        }
      }
    }

    if ($.guid) {
      g.guid = j.guid = j.guid || $.guid++;
    }

    return g;
  };

  $.debounce = function (d, e, f) {
    return f === c ? a(d, e, false) : a(d, f, e !== false);
  };
})(window);
/* eslint-enable */


var InputArray =
/*#__PURE__*/
function () {
  function InputArray(arrayID, elements) {
    var options = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : {};
    var rObserveID = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : 'array_el';

    _classCallCheck(this, InputArray);

    this.arrayID = void 0;
    this.elements = void 0;
    this.rObserveID = void 0;
    this.options = void 0;
    this.indexEl = void 0;
    this.elCount = 0;
    this.elInArray = [];
    this.freeElIDs = [];
    this.isNewElement = true;
    this.arrayID = arrayID;
    this.elements = elements;
    this.rObserveID = rObserveID;
    this.options = options;

    if (options.elRequired === undefined) {
      this.options.elRequired = true;
    }

    if (options.updateTxtWithLabel === undefined) {
      this.options.updateTxtWithLabel = [];
    }
  }

  _createClass(InputArray, [{
    key: "resetDefault",
    value: function resetDefault() {
      return this.options.myopicDefaults === true && this.isNewElement === false;
    }
  }, {
    key: "createEl",
    value: function createEl() {
      var _this = this;

      var rAddID = "add_".concat(this.rObserveID);
      var elID = this.incElCount();
      var arrayContent = "<div id=\"".concat(this.arrayID).concat(elID, "_wrapper\" class=\"config-array-el\">\n");
      var idx = 0;
      var label = '';
      var labelID;
      $.each(this.elements, function (k, v) {
        var selected;
        var value;
        var choices = [];
        var aliases = [];

        switch (v[0]) {
          case 'select':
            if (idx === 0) {
              if (_this.options.elRequired === true) {
                // as labels need to be unique, set default to option that is not already in use
                _this.indexEl = k;

                if (v[2].length - 1 === _this.elCount) {
                  $("#".concat(_this.arrayID, "_wrapper .btn-add-array-el")).prop('disabled', true);
                }

                for (var i = 0; i < v[2].length; i++) {
                  if (_this.elInArray.indexOf(v[2][i]) === -1) {
                    choices.push(v[2][i]);

                    if (typeof v[3] !== 'undefined') {
                      aliases.push(v[3][i]);
                    }
                  }
                }

                var _choices = choices;

                var _choices2 = _slicedToArray(_choices, 1);

                selected = _choices2[0];
                labelID = selected;
                var _aliases = aliases;

                var _aliases2 = _slicedToArray(_aliases, 1);

                label = _aliases2[0];

                for (var _i2 = 1; _i2 < elID; _i2++) {
                  var labelEl = $("#".concat(k).concat(_i2));

                  if (labelEl.length) {
                    labelEl[0].selectize.disable();
                  }
                }

                _this.addLabelEl(elID, selected);
              } else {
                var _v = _slicedToArray(v, 4);

                choices = _v[2];
                aliases = _v[3];
                var _choices3 = choices;

                var _choices4 = _slicedToArray(_choices3, 1);

                selected = _choices4[0];
                var _aliases3 = aliases;

                var _aliases4 = _slicedToArray(_aliases3, 1);

                label = _aliases4[0];
              }

              if (_this.resetDefault()) {
                selected = '';
              } // tell R that new element was addded to array: (ID, label of element)


              Shiny.setInputValue(rAddID, [elID, selected, k], {
                priority: 'event'
              });
            } else if (_this.resetDefault()) {
              selected = '';

              var _v2 = _slicedToArray(v, 4);

              choices = _v2[2];
              aliases = _v2[3];
            } else {
              var _v3 = _slicedToArray(v, 5);

              choices = _v3[2];
              aliases = _v3[3];
              selected = _v3[4];

              if (selected === '.index') {
                selected = labelID;
              }
            }

            arrayContent += InputArray.createSelectInput(k, elID, v[1], choices, aliases, selected, v[5]);
            break;

          case 'selectDep':
            var _v4 = _slicedToArray(v, 4);

            var _v4$ = _slicedToArray(_v4[3], 1);

            selected = _v4$[0];

            if (v[5] !== undefined) {
              var _v5 = _slicedToArray(v, 6);

              selected = _v5[5];
            }

            if (_this.resetDefault()) {
              selected = '';
            } else if (selected === '.index') {
              selected = labelID;
            }

            if (idx === 0) {
              // tell R that new element was addded to array: (ID)
              Shiny.setInputValue(rAddID, [elID, selected, k], {
                priority: 'event'
              });
            }

            arrayContent += InputArray.createSelectDepInput(k, elID, idx !== 0 ? '' : rAddID, v[1], v[2], v[3], v[4], selected, v[6]);
            break;

          case 'text':
            if (_this.resetDefault()) {
              value = '';
            } else {
              value = v[2] !== undefined ? v[2] : '';
            }

            if (idx === 0 && _this.options.elRequired === false) {
              // tell R that new element was addded to array: (ID)
              Shiny.setInputValue(rAddID, [elID, value, k], {
                priority: 'event'
              });
            }

            if (value === 'label') {
              value = label;

              _this.options.updateTxtWithLabel.push("#".concat(k).concat(elID));
            }

            arrayContent += InputArray.createTextInput(k, elID, v[1], value, v[3]);
            break;

          case 'numeric':
            if (_this.resetDefault()) {
              value = '';
            } else {
              var _v6 = _slicedToArray(v, 3);

              value = _v6[2];
            }

            if (idx === 0 && _this.options.elRequired === false) {
              // tell R that new element was addded to array: (ID)
              Shiny.setInputValue(rAddID, [elID, value, k], {
                priority: 'event'
              });
            }

            arrayContent += InputArray.createNumericInput(k, elID, v[1], value, v[3], v[4], v[5]);
            break;

          case 'checkbox':
            if (_this.resetDefault()) {
              value = false;
            } else {
              var _v7 = _slicedToArray(v, 3);

              value = _v7[2];
            }

            if (idx === 0 && _this.options.elRequired === false) {
              // tell R that new element was addded to array: (ID)
              Shiny.setInputValue(rAddID, [elID, value, k], {
                priority: 'event'
              });
            }

            arrayContent += InputArray.createCheckboxInput(k, elID, v[1], value);
            break;

          case 'color':
            if (_this.resetDefault()) {
              value = '';
            } else {
              var _v8 = _slicedToArray(v, 3);

              value = _v8[2];
            }

            arrayContent += InputArray.createColorPickerInput(k, elID, v[1], value);
            break;

          case 'optionsStart':
            arrayContent += "".concat('<div class="form-group" style="min-height:30px;">' + '<h4 class="box-title option-section-header" style="cursor:pointer;font-weight:bold;" onclick="$(this).next().toggle();">').concat(v[1], " <i class=\"fa fa-plus\"></i></h4><div class=\"option-section\"").concat(v[2] === false ? '' : ' style="display:none;"', ">");
            break;

          case 'optionsEnd':
            arrayContent += '</div></div>';
            break;

          default:
            throw new TypeError("Unknown element type: ".concat(v[0]));
        }

        idx += 1;
      });
      arrayContent += "".concat(!this.options.elRequired || elID > 1 ? "<button type=\"button\" onclick=\"Miro.removeArrayEl('".concat(this.arrayID, "','").concat(elID, "')\" class=\"btn btn-default bt-icon\"><i class=\"far fa-minus-square\"></i></button>\n") : '', "<hr></div>");
      $("#".concat(this.arrayID, "_wrapper .array-wrapper")).append(arrayContent);
      this.registerChangeHandlers(this.elements, rAddID, elID, this.options);
      this.isNewElement = false;
      this.elCount += 1;
    }
  }, {
    key: "removeElAtomic",
    value: function removeElAtomic(element, idxRaw, elID) {
      var _this2 = this;

      var el = null;
      var arrayLabel = null;
      var idx = idxRaw;
      $.each(element, function (k, v) {
        if ($(v).children('input[type="number"]').length) {
          el = $(v).children('input[type="number"]');

          if (idx === 0) {
            arrayLabel = elID;
          }

          el.remove();
        } else if ($(v).children('.miro-color-picker').length) {
          el = $(v).children('.miro-color-picker');

          if (idx === 0) {
            arrayLabel = elID;
          }

          el.colorpicker('destroy');
        } else if ($(v).children('.option-section').length) {
          _this2.removeElAtomic($(v).children('.option-section').first().children('.form-group'), idx, elID);
        } else if ($(v).children('input[type="text"]').length) {
          el = $(v).children('input[type="text"]');

          if (idx === 0) {
            arrayLabel = elID;
          }

          el.remove();
        } else if ($(v).children('.dep-el').length) {
          var depEl = $(v).children('.dep-el');

          if (idx === 0) {
            arrayLabel = elID;
          }

          _this2.removeElAtomic(depEl.eq(0).children('.form-group'), idx, elID);

          _this2.removeElAtomic(depEl.eq(1).children('.form-group'), idx, elID);
        } else if ($(v).find('input[type="checkbox"]').length) {
          el = $(v).find('input[type="checkbox"]');

          if (idx === 0) {
            arrayLabel = elID;
          }

          el.remove();
        } else if ($(v).find('select').length) {
          el = $(v).find('select').get(0);

          if (idx === 0) {
            arrayLabel = $(el).val();
          }

          el.selectize.destroy();
        } else {
          throw new Error('Could not remove array element.');
        }

        idx += 1;
      });
      return arrayLabel;
    }
  }, {
    key: "removeArrayEl",
    value: function removeArrayEl(elID) {
      var idx = 0;
      var arrayLabel = this.removeElAtomic($("#".concat(this.arrayID).concat(elID, "_wrapper")).children('.form-group'), idx, elID);

      if (arrayLabel.length === 0) {
        throw new Error("Failed to remove array element: Could not find identifier for array element: ".concat(this.arrayID, " (label ID: ").concat(this.arrayID).concat(elID, ")."));
      }

      this.elCount -= 1;
      delete this.elInArray[elID];

      if (this.elCount === 1 && this.indexEl !== undefined) {
        var labelEl = $("#".concat(this.indexEl, "1"));

        if (labelEl.length) {
          labelEl[0].selectize.enable();
        }
      }

      $("#".concat(this.arrayID, "_wrapper .btn-add-array-el:disabled")).prop('disabled', false);
      Shiny.setInputValue("remove_".concat(this.rObserveID), [this.arrayID, arrayLabel, elID], {
        priority: 'event'
      });
      this.freeElIDs.push(elID);
      $("#".concat(this.arrayID).concat(elID, "_wrapper")).remove();
    }
  }, {
    key: "updateArrayEl",
    value: function updateArrayEl(elID, newVal) {
      if (this.elInArray[elID] !== undefined) {
        this.elInArray[elID] = newVal;
      }
    }
  }, {
    key: "registerChangeHandlers",
    value: function registerChangeHandlers(elements, rAddID, elID, options) {
      var _this3 = this;

      var htmlID = arguments.length > 4 && arguments[4] !== undefined ? arguments[4] : elID;
      var notFirstIdx = arguments.length > 5 && arguments[5] !== undefined ? arguments[5] : false;
      var idx = 0;
      var arrayID = this.arrayID;
      $.each(elements, function (k, v) {
        if (v[0] === 'select' || v[0] === 'selectDep') {
          if (!notFirstIdx && idx === 0) {
            $("#".concat(k).concat(htmlID)).selectize({
              onChange: function onChange(value) {
                if (options.elRequired) {
                  Miro.updateArrayEl(arrayID, elID, value);
                }

                if (options.updateTxtWithLabel.length) {
                  var alias = $("#".concat(k).concat(htmlID)).text();

                  if (typeof alias !== 'undefined') {
                    for (var i = 0; i < options.updateTxtWithLabel.length; i++) {
                      $(options.updateTxtWithLabel[i]).val(alias);
                    }
                  }
                }

                Shiny.setInputValue(rAddID, [elID, value, k, 'change'], {
                  priority: 'event'
                });
              }
            });
          } else {
            $("#".concat(k).concat(htmlID)).selectize({
              onChange: function onChange(value) {
                Shiny.setInputValue(k, [elID, value], {
                  priority: 'event'
                });
              }
            });
          }

          if (v[0] === 'selectDep') {
            var altElements = {};

            var _v9 = _slicedToArray(v, 2);

            altElements[k] = _v9[1];
            altElements[k] = altElements[k].slice(1);

            _this3.registerChangeHandlers(altElements, rAddID, elID, options, "_alt".concat(elID), idx !== 0);
          }
        } else if (v[0] === 'checkbox') {
          if (!notFirstIdx && idx === 0) {
            $("#".concat(k).concat(htmlID)).on('change', function () {
              Shiny.setInputValue(rAddID, [elID, $(this).prop('checked'), k, 'change'], {
                priority: 'event'
              });
            });
          } else {
            $("#".concat(k).concat(htmlID)).on('change', function () {
              Shiny.setInputValue(k, [elID, $(this).prop('checked')], {
                priority: 'event'
              });
            });
          }
        } else if (v[0] === 'color') {
          if (!notFirstIdx && idx === 0) {
            $("#".concat(k).concat(htmlID)).colorpicker({
              align: 'left'
            }).on('changeColor', $.debounce(500, function () {
              Shiny.setInputValue(rAddID, [elID, $(this).val(), k, 'change'], {
                priority: 'event'
              });
            }));
          } else {
            $("#".concat(k).concat(htmlID)).colorpicker({
              align: 'left'
            }).on('changeColor', $.debounce(500, function () {
              Shiny.setInputValue(k, [elID, $(this).val()]);
            }));
          }
        } else if (!notFirstIdx && idx === 0) {
          $("#".concat(k).concat(htmlID)).on('change', $.debounce(250, function () {
            Shiny.setInputValue(rAddID, [elID, $(this).val(), k, 'change'], {
              priority: 'event'
            });
          }));
        } else {
          $("#".concat(k).concat(htmlID)).on('change', $.debounce(250, function () {
            Shiny.setInputValue(k, [elID, $(this).val()], {
              priority: 'event'
            });
          }));
        }

        idx += 1;
      });
    }
  }, {
    key: "incElCount",
    value: function incElCount() {
      var count = $("#".concat(this.arrayID, "_wrapper .array-wrapper")).children('.config-array-el').length;

      if (this.freeElIDs.length) {
        return this.freeElIDs.pop();
      }

      return count + 1;
    }
  }, {
    key: "addLabelEl",
    value: function addLabelEl(elID, newLabel) {
      if (this.elInArray.indexOf(newLabel) !== -1) {
        throw new Error("Label: ".concat(newLabel, " already in use."));
      }

      this.elInArray[elID] = newLabel;
    }
  }], [{
    key: "createSelectInput",
    value: function createSelectInput(arrayID, elID, label, choices) {
      var aliases = arguments.length > 4 && arguments[4] !== undefined ? arguments[4] : choices;
      var selectedRaw = arguments.length > 5 && arguments[5] !== undefined ? arguments[5] : '';
      var multiple = arguments.length > 6 && arguments[6] !== undefined ? arguments[6] : false;
      var id = arrayID + elID;
      var selected = selectedRaw;
      var optionsHTML = '';

      if (!$.isArray(selected)) {
        selected = [selected];
      }

      for (var i = 0, itemLen = choices.length; i < itemLen; optionsHTML += "<option value=\"".concat(choices[i], "\"").concat($.inArray(choices[i], selected) !== -1 ? ' selected' : '', ">").concat(aliases[i++], "</option>\n")) {
        ;
      }

      return "<div class=\"config-array-err\" id=\"".concat(id, "_err\" style=\"display:none;\"></div>") + '<div class="form-group">\n' + "<label class=\"control-label\" for=\"".concat(id, "\">").concat(label, "</label>\n") + "<div><select id=\"".concat(id, "\"").concat(multiple === true ? ' multiple="multiple"' : '', ">").concat(optionsHTML, "</select>\n</div>\n</div>");
    }
  }, {
    key: "createTextInput",
    value: function createTextInput(arrayID, elID, label) {
      var value = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : '';
      var placeholder = arguments.length > 4 && arguments[4] !== undefined ? arguments[4] : null;
      var id = arrayID + elID;
      Shiny.setInputValue(arrayID, [elID, value], {
        priority: 'event'
      });
      return "<div class=\"config-array-err\" id=\"".concat(id, "_err\" style=\"display:none;\"></div>\n") + '<div class="form-group">\n' + "<label for=\"".concat(id, "\">").concat(label, "</label>\n") + "<input id=\"".concat(id, "\" type=\"text\" class=\"form-control\" value=\"").concat(value, "\"").concat(placeholder !== null ? " placeholder=\"".concat(placeholder, "\"") : '', "/>\n") + '</div>';
    }
  }, {
    key: "createNumericInput",
    value: function createNumericInput(arrayID, elID, label, value) {
      var min = arguments.length > 4 && arguments[4] !== undefined ? arguments[4] : null;
      var max = arguments.length > 5 && arguments[5] !== undefined ? arguments[5] : null;
      var step = arguments.length > 6 && arguments[6] !== undefined ? arguments[6] : null;
      var id = arrayID + elID;
      Shiny.setInputValue(arrayID, [elID, value], {
        priority: 'event'
      });
      return "".concat('<div class="form-group">\n' + '<label for="').concat(id, "\">").concat(label, "</label>\n") + "<input id=\"".concat(id, "\" type=\"number\" class=\"form-control\" value=\"").concat(value, "\"").concat(min !== null ? " min=\"".concat(min, "\"") : '').concat(max !== null ? " max=\"".concat(max, "\"") : '').concat(step !== null ? " step=\"".concat(step, "\"") : '', "/>\n") + '</div>';
    }
  }, {
    key: "createCheckboxInput",
    value: function createCheckboxInput(arrayID, elID, label) {
      var value = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : false;
      var id = arrayID + elID;
      Shiny.setInputValue(arrayID, [elID, value], {
        priority: 'event'
      });
      return "".concat('<div class="form-group">\n' + '<label class="cb-label" for="').concat(id, "\">").concat(label, "</label>\n") + "<div>\n<label class=\"checkbox-material\" for=\"".concat(id, "\">\n<div class=\"checkbox\">\n") + '<label>\n' + "<input id=\"".concat(id, "\" type=\"checkbox\"").concat(value === true ? ' checked="checked"' : '', "/>\n") + '<span></span>\n' + '</label>\n' + '</div>\n' + '</div>\n</label>\n</div>';
    }
  }, {
    key: "createColorPickerInput",
    value: function createColorPickerInput(arrayID, elID, label) {
      var value = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : '';
      var id = arrayID + elID;
      Shiny.setInputValue(arrayID, [elID, value], {
        priority: 'event'
      });
      return "".concat('<div class="form-group shiny-input-container">\n' + '<label for="').concat(id, "\">").concat(label, "</label>\n") + "<input id=\"".concat(id, "\" type=\"text\" class=\"form-control miro-color-picker\" value=\"").concat(value, "\"/>\n") + '</div>';
    }
  }, {
    key: "createSelectDepInput",
    value: function createSelectDepInput(arrayID, elID, rAddID, altEl, label, choices) {
      var aliases = arguments.length > 6 && arguments[6] !== undefined ? arguments[6] : choices;
      var selected = arguments.length > 7 && arguments[7] !== undefined ? arguments[7] : '';
      var firstInput = InputArray.createSelectInput(arrayID, elID, label, choices, aliases, selected);
      var secondInput;

      switch (altEl[1]) {
        case 'select':
          secondInput = InputArray.createSelectInput("".concat(arrayID, "_alt"), elID, altEl[2], altEl[3], altEl[4], altEl[5]);
          break;

        case 'text':
          secondInput = InputArray.createTextInput("".concat(arrayID, "_alt"), elID, altEl[2], altEl[3]);
          break;

        case 'numeric':
          secondInput = InputArray.createNumericInput("".concat(arrayID, "_alt"), elID, altEl[2], altEl[3], altEl[4], altEl[5]);
          break;

        case 'checkbox':
          secondInput = InputArray.createCheckboxInput("".concat(arrayID, "_alt"), elID, altEl[2], altEl[3]);
          break;

        case 'color':
          secondInput = InputArray.createColorPickerInput("".concat(arrayID, "_alt"), elID, altEl[2], altEl[3]);
          break;

        default:
          throw new TypeError("Unknown element type: ".concat(altEl[1]));
      }

      var checkboxInput = "".concat('<div class="form-group col-sm-4">\n' + '<label class="cb-label" for="depCb_').concat(arrayID).concat(elID, "\">").concat(altEl[0], "</label>\n") + "<div>\n<label class=\"checkbox-material\" for=\"depCb_".concat(arrayID).concat(elID, "\">\n<div class=\"checkbox\">\n<label>\n") + "<input id=\"depCb_".concat(arrayID).concat(elID, "\" type=\"checkbox\" onclick=\"Miro.toggleDepContainer(this, '").concat(arrayID, "', '").concat(elID, "', '").concat(rAddID, "');\"/>\n") + '<span></span>\n' + '</label>\n' + '</div>\n' + '</div>\n</label>\n</div>';
      return "<div class=\"form-group dep-group\" style=\"width:100%;display:inline-block;\">\n<div class=\"dep-el col-sm-8\">\n".concat(firstInput, "\n</div>\n<div class=\"dep-el col-sm-8\" style=\"display:none\">\n").concat(secondInput, "\n</div>\n<div>\n").concat(checkboxInput, "\n</div>\n</div>");
    }
  }]);

  return InputArray;
}();

var InputArrayFactory =
/*#__PURE__*/
function () {
  function InputArrayFactory() {
    _classCallCheck(this, InputArrayFactory);

    this.inputArrays = {};
  }

  _createClass(InputArrayFactory, [{
    key: "add",
    value: function add(arrayID, elements, options, rObserveID) {
      var reinitialize = arguments.length > 4 && arguments[4] !== undefined ? arguments[4] : false;

      if (reinitialize === false && Object.prototype.hasOwnProperty.call(this.inputArrays, arrayID)) {
        this.inputArrays[arrayID].createEl();
        return;
      }

      this.inputArrays[arrayID] = new InputArray(arrayID, elements, options, rObserveID);
      this.inputArrays[arrayID].createEl();
    }
  }, {
    key: "remove",
    value: function remove(arrayID, elID) {
      if (Object.prototype.hasOwnProperty.call(this.inputArrays, arrayID)) {
        this.inputArrays[arrayID].removeArrayEl(elID);
        return true;
      }

      return false;
    }
  }, {
    key: "update",
    value: function update(arrayID, elID, newVal) {
      if (Object.prototype.hasOwnProperty.call(this.inputArrays, arrayID)) {
        this.inputArrays[arrayID].updateArrayEl(elID, newVal);
        return true;
      }

      return false;
    }
  }]);

  return InputArrayFactory;
}();



/***/ }),

/***/ "./srcjs/miro.js":
/*!***********************!*\
  !*** ./srcjs/miro.js ***!
  \***********************/
/*! exports provided: showSpinnerIcon, changeTab, slideToggleEl, showNewNameBaseDialog, confirmModalShow, removeAttachment, changeDDButtonEvent, showJobsDialog, renderMathJax, validateSname, validateHcubeHash, hcHashImport, jumpToLogMark */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "showSpinnerIcon", function() { return showSpinnerIcon; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "changeTab", function() { return changeTab; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "slideToggleEl", function() { return slideToggleEl; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "showNewNameBaseDialog", function() { return showNewNameBaseDialog; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "confirmModalShow", function() { return confirmModalShow; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "removeAttachment", function() { return removeAttachment; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "changeDDButtonEvent", function() { return changeDDButtonEvent; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "showJobsDialog", function() { return showJobsDialog; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "renderMathJax", function() { return renderMathJax; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "validateSname", function() { return validateSname; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "validateHcubeHash", function() { return validateHcubeHash; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "hcHashImport", function() { return hcHashImport; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "jumpToLogMark", function() { return jumpToLogMark; });
function asyncGeneratorStep(gen, resolve, reject, _next, _throw, key, arg) { try { var info = gen[key](arg); var value = info.value; } catch (error) { reject(error); return; } if (info.done) { resolve(value); } else { Promise.resolve(value).then(_next, _throw); } }

function _asyncToGenerator(fn) { return function () { var self = this, args = arguments; return new Promise(function (resolve, reject) { var gen = fn.apply(self, args); function _next(value) { asyncGeneratorStep(gen, resolve, reject, _next, _throw, "next", value); } function _throw(err) { asyncGeneratorStep(gen, resolve, reject, _next, _throw, "throw", err); } _next(undefined); }); }; }

/* global $:false Shiny: false HTMLWidgets:false MathJax:false */
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
    $("#jobImportDlProgressWrapper_".concat(data.id)).siblings('div:first').text(data.text);
    $("#jobImportDlProgressWrapper_".concat(data.id)).hide();
    $("#btDownloadJob_".concat(data.id)).hide();
    $("#btImportJob_".concat(data.id)).show();

    if (data.triggerImport === true && $("#jobImportDlProgressWrapper_".concat(data.id)).is(':visible')) {
      Shiny.setInputValue('importJob', data.id, {
        priority: 'event'
      });
    }
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

/***/ }),

/***/ "./srcjs/miro_admin.js":
/*!*****************************!*\
  !*** ./srcjs/miro_admin.js ***!
  \*****************************/
/*! exports provided: confirmModalShow, slideToggleEl, removeArrayEl, updateArrayEl, toggleDepContainer, addArrayDataEl */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "removeArrayEl", function() { return removeArrayEl; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "updateArrayEl", function() { return updateArrayEl; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "toggleDepContainer", function() { return toggleDepContainer; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "addArrayDataEl", function() { return addArrayDataEl; });
/* harmony import */ var _input_array__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./input_array */ "./srcjs/input_array.js");
/* harmony import */ var _miro__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ./miro */ "./srcjs/miro.js");
/* harmony reexport (safe) */ __webpack_require__.d(__webpack_exports__, "confirmModalShow", function() { return _miro__WEBPACK_IMPORTED_MODULE_1__["confirmModalShow"]; });

/* harmony reexport (safe) */ __webpack_require__.d(__webpack_exports__, "slideToggleEl", function() { return _miro__WEBPACK_IMPORTED_MODULE_1__["slideToggleEl"]; });

function _slicedToArray(arr, i) { return _arrayWithHoles(arr) || _iterableToArrayLimit(arr, i) || _nonIterableRest(); }

function _nonIterableRest() { throw new TypeError("Invalid attempt to destructure non-iterable instance"); }

function _iterableToArrayLimit(arr, i) { var _arr = []; var _n = true; var _d = false; var _e = undefined; try { for (var _i = arr[Symbol.iterator](), _s; !(_n = (_s = _i.next()).done); _n = true) { _arr.push(_s.value); if (i && _arr.length === i) break; } } catch (err) { _d = true; _e = err; } finally { try { if (!_n && _i["return"] != null) _i["return"](); } finally { if (_d) throw _e; } } return _arr; }

function _arrayWithHoles(arr) { if (Array.isArray(arr)) return arr; }



/* global $:false Shiny:false */

var lang = {};
var indices = [];
var indexAliases = [];
var scalarIndices = [];
var scalarIndexAliases = [];
var nonScalarIndices = [];
var nonScalarIndexAliases = [];
var outputScalars = [];
var outputScalarAliases = [];
var inputSymbols = [];
var inputSymbolsAliases = [];
var outputSymbols = [];
var outputSymbolsAliases = [];
var inputArrayFactory = new _input_array__WEBPACK_IMPORTED_MODULE_0__["default"]();
function removeArrayEl(arrayID, elID) {
  inputArrayFactory.remove(arrayID, elID);
}
function updateArrayEl(arrayID, elID, newVal) {
  inputArrayFactory.update(arrayID, elID, newVal);
}
function toggleDepContainer(el, arrayID, elID, rAddID) {
  var depGroup = $(el).closest('.dep-group');
  depGroup.children('.dep-el').toggle();
  var activeEl = depGroup.children('.dep-el:visible').first().children('.form-group');
  var value;
  $.each(activeEl, function (k, v) {
    if ($(v).children('input[type="number"]').length) {
      value = $(v).children('input[type="number"]').val();
    } else if ($(v).children('.miro-color-picker').length) {
      value = $(v).children('.miro-color-picker').val();
    } else if ($(v).children('input[type="text"]').length) {
      value = $(v).children('input[type="text"]').val();
    } else if ($(v).find('input[type="checkbox"]').length) {
      value = $(v).find('input[type="checkbox"]').val();
    } else if ($(v).find('select').length) {
      var _$$find$get$selectize = _slicedToArray($(v).find('select').get(0).selectize.items, 1);

      value = _$$find$get$selectize[0];
    }
  });

  if (rAddID.length) {
    Shiny.setInputValue(rAddID, [elID, value, arrayID, 'change'], {
      priority: 'event'
    });
  } else {
    Shiny.setInputValue(arrayID, [elID, value], {
      priority: 'event'
    });
  }
}
var arrayTypes = {
  symbol_inputGroups: function symbol_inputGroups(defaults) {
    var name;
    var members;

    if (defaults !== undefined) {
      name = defaults.name;
      members = defaults.members;
    }

    var elements = {
      symbol_inputGroups: ['text', lang.addInputGroup.symbolInputgroups, name],
      group_memberIn: ['select', lang.addInputGroup.groupMemberIn, inputSymbols, inputSymbolsAliases, members, true]
    };
    return [elements, {
      elRequired: false,
      myopicDefaults: true
    }, 'general'];
  },
  symbol_outputGroups: function symbol_outputGroups(defaults) {
    var name;
    var members;

    if (defaults !== undefined) {
      name = defaults.name;
      members = defaults.members;
    }

    var elements = {
      symbol_outputGroups: ['text', lang.addOutputGroup.symbolOutputgroups, name],
      group_memberOut: ['select', lang.addOutputGroup.groupMemberOut, outputSymbols, outputSymbolsAliases, members, true]
    };
    return [elements, {
      elRequired: false,
      myopicDefaults: true
    }, 'general'];
  },
  dy_dyEvent: function dy_dyEvent() {
    var elements = {
      dy_dyEvent: ['select', lang.addDyEvent.dyDyEvent, outputScalars, outputScalarAliases],
      dyEvent_label: ['text', lang.addDyEvent.label],
      dyEvent_labelLoc: ['select', lang.addDyEvent.labelLoc, ['top', 'bottom'], lang.addDyEvent.labelLocChoices],
      dyEvent_color: ['color', lang.addDyEvent.color, 'rgb(0,0,0)'],
      dyEvent_strokePattern: ['select', lang.addDyEvent.strokePattern, ['dashed', 'dotted', 'dotdash', 'solid'], lang.addDyEvent.strokePatternChoices]
    };
    return [elements, {
      elRequired: false
    }];
  },
  dy_dyLimit: function dy_dyLimit() {
    var elements = {
      dy_dyLimit: ['selectDep', [lang.addDyLimit.dyDyLimitCheck, 'numeric', lang.addDyLimit.dyDyLimitTrue, 0, 0], lang.addDyLimit.dyDyLimitFalse, outputScalars, outputScalarAliases],
      dyLimit_label: ['text', lang.addDyLimit.label],
      dyLimit_labelLoc: ['select', lang.addDyLimit.labelLoc, ['left', 'right'], lang.addDyLimit.labelLocChoices],
      dyLimit_color: ['color', lang.addDyLimit.color, 'rgb(0,0,0)'],
      dyLimit_strokePattern: ['select', lang.addDyLimit.strokePattern, ['dashed', 'dotted', 'dotdash', 'solid'], lang.addDyLimit.strokePatternChoices]
    };
    return [elements, {
      elRequired: false
    }];
  },
  dy_dyAnnotation: function dy_dyAnnotation() {
    var elements = {
      dy_dyAnnotation: ['select', lang.addDyAnnotation.dyDyAnnotation, outputScalars, outputScalarAliases],
      dyAnnotation_text: ['text', lang.addDyAnnotation.text, 'label'],
      dyAnnotation_tooltip: ['text', lang.addDyAnnotation.tooltip],
      dyAnnotation_width: ['numeric', lang.addDyAnnotation.width, 0, 0],
      dyAnnotation_height: ['numeric', lang.addDyAnnotation.height, 0, 0],
      dyAnnotation_attachAtBottom: ['checkbox', lang.addDyAnnotation.attachAtBottom]
    };
    return [elements, {
      elRequired: false
    }];
  },
  dy_dyShading: function dy_dyShading() {
    var elements = {
      dy_dyShading: ['selectDep', [lang.addDyShading.dyDyShadingCheck, 'text', lang.addDyShading.dyDyShadingTrue], lang.addDyShading.dyDyShadingFalse, outputScalars, outputScalarAliases],
      dyShading_up: ['selectDep', [lang.addDyShading.dyDyShadingUpCheck, 'text', lang.addDyShading.dyDyShadingUpTrue], lang.addDyShading.dyDyShadingUpFalse, outputScalars, outputScalarAliases],
      dyShading_axis: ['select', lang.addDyShading.axis, ['x', 'y'], lang.addDyShading.axisChoices],
      dyShading_color: ['color', lang.addDyShading.color, '#EFEFEF']
    };
    return [elements, {
      elRequired: false
    }];
  },
  leaflet_markers: function leaflet_markers() {
    var elements = {
      leaflet_markers: ['select', lang.addLeafletMarkers.leafletMarkers, scalarIndices, scalarIndexAliases],
      leafMark_lng: ['select', lang.addLeafletMarkers.lng, scalarIndices, scalarIndexAliases],
      leafMark_groupName: ['text', lang.addLeafletMarkers.groupName],
      leafMark_label: ['text', lang.addLeafletMarkers.label],
      optionsStart: ['optionsStart', lang.addLeafletMarkers.options],
      leafMark_labelPermanent: ['checkbox', lang.addLeafletMarkers.labelPermanent, true],
      leafMark_labelcolor: ['color', lang.addLeafletMarkers.labelcolor],
      leafMark_labelbgcolor: ['color', lang.addLeafletMarkers.labelbgcolor],
      leafMark_labelsize: ['numeric', lang.addLeafletMarkers.labelsize, 12, 0],
      optionsEnd: ['optionsEnd']
    };
    return [elements, {
      elRequired: false
    }];
  },
  leaflet_flows: function leaflet_flows() {
    var elements = {
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
      optionsEnd: ['optionsEnd']
    };
    return [elements, {
      elRequired: false
    }];
  },
  leaflet_minicharts: function leaflet_minicharts() {
    var elements = {
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
      optionsEnd: ['optionsEnd']
    };
    return [elements, {
      elRequired: false
    }];
  },
  chart_ydatabar: function chart_ydatabar() {
    var elements = {
      chart_ydata: ['select', lang.addBarDataEl.chartYdatabar, indices, indexAliases],
      chart_ylabel: ['text', lang.addBarDataEl.chartYlabel, 'label'],
      marker_color: ['color', lang.addBarDataEl.color],
      marker_line_width: ['numeric', lang.addBarDataEl.lineWidth, 0, 0],
      marker_line_color: ['color', lang.addBarDataEl.lineColor]
    };
    return [elements];
  },
  chart_ydatascatter: function chart_ydatascatter() {
    var elements = {
      chart_ydata: ['select', lang.addScatterDataEl.chartYdata, scalarIndices, scalarIndexAliases],
      chart_ylabel: ['text', lang.addScatterDataEl.chartYlabel, 'label'],
      marker_symbol: ['select', lang.addScatterDataEl.symbol, ['circle', 'circle-open', 'circle-dot', 'circle-open-dot', 'square', 'square-open', 'square-dot', 'square-open-dot', 'diamond', 'diamond-open', 'diamond-dot', 'diamond-open-dot', 'cross', 'cross-open', 'cross-dot', 'cross-open-dot', 'x', 'x-open', 'x-dot', 'x-open-dot', 'triangle-up', 'triangle-up-open', 'triangle-up-dot', 'triangle-up-open-dot', 'triangle-down', 'triangle-down-open', 'triangle-down-dot', 'triangle-down-open-dot', 'triangle-left', 'triangle-left-open', 'triangle-left-dot', 'triangle-left-open-dot', 'triangle-right', 'triangle-right-open', 'triangle-right-dot', 'triangle-right-open-dot', 'triangle-ne', 'triangle-ne-open', 'triangle-ne-dot', 'triangle-ne-open-dot', 'triangle-se', 'triangle-se-open', 'triangle-se-dot', 'triangle-se-open-dot', 'triangle-sw', 'triangle-sw-open', 'triangle-sw-dot', 'triangle-sw-open-dot', 'triangle-nw', 'triangle-nw-open', 'triangle-nw-dot', 'triangle-nw-open-dot', 'pentagon', 'pentagon-open', 'pentagon-dot', 'pentagon-open-dot', 'hexagon', 'hexagon-open', 'hexagon-dot', 'hexagon-open-dot', 'hexagon2', 'hexagon2-open', 'hexagon2-dot', 'hexagon2-open-dot', 'octagon', 'octagon-open', 'octagon-dot', 'octagon-open-dot', 'star', 'star-open', 'star-dot', 'star-open-dot', 'hexagram', 'hexagram-open', 'hexagram-dot', 'hexagram-open-dot', 'star-triangle-up', 'star-triangle-up-open', 'star-triangle-up-dot', 'star-triangle-up-open-dot', 'star-triangle-down', 'star-triangle-down-open', 'star-triangle-down-dot', 'star-triangle-down-open-dot', 'star-square', 'star-square-open', 'star-square-dot', 'star-square-open-dot', 'star-diamond', 'star-diamond-open', 'star-diamond-dot', 'star-diamond-open-dot', 'diamond-tall', 'diamond-tall-open', 'diamond-tall-dot', 'diamond-tall-open-dot', 'diamond-wide', 'diamond-wide-open', 'diamond-wide-dot', 'diamond-wide-open-dot', 'hourglass', 'hourglass-open', 'bowtie', 'bowtie-open', 'circle-cross', 'circle-cross-open', 'circle-x', 'circle-x-open', 'square-cross', 'square-cross-open', 'square-x', 'square-x-open', 'diamond-cross', 'diamond-cross-open', 'diamond-x', 'diamond-x-open', 'cross-thin', 'cross-thin-open', 'x-thin', 'x-thin-open', 'asterisk', 'asterisk-open', 'hash', 'hash-open', 'hash-dot', 'hash-open-dot', 'y-up', 'y-up-open', 'y-down', 'y-down-open', 'y-left', 'y-left-open', 'y-right', 'y-right-open', 'line-ew', 'line-ew-open', 'line-ns', 'line-ns-open', 'line-ne', 'line-ne-open', 'line-nw', 'line-nw-open'], lang.addScatterDataEl.symbolChoices],
      marker_color: ['color', lang.addScatterDataEl.color],
      marker_size: ['numeric', lang.addScatterDataEl.size, 6, 0],
      marker_line_width: ['numeric', lang.addScatterDataEl.lineWidth, 0, 0],
      marker_line_color: ['color', lang.addScatterDataEl.lineColor],
      trace_legend: ['checkbox', lang.addScatterDataEl.legend],
      trace_frame: ['select', lang.addScatterDataEl.frame, ['_'].concat(indices), ['_'].concat(indexAliases)]
    };
    return [elements];
  },
  chart_ydataline: function chart_ydataline() {
    var elements = {
      chart_ydata: ['select', lang.addLineDataEl.chartYdata, scalarIndices, scalarIndexAliases],
      chart_ylabel: ['text', lang.addLineDataEl.chartYlabel, 'label'],
      line_color: ['color', lang.addLineDataEl.color],
      line_width: ['numeric', lang.addLineDataEl.width, 2, 0],
      line_shape: ['select', lang.addLineDataEl.shape, ['linear', 'spline', 'hv', 'vh', 'hvh', 'vhv'], lang.addLineDataEl.shapeChoices],
      line_dash: ['select', lang.addLineDataEl.dash, ['solid', 'dot', 'dash', 'longdash', 'dashdot', 'longdashdot'], lang.addLineDataEl.dashChoices],
      line_fill: ['select', lang.addLineDataEl.fill, ['none', 'tozeroy', 'tozerox', 'tonexty', 'tonextx', 'toself', 'tonext'], lang.addLineDataEl.fillChoices],
      trace_legend: ['checkbox', lang.addLineDataEl.legend],
      trace_frame: ['select', lang.addLineDataEl.frame, ['_'].concat(indices), ['_'].concat(indexAliases)]
    };
    return [elements];
  },
  chart_ydatabubble: function chart_ydatabubble() {
    var elements = {
      chart_ydata: ['select', lang.addBubbleDataEl.chartYdata, scalarIndices, scalarIndexAliases],
      chart_ylabel: ['text', lang.addBubbleDataEl.chartYlabel, 'label'],
      marker_symbol: ['select', lang.addBubbleDataEl.symbol, ['circle', 'circle-open', 'circle-dot', 'circle-open-dot', 'square', 'square-open', 'square-dot', 'square-open-dot', 'diamond', 'diamond-open', 'diamond-dot', 'diamond-open-dot', 'cross', 'cross-open', 'cross-dot', 'cross-open-dot', 'x', 'x-open', 'x-dot', 'x-open-dot', 'triangle-up', 'triangle-up-open', 'triangle-up-dot', 'triangle-up-open-dot', 'triangle-down', 'triangle-down-open', 'triangle-down-dot', 'triangle-down-open-dot', 'triangle-left', 'triangle-left-open', 'triangle-left-dot', 'triangle-left-open-dot', 'triangle-right', 'triangle-right-open', 'triangle-right-dot', 'triangle-right-open-dot', 'triangle-ne', 'triangle-ne-open', 'triangle-ne-dot', 'triangle-ne-open-dot', 'triangle-se', 'triangle-se-open', 'triangle-se-dot', 'triangle-se-open-dot', 'triangle-sw', 'triangle-sw-open', 'triangle-sw-dot', 'triangle-sw-open-dot', 'triangle-nw', 'triangle-nw-open', 'triangle-nw-dot', 'triangle-nw-open-dot', 'pentagon', 'pentagon-open', 'pentagon-dot', 'pentagon-open-dot', 'hexagon', 'hexagon-open', 'hexagon-dot', 'hexagon-open-dot', 'hexagon2', 'hexagon2-open', 'hexagon2-dot', 'hexagon2-open-dot', 'octagon', 'octagon-open', 'octagon-dot', 'octagon-open-dot', 'star', 'star-open', 'star-dot', 'star-open-dot', 'hexagram', 'hexagram-open', 'hexagram-dot', 'hexagram-open-dot', 'star-triangle-up', 'star-triangle-up-open', 'star-triangle-up-dot', 'star-triangle-up-open-dot', 'star-triangle-down', 'star-triangle-down-open', 'star-triangle-down-dot', 'star-triangle-down-open-dot', 'star-square', 'star-square-open', 'star-square-dot', 'star-square-open-dot', 'star-diamond', 'star-diamond-open', 'star-diamond-dot', 'star-diamond-open-dot', 'diamond-tall', 'diamond-tall-open', 'diamond-tall-dot', 'diamond-tall-open-dot', 'diamond-wide', 'diamond-wide-open', 'diamond-wide-dot', 'diamond-wide-open-dot', 'hourglass', 'hourglass-open', 'bowtie', 'bowtie-open', 'circle-cross', 'circle-cross-open', 'circle-x', 'circle-x-open', 'square-cross', 'square-cross-open', 'square-x', 'square-x-open', 'diamond-cross', 'diamond-cross-open', 'diamond-x', 'diamond-x-open', 'cross-thin', 'cross-thin-open', 'x-thin', 'x-thin-open', 'asterisk', 'asterisk-open', 'hash', 'hash-open', 'hash-dot', 'hash-open-dot', 'y-up', 'y-up-open', 'y-down', 'y-down-open', 'y-left', 'y-left-open', 'y-right', 'y-right-open', 'line-ew', 'line-ew-open', 'line-ns', 'line-ns-open', 'line-ne', 'line-ne-open', 'line-nw', 'line-nw-open'], lang.addScatterDataEl.symbolChoices],
      marker_colorDep: ['selectDep', [lang.addBubbleDataEl.colorCheck, 'color', lang.addBubbleDataEl.colorCheckTrue], lang.addBubbleDataEl.colorCheckFalse, scalarIndices, scalarIndexAliases, '.index'],
      marker_size: ['select', lang.addBubbleDataEl.size, scalarIndices, scalarIndexAliases, '.index'],
      marker_maxsize: ['numeric', lang.addBubbleDataEl.maxsize, 0, 0],
      marker_line_width: ['numeric', lang.addBubbleDataEl.lineWidth, 0, 0],
      marker_line_color: ['color', lang.addBubbleDataEl.lineColor],
      trace_legend: ['checkbox', lang.addBubbleDataEl.legend],
      trace_frame: ['select', lang.addBubbleDataEl.frame, ['_'].concat(indices), ['_'].concat(indexAliases)]
    };
    return [elements];
  },
  hist_xdata: function hist_xdata() {
    var elements = {
      hist_xdata: ['select', lang.addHistDataEl.histXdata, scalarIndices, scalarIndexAliases],
      hist_label: ['text', lang.addHistDataEl.label, 'label'],
      hist_color: ['color', lang.addHistDataEl.color, '#000000']
    };
    return [elements];
  },
  dy_ydata: function dy_ydata() {
    var elements = {
      chart_ydata: ['select', lang.addDyDataEl.chartYdata, scalarIndices, scalarIndexAliases],
      chart_ylabel: ['text', lang.addDyDataEl.label, 'label'],
      dyser_color: ['color', lang.addDyDataEl.color],
      dyopt_stepPlot: ['checkbox', lang.addDyDataEl.stepPlot],
      dyopt_stemPlot: ['checkbox', lang.addDyDataEl.stemPlot],
      dyopt_fillGraph: ['checkbox', lang.addDyDataEl.fillGraph],
      dyopt_drawPoints: ['checkbox', lang.addDyDataEl.drawPoints],
      dyopt_pointShape: ['select', lang.addDyDataEl.pointShape, ['dot', 'triangle', 'square', 'diamond', 'pentagon', 'hexagon', 'circle', 'star', 'plus', 'ex'], lang.addDyDataEl.pointShapeChoices],
      dyopt_pointSize: ['numeric', lang.addDyDataEl.pointSize, 2, 0]
    };
    return [elements];
  },
  timevis_series: function timevis_series() {
    var elements = {
      timevis_series: ['select', lang.addTimevisDataEl.timevisSeries, indices, indexAliases],
      timedata_start: ['select', lang.addTimevisDataEl.start, indices, indexAliases],
      timedata_end: ['select', lang.addTimevisDataEl.end, ['_'].concat(indices), ['_'].concat(indexAliases)],
      timedata_type: ['select', lang.addTimevisDataEl.type, ['box', 'point', 'range', 'background'], lang.addTimevisDataEl.typeChoices],
      timedata_title: ['select', lang.addTimevisDataEl.title, ['_'].concat(indices), ['_'].concat(indexAliases)],
      timedata_group: ['select', lang.addTimevisDataEl.group, ['_'].concat(indices), ['_'].concat(indexAliases)],
      timedata_subgroup: ['select', lang.addTimevisDataEl.subgroup, ['_'].concat(indices), ['_'].concat(indexAliases)],
      timedata_grouptitle: ['select', lang.addTimevisDataEl.grouptitle, ['_'].concat(indices), ['_'].concat(indexAliases)],
      timedata_subgrouporder: ['select', lang.addTimevisDataEl.subgrouporder, ['_'].concat(indices), ['_'].concat(indexAliases)]
    };
    return [elements];
  },
  timevis_custom: function timevis_custom() {
    var elements = {
      timevis_custom: ['text', lang.addCustomTimeEl.timevisCustom, '', lang.addCustomTimeEl.placeholder]
    };
    return [elements, {
      elRequired: false
    }];
  }
};
function addArrayDataEl(arrayID, defaultsRaw) {
  var reinitialize = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : false;

  if ($("#".concat(arrayID, "_wrapper .btn-add-array-el")).is(':disabled')) {
    return;
  }

  var defaults = defaultsRaw;

  if (defaults == null) {
    defaults = [undefined];
  }

  defaults.forEach(function (def) {
    if (typeof arrayTypes[arrayID] === 'undefined') {
      throw new ReferenceError("Array ID: ".concat(arrayID, " not defined."));
    }

    var _arrayTypes$arrayID = arrayTypes[arrayID](def),
        _arrayTypes$arrayID2 = _slicedToArray(_arrayTypes$arrayID, 3),
        elements = _arrayTypes$arrayID2[0],
        options = _arrayTypes$arrayID2[1],
        rObserveID = _arrayTypes$arrayID2[2];

    inputArrayFactory.add(arrayID, elements, options, rObserveID, reinitialize);
  });
}

function addArrayDataElWrapper(arrayID, defaults) {
  var cnt = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : 1;

  if ($("#".concat(arrayID, "_wrapper")).is(':visible')) {
    addArrayDataEl(arrayID, defaults, true);
  } else if (cnt <= 6) {
    setTimeout(addArrayDataElWrapper, 200, arrayID, defaults, cnt + 1);
  }
}

$(document).ready(function () {
  Shiny.addCustomMessageHandler('gms-setScalarOutputs', function (scalarDataRaw) {
    var scalarData = scalarDataRaw;
    $.each(scalarData, function (key, val) {
      if (!$.isArray(val)) {
        scalarData[key] = [val];
      }
    });
    outputScalars = scalarData.indices;
    outputScalarAliases = scalarData.aliases;
  });
  Shiny.addCustomMessageHandler('gms-setIndices', function (indicesFromRRaw) {
    var indicesFromR = indicesFromRRaw;
    $.each(indicesFromR, function (key, val) {
      if (!$.isArray(val)) {
        indicesFromR[key] = [val];
      }
    });
    indices = indicesFromR.indices;
    indexAliases = indicesFromR.aliases;
    scalarIndices = indicesFromR.scalarIndices;
    scalarIndexAliases = indicesFromR.scalarAliases;
    nonScalarIndices = indices.filter(function (index) {
      return scalarIndices.indexOf(index) === -1;
    });
    nonScalarIndexAliases = indexAliases.filter(function (index) {
      return scalarIndexAliases.indexOf(index) === -1;
    });
  });
  Shiny.addCustomMessageHandler('gms-setGAMSSymbols', function (data) {
    var symData = data.gamsSymbols;
    lang = data.lang;
    $.each(symData, function (key, val) {
      if (!$.isArray(val)) {
        symData[key] = [val];
      }
    });
    inputSymbols = symData.inSym;
    inputSymbolsAliases = symData.inAlias;
    outputSymbols = symData.outSym;
    outputSymbolsAliases = symData.outAlias;
  });
  Shiny.addCustomMessageHandler('gms-addArrayEl', function (data) {
    setTimeout(addArrayDataElWrapper, 200, data.arrayID, data.defaults);
  });
  var colorPickerBinding = new Shiny.InputBinding();
  $.extend(colorPickerBinding, {
    find: function find(scope) {
      return $(scope).find('.miro-color-picker');
    },
    getValue: function getValue(el) {
      return $(el).val();
    },
    setValue: function setValue(el, value) {
      $(el).setColor(value);
    },
    subscribe: function subscribe(el, callback) {
      $(el).on('change.colorPickerBinding', function () {
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
      $(el).colorpicker({
        align: 'left'
      });
    },
    unsubscribe: function unsubscribe(el) {
      $(el).colorpicker('destroy');
      $(el).off('.colorPickerBinding');
    }
  });
  Shiny.inputBindings.register(colorPickerBinding);
});

/***/ })

/******/ });
//# sourceMappingURL=miro_admin.js.map