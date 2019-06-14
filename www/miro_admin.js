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

/* global $:false Shiny:false outputScalars outputScalarAliases */

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
    this.elInArray = void 0;
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
      $.each(this.elements, function (k, v) {
        var selected;
        var value;

        switch (v[0]) {
          case 'select':
            if (idx === 0) {
              if (_this.options.elRequired === true) {
                // as labels need to be unique, set default to option that is not already in use
                var noElInArray = _this.elInArray !== undefined ? _this.elInArray.length : 0;

                if (v[2].length - 1 === noElInArray) {
                  $("#".concat(_this.arrayID, "_wrapper .btn-add-array-el")).prop('disabled', true);
                }

                for (var i = 0; i < v[2].length; i++) {
                  if ($.inArray(v[2][i], _this.elInArray) === -1) {
                    selected = v[2][i];
                    label = v[3][i];
                    break;
                  }
                }

                _this.addLabelEl(selected);
              } else {
                var _v = _slicedToArray(v, 4);

                var _v$ = _slicedToArray(_v[2], 1);

                selected = _v$[0];

                var _v$2 = _slicedToArray(_v[3], 1);

                label = _v$2[0];
              }

              if (_this.resetDefault()) {
                selected = '';
              } // tell R that new element was addded to array: (ID, label of element)


              Shiny.setInputValue(rAddID, [elID, selected, k], {
                priority: 'event'
              });
            } else if (_this.resetDefault()) {
              selected = '';
            } else {
              var _v2 = _slicedToArray(v, 5);

              selected = _v2[4];
            }

            arrayContent += InputArray.createSelectInput(k, elID, v[1], v[2], v[3], selected, v[5]);
            break;

          case 'selectDep':
            var _v3 = _slicedToArray(v, 4);

            var _v3$ = _slicedToArray(_v3[3], 1);

            selected = _v3$[0];

            if (v[5] !== undefined) {
              var _v4 = _slicedToArray(v, 6);

              selected = _v4[5];
            }

            if (_this.resetDefault()) {
              selected = '';
            }

            if (idx === 0) {
              // tell R that new element was addded to array: (ID)
              Shiny.setInputValue(rAddID, [elID, selected, k], {
                priority: 'event'
              });
            }

            arrayContent += InputArray.createSelectDepInput(k, elID, rAddID, v[1], v[2], v[3], v[4], selected, v[6]);
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
              var _v5 = _slicedToArray(v, 3);

              value = _v5[2];
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
              var _v6 = _slicedToArray(v, 3);

              value = _v6[2];
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
              var _v7 = _slicedToArray(v, 3);

              value = _v7[2];
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
            arrayLabel = _this2.elID;
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

      if (this.elInArray !== undefined) {
        this.elInArray.splice($.inArray(arrayLabel, this.elInArray), 1);
      }

      $("#".concat(this.arrayID, "_wrapper .btn-add-array-el:disabled")).prop('disabled', false);
      Shiny.setInputValue("remove_".concat(this.rObserveID), [this.arrayID, arrayLabel, elID], {
        priority: 'event'
      });
      this.freeElIDs.push(elID);
      $("#".concat(this.arrayID).concat(elID, "_wrapper")).remove();
    }
  }, {
    key: "registerChangeHandlers",
    value: function registerChangeHandlers(elements, rAddID, elID, options) {
      var _this3 = this;

      var htmlID = arguments.length > 4 && arguments[4] !== undefined ? arguments[4] : elID;
      var notFirstIdx = arguments.length > 5 && arguments[5] !== undefined ? arguments[5] : false;
      var idx = 0;
      $.each(elements, function (k, v) {
        if (v[0] === 'select' || v[0] === 'selectDep') {
          if (!notFirstIdx && idx === 0) {
            $("#".concat(k).concat(htmlID)).selectize({
              onChange: function onChange(value) {
                if (options.updateTxtWithLabel.length) {
                  var alias = outputScalarAliases[outputScalars.findIndex(function (val) {
                    return val === value;
                  })];

                  for (var i = 0; i < options.updateTxtWithLabel.length; i++) {
                    $(options.updateTxtWithLabel[i]).val(alias);
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

            var _v8 = _slicedToArray(v, 2);

            altElements[k] = _v8[1];
            altElements[k].shift();

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

      if (count === 0) {
        delete this.elInArray;
      }

      if (this.freeElIDs.length) {
        return this.freeElIDs.pop();
      }

      return count + 1;
    }
  }, {
    key: "addLabelEl",
    value: function addLabelEl(newLabel) {
      if (this.elInArray !== undefined) {
        if ($.inArray(newLabel, this.elInArray) !== -1) {
          throw new Error("Label: ".concat(newLabel, " already in use."));
        }

        this.elInArray.push(newLabel);
        return;
      }

      this.elInArray = [newLabel];
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
  }]);

  return InputArrayFactory;
}();



/***/ }),

/***/ "./srcjs/miro_admin.js":
/*!*****************************!*\
  !*** ./srcjs/miro_admin.js ***!
  \*****************************/
/*! exports provided: removeArrayEl, toggleDepContainer, addArrayDataEl */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "removeArrayEl", function() { return removeArrayEl; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "toggleDepContainer", function() { return toggleDepContainer; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "addArrayDataEl", function() { return addArrayDataEl; });
/* harmony import */ var _input_array__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./input_array */ "./srcjs/input_array.js");
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
  Shiny.setInputValue(rAddID, [elID, value, arrayID, 'change'], {
    priority: 'event'
  });
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
      dyAnnotation_text: ['text', lang.addDyAnnotation.text, outputScalarAliases[1]],
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
      dy_dyShading: ['select', lang.addDyShading.dyDyShading, outputScalars, outputScalarAliases],
      dyShading_up: ['select', lang.addDyShading.up, outputScalars, outputScalarAliases],
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
      marker_color: ['color', lang.addBarDataEl.color, 'rgb(0,0,0)'],
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
      marker_color: ['color', lang.addScatterDataEl.color, 'rgb(0,0,0)'],
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
      line_color: ['color', lang.addLineDataEl.color, 'rgb(0,0,0)'],
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
      marker_symbol: ['select', lang.addBubbleDataEl.symbol, ['circle', 'circle-open', 'circle-dot', 'circle-open-dot', 'square', 'square-open', 'square-dot', 'square-open-dot', 'diamond', 'diamond-open', 'diamond-dot', 'diamond-open-dot', 'cross', 'cross-open', 'cross-dot', 'cross-open-dot', 'x', 'x-open', 'x-dot', 'x-open-dot', 'triangle-up', 'triangle-up-open', 'triangle-up-dot', 'triangle-up-open-dot', 'triangle-down', 'triangle-down-open', 'triangle-down-dot', 'triangle-down-open-dot', 'triangle-left', 'triangle-left-open', 'triangle-left-dot', 'triangle-left-open-dot', 'triangle-right', 'triangle-right-open', 'triangle-right-dot', 'triangle-right-open-dot', 'triangle-ne', 'triangle-ne-open', 'triangle-ne-dot', 'triangle-ne-open-dot', 'triangle-se', 'triangle-se-open', 'triangle-se-dot', 'triangle-se-open-dot', 'triangle-sw', 'triangle-sw-open', 'triangle-sw-dot', 'triangle-sw-open-dot', 'triangle-nw', 'triangle-nw-open', 'triangle-nw-dot', 'triangle-nw-open-dot', 'pentagon', 'pentagon-open', 'pentagon-dot', 'pentagon-open-dot', 'hexagon', 'hexagon-open', 'hexagon-dot', 'hexagon-open-dot', 'hexagon2', 'hexagon2-open', 'hexagon2-dot', 'hexagon2-open-dot', 'octagon', 'octagon-open', 'octagon-dot', 'octagon-open-dot', 'star', 'star-open', 'star-dot', 'star-open-dot', 'hexagram', 'hexagram-open', 'hexagram-dot', 'hexagram-open-dot', 'star-triangle-up', 'star-triangle-up-open', 'star-triangle-up-dot', 'star-triangle-up-open-dot', 'star-triangle-down', 'star-triangle-down-open', 'star-triangle-down-dot', 'star-triangle-down-open-dot', 'star-square', 'star-square-open', 'star-square-dot', 'star-square-open-dot', 'star-diamond', 'star-diamond-open', 'star-diamond-dot', 'star-diamond-open-dot', 'diamond-tall', 'diamond-tall-open', 'diamond-tall-dot', 'diamond-tall-open-dot', 'diamond-wide', 'diamond-wide-open', 'diamond-wide-dot', 'diamond-wide-open-dot', 'hourglass', 'hourglass-open', 'bowtie', 'bowtie-open', 'circle-cross', 'circle-cross-open', 'circle-x', 'circle-x-open', 'square-cross', 'square-cross-open', 'square-x', 'square-x-open', 'diamond-cross', 'diamond-cross-open', 'diamond-x', 'diamond-x-open', 'cross-thin', 'cross-thin-open', 'x-thin', 'x-thin-open', 'asterisk', 'asterisk-open', 'hash', 'hash-open', 'hash-dot', 'hash-open-dot', 'y-up', 'y-up-open', 'y-down', 'y-down-open', 'y-left', 'y-left-open', 'y-right', 'y-right-open', 'line-ew', 'line-ew-open', 'line-ns', 'line-ns-open', 'line-ne', 'line-ne-open', 'line-nw', 'line-nw-open'], lang.addBubbleDataEl.symbolChoices],
      marker_colorDep: ['selectDep', [lang.addBubbleDataEl.colorCheck, 'color', lang.addBubbleDataEl.colorCheckTrue, 'rgb(0,0,0)'], lang.addBubbleDataEl.colorCheckFalse, scalarIndices, scalarIndexAliases],
      marker_size: ['select', lang.addBubbleDataEl.size, scalarIndices, scalarIndexAliases],
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
  if ($("#".concat(arrayID, "_wrapper")).is(':visible')) {
    addArrayDataEl(arrayID, defaults, true);
  } else {
    setTimeout(addArrayDataElWrapper, 200, arrayID, defaults);
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