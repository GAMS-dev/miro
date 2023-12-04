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
(function(b,c){var $=b.jQuery||b.Cowboy||(b.Cowboy={}),a;$.throttle=a=function(e,f,j,i){var h,d=0;if(typeof f!=="boolean"){i=j;j=f;f=c}function g(){var o=this,m=+new Date()-d,n=arguments;function l(){d=+new Date();j.apply(o,n)}function k(){h=c}if(i&&!h){l()}h&&clearTimeout(h);if(i===c&&m>e){l()}else{if(f!==true){h=setTimeout(i?k:l,i===c?e-m:e)}}}if($.guid){g.guid=j.guid=j.guid||$.guid++}return g};$.debounce=function(d,e,f){return f===c?a(d,e,false):a(d,f,e!==false)}})(window);
/* eslint-enable */
/* eslint no-console: ["error", { allow: ["warn", "error"] }] */

class InputArray {
  arrayID;

  rObserveID;

  options;

  isDisabled = false;

  indexEl;

  elCount = 0;

  elInArray = [];

  freeElIDs = [];

  constructor(arrayID, options = {}, rObserveID = 'array_el') {
    this.arrayID = arrayID;
    this.rObserveID = rObserveID;
    this.options = options;

    if (options.uniqueItems === true) {
      this.options.elRequired = true;
    } else if (options.elRequired === undefined) {
      this.options.elRequired = true;
    }
    if (options.updateTxtWithLabel === undefined) {
      this.options.updateTxtWithLabel = [];
    }
  }

  createEl(elements, newEl = true) {
    if (this.isDisabled) {
      console.warn('Array is disabled. Will not create new element.');
      return;
    }
    const rAddID = `add_${this.rObserveID}`;
    const elID = this.incElCount();
    this.options.updateTxtWithLabel = [];

    let arrayContent = `<div id="${this.arrayID}${elID}_wrapper" class="config-array-el">\n`;
    let idx = 0;
    let label = '';
    let labelID;

    $.each(elements, (k, v) => {
      let selected;
      let value;
      let choices = [];
      let aliases = [];

      switch (v[0]) {
        case 'select':
          if (idx === 0) {
            if (this.options.elRequired === true) {
              // as labels need to be unique, set default to option that is not already in use
              this.indexEl = k;

              if (v[2].length - 1 === this.elCount) {
                this.isDisabled = true;
                $(`#${this.arrayID}_wrapper .btn-add-array-el`).prop('disabled', true);
              }
              for (let i = 0; i < v[2].length; i += 1) {
                if (this.elInArray.indexOf(v[2][i]) === -1) {
                  choices.push(v[2][i]);
                  if (typeof v[3] !== 'undefined') {
                    aliases.push(v[3][i]);
                  }
                }
              }
              if (typeof v[4] === 'undefined' || !choices.includes(v[4])) {
                [selected] = choices;
              } else {
                [,,,, selected] = v;
              }
              labelID = selected;
              [label] = aliases;
              this.addLabelEl(elID, selected);
              for (let i = 1; i < elID; i += 1) {
                const labelEl = $(`#${k}${i}`);
                if (labelEl.length) {
                  labelEl[0].selectize.disable();
                }
              }
            } else {
              [,, choices, aliases, selected] = v;
              if (selected == null) {
                [selected] = choices;
              }
              [label] = aliases;
            }
            // tell R that new element was addded to array: (ID, label of element)
            if (newEl) {
              Shiny.setInputValue(rAddID, [elID, selected, k], { priority: 'event' });
            }
          } else {
            [,, choices, aliases, selected] = v;
            if (selected === '.index') {
              selected = labelID;
            }
          }

          arrayContent += InputArray.createSelectInput(
            k,
            elID,
            v[1],
            choices,
            aliases,
            selected,
            v[5],
            v[6],
            v[7],
          );
          break;
        case 'selectDep':
          [,,, [selected]] = v;

          if (v[5] !== undefined) {
            [,,,,, selected] = v;
          }
          if (selected === '.index') {
            selected = labelID;
          }
          if (idx === 0) {
            // tell R that new element was addded to array: (ID)
            if (newEl) {
              Shiny.setInputValue(rAddID, [elID, selected, k], { priority: 'event' });
            }
          }
          arrayContent += InputArray.createSelectDepInput(
            k,
            elID,
            (idx !== 0 ? '' : rAddID),
            v[1],
            v[2],
            v[3],
            v[4],
            selected,
            v[6],
            newEl,
          );
          break;
        case 'text':
          value = (v[2] !== undefined ? v[2] : '');
          if (idx === 0 && this.options.elRequired === false) {
            // tell R that new element was addded to array: (ID)
            if (newEl) {
              Shiny.setInputValue(rAddID, [elID, value, k], { priority: 'event' });
            }
          }
          if (value === 'label') {
            value = label;
            this.options.updateTxtWithLabel.push(`#${k}${elID}`);
          }

          arrayContent += InputArray.createTextInput(
            k,
            elID,
            v[1],
            value,
            v[3],
            newEl,
          );
          break;
        case 'numeric':

          [,, value] = v;

          if (idx === 0 && this.options.elRequired === false) {
            // tell R that new element was addded to array: (ID)
            if (newEl) {
              Shiny.setInputValue(rAddID, [elID, value, k], { priority: 'event' });
            }
          }
          arrayContent += InputArray.createNumericInput(
            k,
            elID,
            v[1],
            value,
            v[3],
            v[4],
            v[5],
            newEl,
          );
          break;
        case 'checkbox':

          [,, value] = v;

          if (idx === 0 && this.options.elRequired === false) {
            // tell R that new element was addded to array: (ID)
            if (newEl) {
              Shiny.setInputValue(rAddID, [elID, value, k], { priority: 'event' });
            }
          }
          arrayContent += InputArray.createCheckboxInput(k, elID, v[1], value, newEl);
          break;
        case 'color':
          [,, value] = v;
          arrayContent += InputArray.createColorPickerInput(k, elID, v[1], value, newEl);
          break;
        case 'optionsStart':
          arrayContent += `${'<div class="form-group" style="min-height:30px;">'
                + '<h4 class="box-title option-section-header" style="cursor:pointer;font-weight:bold;" '
                + 'onclick="$(this).next().toggle();$(this).children(\'.fa\').toggleClass(\'fa-plus fa-minus\')">'}${
            v[1]} <i class="fa fa-plus" role="presentation" aria-label="More options"></i></h4><div class="option-section"${(v[2] === false) ? '' : ' style="display:none;"'}>`;
          break;
        case 'optionsEnd':
          arrayContent += '</div></div>';
          break;
        default:
          throw new TypeError(`Unknown element type: ${v[0]}`);
      }
      idx += 1;
    });
    arrayContent += `${(!this.options.elRequired || this.options.uniqueItems === true || elID > 1)
      ? `<button type="button" onclick="Miro.removeArrayEl('${this.arrayID}','${elID}\
')" class="btn btn-default bt-icon"><i class="far fa-minus-square" role="presentation" aria-label="Remove array element"></i></button>\n` : ''}<hr></div>`;
    $(`#${this.arrayID}_wrapper .array-wrapper`).append(arrayContent);
    this.registerChangeHandlers(elements, rAddID, elID, this.options);
    this.elCount += 1;
  }

  removeElAtomic(element, idxRaw, elID) {
    let el = null;
    let arrayLabel = null;
    let idx = idxRaw;

    $.each(element, (k, v) => {
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
        this.removeElAtomic($(v).children('.option-section').first().children('.form-group'), idx, elID);
      } else if ($(v).children('input[type="text"]').length) {
        el = $(v).children('input[type="text"]');
        if (idx === 0) {
          arrayLabel = elID;
        }
        el.remove();
      } else if ($(v).children('.dep-el').length) {
        const depEl = $(v).children('.dep-el');
        if (idx === 0) {
          arrayLabel = elID;
        }
        this.removeElAtomic(depEl.eq(0).children('.form-group'), idx, elID);
        this.removeElAtomic(depEl.eq(1).children('.form-group'), idx, elID);
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

  removeArrayEl(elID) {
    const idx = 0;
    const arrayLabel = this.removeElAtomic($(`#${this.arrayID}${elID}_wrapper`).children('.form-group'), idx, elID);

    if (arrayLabel.length === 0) {
      console.error(`Could not find identifier for array element: ${
        this.arrayID} (label ID: ${this.arrayID}${elID}).`);
    }

    this.elCount -= 1;
    delete this.elInArray[elID];

    if (this.elCount === 1 && this.indexEl !== undefined) {
      const labelEl = $(`#${this.indexEl}1`);
      if (labelEl.length) {
        labelEl[0].selectize.enable();
      }
    }
    this.isDisabled = false;
    $(`#${this.arrayID}_wrapper .btn-add-array-el:disabled`).prop('disabled', false);
    Shiny.setInputValue(`remove_${this.rObserveID}`, [this.arrayID, arrayLabel, elID], {
      priority: 'event',
    });
    this.freeElIDs.push(elID);
    $(`#${this.arrayID}${elID}_wrapper`).remove();
  }

  updateArrayEl(elID, newVal) {
    if (this.elInArray[elID] !== undefined) {
      this.elInArray[elID] = newVal;
    }
  }

  registerChangeHandlers(elements, rAddID, elID, options, htmlID = elID, notFirstIdx = false) {
    let idx = 0;
    const { arrayID } = this;
    $.each(elements, (k, v) => {
      if (v[0] === 'select' || v[0] === 'selectDep') {
        if (!notFirstIdx && idx === 0) {
          $(`#${k + htmlID}`).selectize({
            onChange(value) {
              if (options.elRequired) {
                Miro.updateArrayEl(arrayID, elID, value);
              }
              if (options.updateTxtWithLabel.length) {
                const alias = $(`#${k}${htmlID}`).text();
                if (typeof (alias) !== 'undefined') {
                  for (let i = 0; i < options.updateTxtWithLabel.length; i += 1) {
                    $(options.updateTxtWithLabel[i]).val(alias);
                  }
                }
              }

              Shiny.setInputValue(rAddID, [elID, value, k, 'change'], { priority: 'event' });
            },
          });
        } else {
          const selectizeOptions = $(`#${k + htmlID}`)
            .parent()
            .find(`script[data-for="${k + htmlID}"]`);
          const addOptions = selectizeOptions.length ? JSON.parse(selectizeOptions.text()) : {};
          $(`#${k}${htmlID}`).selectize($.extend({
            onChange(value) {
              Shiny.setInputValue(k, [elID, value], { priority: 'event' });
            },
          }, addOptions));
        }
        if (v[0] === 'selectDep') {
          const altElements = {};
          [, altElements[k]] = v;
          altElements[k] = altElements[k].slice(1);

          this.registerChangeHandlers(
            altElements,
            rAddID,
            elID,
            options,
            `_alt${elID}`,
            idx !== 0,
          );
        }
      } else if (v[0] === 'checkbox') {
        if (!notFirstIdx && idx === 0) {
          $(`#${k + htmlID}`).on('change', function () {
            Shiny.setInputValue(rAddID, [elID, $(this).prop('checked'), k, 'change'], { priority: 'event' });
          });
        } else {
          $(`#${k + htmlID}`).on('change', function () {
            Shiny.setInputValue(k, [elID, $(this).prop('checked')], { priority: 'event' });
          });
        }
      } else if (v[0] === 'color') {
        if (!notFirstIdx && idx === 0) {
          $(`#${k + htmlID}`).colorpicker({
            align: 'left',
          }).on('changeColor', $.debounce(500, function () {
            Shiny.setInputValue(rAddID, [elID, $(this).val(), k, 'change'], { priority: 'event' });
          }));
        } else {
          $(`#${k + htmlID}`).colorpicker({
            align: 'left',
          }).on('changeColor', $.debounce(500, function () {
            Shiny.setInputValue(k, [elID, $(this).val()]);
          }));
        }
      } else if (!notFirstIdx && idx === 0) {
        $(`#${k + htmlID}`).on('change', $.debounce(250, function () {
          Shiny.setInputValue(rAddID, [elID, $(this).val(), k, 'change'], { priority: 'event' });
        }));
      } else {
        $(`#${k + htmlID}`).on('change', $.debounce(250, function () {
          Shiny.setInputValue(k, [elID, $(this).val()], { priority: 'event' });
        }));
      }
      idx += 1;
    });
  }

  incElCount() {
    const count = $(`#${this.arrayID}_wrapper .array-wrapper`).children('.config-array-el').length;
    if (this.freeElIDs.length) {
      return (this.freeElIDs.pop());
    }
    return (count + 1);
  }

  addLabelEl(elID, newLabel) {
    if (this.elInArray.indexOf(newLabel) !== -1) {
      throw new Error(`Label: ${newLabel} already in use.`);
    }
    this.elInArray[elID] = newLabel;
  }

  static createSelectInput(
    arrayID,
    elID,
    label,
    choicesRaw,
    aliasesRaw = choicesRaw,
    selectedRaw = '',
    multiple = false,
    create = false,
    options = {},
  ) {
    const id = arrayID + elID;
    let choices = choicesRaw;
    let aliases = aliasesRaw;
    let selected = selectedRaw;
    let optionsHTML = '';
    let optionsJSON = create === true ? { create: true, persist: false, openOnFocus: false } : {};
    optionsJSON = JSON.stringify({ ...optionsJSON, ...options });
    if (!$.isArray(choices)) {
      choices = [choices];
    }
    if (!$.isArray(aliases)) {
      aliases = [aliases];
    }
    if (!$.isArray(selected)) {
      selected = [selected];
    }
    if (choices) {
      /* eslint-disable no-plusplus */
      for (let i = 0, itemLen = choices.length; i < itemLen;
        optionsHTML += `<option value="${choices[i]}"${$.inArray(choices[i], selected) !== -1 ? ' selected' : ''
        }>${aliases[i++]}</option>\n`);
      /* eslint-enable no-plusplus */
    }
    return (`<div class="config-array-err" id="${id}_err" style="display:none;"></div>\
<div class="form-group">\n\
<label class="control-label" for="${id}">${label}</label>\n\
<div><select id="${id}"${multiple === true ? ' multiple="multiple"' : ''}>${optionsHTML}</select>\
${optionsJSON !== '{}' ? `<script type="application/json" data-for="${id}">${optionsJSON}</script>` : ''}\
\n</div>\n</div>`);
  }

  static createTextInput(arrayID, elID, label, value = '', placeholder = null, newEl = true) {
    const id = arrayID + elID;

    if (newEl) {
      Shiny.setInputValue(arrayID, [elID, value], { priority: 'event' });
    }

    return (`<div class="config-array-err" id="${id}_err" style="display:none;"></div>\n`
  + '<div class="form-group">\n'
  + `<label for="${id}">${label}</label>\n`
  + `<input id="${id}" type="text" class="form-control" value="${value}"${placeholder !== null ? ` placeholder="${placeholder}"` : ''}/>\n`
+ '</div>');
  }

  static createNumericInput(
    arrayID,
    elID,
    label,
    value,
    min = null,
    max = null,
    step = null,
    newEl = true,
  ) {
    const id = arrayID + elID;

    if (newEl) {
      Shiny.setInputValue(arrayID, [elID, value], { priority: 'event' });
    }

    return (`${'<div class="form-group">\n'
  + '<label for="'}${id}">${label}</label>\n`
  + `<input id="${id}" type="number" class="form-control" value="${value}"${
    min !== null ? ` min="${min}"` : ''}${max !== null ? ` max="${max}"` : ''
  }${step !== null ? ` step="${step}"` : ''}/>\n`
+ '</div>');
  }

  static createCheckboxInput(arrayID, elID, label, value = false, newEl = true) {
    const id = arrayID + elID;

    if (newEl) {
      Shiny.setInputValue(arrayID, [elID, value], { priority: 'event' });
    }
    return ('<div class="form-group">\n'
      + '<div class="checkbox-simple">\n'
        + '<label>\n'
          + '<div class="checkbox">\n'
            + '<label>\n'
              + `<input id="${id}" type="checkbox"${value === true ? ' checked="checked"' : ''}/>\n`
              + '<span></span>\n'
            + '</label>\n'
          + '</div>\n'
          + `<span class="label-span">${label}</span>\n`
        + '</label>\n'
      + '</div>\n'
    + '</div>');
  }

  static createColorPickerInput(arrayID, elID, label, value = '', newEl = true) {
    const id = arrayID + elID;

    if (newEl) {
      Shiny.setInputValue(arrayID, [elID, value], { priority: 'event' });
    }

    return (`${'<div class="form-group shiny-input-container">\n'
  + '<label for="'}${id}">${label}</label>\n`
  + `<input id="${id}" type="text" class="form-control miro-color-picker" value="${value}"/>\n`
+ '</div>');
  }

  static createSelectDepInput(
    arrayID,
    elID,
    rAddID,
    altEl,
    label,
    choices,
    aliases = choices,
    selected = '',
    isChecked = false,
    newEl = true,
  ) {
    const firstInput = InputArray.createSelectInput(
      arrayID,
      elID,
      label,
      choices,
      aliases,
      selected,
    );
    let secondInput;

    switch (altEl[1]) {
      case 'select':
        secondInput = InputArray.createSelectInput(
          `${arrayID}_alt`,
          elID,
          altEl[2],
          altEl[3],
          altEl[4],
          altEl[5],
          altEl[6],
          altEl[7],
        );
        break;
      case 'text':
        secondInput = InputArray.createTextInput(
          `${arrayID}_alt`,
          elID,
          altEl[2],
          altEl[3],
          altEl[4],
          newEl,
        );
        break;
      case 'numeric':
        secondInput = InputArray.createNumericInput(
          `${arrayID}_alt`,
          elID,
          altEl[2],
          altEl[3],
          altEl[4],
          altEl[5],
          altEl[6],
          newEl,
        );
        break;
      case 'checkbox':
        secondInput = InputArray.createCheckboxInput(
          `${arrayID}_alt`,
          elID,
          altEl[2],
          altEl[3],
          newEl,
        );
        break;
      case 'color':
        secondInput = InputArray.createColorPickerInput(
          `${arrayID}_alt`,
          elID,
          altEl[2],
          altEl[3],
          newEl,
        );
        break;
      default:
        throw new TypeError(`Unknown element type: ${altEl[1]}`);
    }
    const checkboxInput = `${'<div class="form-group col-sm-4">\n'
  + '<label class="cb-label" for="depCb_'}${arrayID}${elID}">${altEl[0]}</label>\n`
  + `<div>\n<label class="checkbox-material" for="depCb_${arrayID}${elID
  }">\n<div class="checkbox">\n<label>\n`
      + `<input id="depCb_${arrayID}${elID}" type="checkbox"${isChecked === true ? ' checked="checked"' : ''} onclick="Miro.toggleDepContainer(this, '${
        arrayID}', '${elID}', '${rAddID}');"/>\n`
      + '<span></span>\n'
    + '</label>\n'
  + '</div>\n'
+ '</div>\n</label>\n</div>';

    return (`<div class="form-group dep-group" style="width:100%;display:inline-block;">\n<div class="dep-el col-sm-8"${isChecked === true ? ' style="display:none"' : ''}>\n${
      firstInput}\n</div>\n<div class="dep-el col-sm-8"${isChecked === true ? '' : ' style="display:none"'}>\n${secondInput
    }\n</div>\n<div>\n${checkboxInput}\n</div>\n</div>`);
  }
}

export default class InputArrayFactory {
  inputArrays = {};

  add(arrayID, elements, options, rObserveID, reinitialize = false, newEl = true) {
    if (reinitialize === false
      && Object.prototype.hasOwnProperty.call(this.inputArrays, arrayID)) {
      this.inputArrays[arrayID].createEl(elements, newEl);
      return;
    }
    this.inputArrays[arrayID] = new InputArray(arrayID, options, rObserveID);
    this.inputArrays[arrayID].createEl(elements, newEl);
  }

  destroy(arrayID) {
    if (Object.prototype.hasOwnProperty.call(this.inputArrays, arrayID)) {
      delete this.inputArrays[arrayID];
    }
  }

  remove(arrayID, elID) {
    if (Object.prototype.hasOwnProperty.call(this.inputArrays, arrayID)) {
      this.inputArrays[arrayID].removeArrayEl(elID);
      return (true);
    }
    return (false);
  }

  update(arrayID, elID, newVal) {
    if (Object.prototype.hasOwnProperty.call(this.inputArrays, arrayID)) {
      this.inputArrays[arrayID].updateArrayEl(elID, newVal);
      return (true);
    }
    return (false);
  }
}
