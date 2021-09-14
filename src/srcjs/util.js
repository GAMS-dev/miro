/* global $:false HTMLWidgets:false Chart:false Shiny:false */

export function sleep(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

export class LoadingScreen {
  constructor() {
    this.activeCount = 0;
  }

  show(delay) {
    this.activeCount += 1;
    setTimeout(() => {
      if (this.activeCount > 0) {
        $('#loading-screen').show();
      }
    }, delay);
  }

  hide() {
    if (this.activeCount === 0) {
      return;
    }
    this.activeCount -= 1;
    if (this.activeCount === 0) {
      $('#loading-screen').hide();
    }
  }
}

function changeChartjsTheme(dark = false) {
  if (typeof Chart === 'undefined') {
    return;
  }
  if (dark) {
    Chart.defaults.color = 'white';
  } else {
    Chart.defaults.color = '#666';
  }
  Chart.helpers.each(Chart.instances, (instance) => {
    instance.update();
  });
}

export function changeTheme(dark = false) {
  changeChartjsTheme(dark);
}

export function changeActiveButtons(tabId) {
  switch (tabId) {
    case 'inputData':
      $('#btImport').show();
      $('.btSolve').show();
      $('#btInterrupt').hide();
      $('.btSplitView').hide();
      $('#btCompareScen').hide();
      break;

    case 'outputData':
      $('#btImport').hide();
      $('.btSolve').hide();
      $('#btInterrupt').hide();
      $('.btSplitView').hide();
      $('#btCompareScen').hide();
      break;

    case 'gamsinter':
      $('#btImport').hide();
      $('.btSolve').hide();
      $('#btInterrupt').show();
      $('.btSplitView').hide();
      $('#btCompareScen').hide();
      break;

    case 'scenarios':
      $('#btImport').hide();
      $('.btSolve').hide();
      $('#btInterrupt').hide();
      $('.btSplitView').show();
      if ($('#btCompareScen').attr('data-noshow') === 'false') {
        $('#btCompareScen').show();
      }
      break;

    default:
      $('#btImport').hide();
      $('.btSolve').hide();
      $('#btInterrupt').hide();
      $('.btSplitView').hide();
      $('#btCompareScen').hide();
  }
}

export function switchTabInTabset(tabsetID, tabValue) {
  const tabset = $(`#${tabsetID}`);
  tabset.find(`a[data-value='${tabValue}']`).tab('show');
}

export function removeModal() {
  $('#shiny-modal-wrapper').find('.modal').modal('hide');
}

export function switchTab(el) {
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

export function isInputEl(id) {
  if ($(id).parents('.form-group').length) {
    return true;
  }
  return false;
}
export function rerenderDygraph(delay = 100) {
  try {
    setTimeout(() => {
      HTMLWidgets.getInstance($('.dygraphs:visible').get(0)).dygraph.resize();
    }, delay);
  } catch (e) {
    // continue regardless of error
  }
}
export function rerenderHot(delay = 100) {
  setTimeout(() => {
    const el = $('.rhandsontable:visible').get(0);
    if (el !== undefined) {
      const hotInstance = HTMLWidgets.getInstance(el);
      if (hotInstance !== undefined) {
        hotInstance.hot.render();
      }
    }
  }, delay);
}

export function showHideEl(el, delay, msg = null) {
  if (msg !== null) {
    $(el).text(msg);
  }
  $(el).show().delay(delay).fadeOut();
}

export function scrollDown(id, delay = 500) {
  setTimeout(() => {
    $(id).animate({
      scrollTop: $(id)[0].scrollHeight - $(id)[0].clientHeight,
    }, 300);
  }, delay);
}

export function debounce(fn, ms = 0) {
  let timeoutId;
  return function (...args) {
    clearTimeout(timeoutId);
    timeoutId = setTimeout(() => fn.apply(this, args), ms);
  };
}

export const colorPickerBinding = new Shiny.InputBinding();
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

/*! Copyright 2011, Ben Lin (http://dreamerslab.com/)

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
export function getActualHeight($target, method, options) {
  const defaults = {
    absolute: false,
    includeMargin: false,
    display: 'block',
  };

  const configs = $.extend(defaults, options);

  const tmp = [];
  let style = '';
  let $hidden;

  const fix = function () {
    // get all hidden parents
    $hidden = $target.parents().addBack().filter(':hidden');
    style += `visibility: hidden !important; display: ${configs.display} !important; `;

    if (configs.absolute === true) style += 'position: absolute !important; ';

    // save the origin style props
    // set the hidden el css to be got the actual value later
    $hidden.each(function () {
      // Save original style. If no style was set, attr() returns undefined
      const $this = $(this);
      const thisStyle = $this.attr('style');

      tmp.push(thisStyle);
      // Retain as much of the original style as possible, if there is one
      $this.attr('style', thisStyle ? `${thisStyle};${style}` : style);
    });
  };

  const restore = function () {
    // restore origin style values
    $hidden.each(function (i) {
      const $this = $(this);
      const tmpI = tmp[i];

      if (tmpI === undefined) {
        $this.removeAttr('style');
      } else {
        $this.attr('style', tmpI);
      }
    });
  };

  fix();
  // get the actual value with user specific methed
  // it can be 'width', 'height', 'outerWidth', 'innerWidth'... etc
  // configs.includeMargin only works for 'outerWidth' and 'outerHeight'
  const actual = /(outer)/.test(method)
    ? $target[method](configs.includeMargin)
    : $target[method]();

  restore();
  // IMPORTANT, this plugin only return the value of the first element
  return actual;
}
