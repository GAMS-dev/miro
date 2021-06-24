/* global $:false Shiny: false */

function escapeHTML(text) {
  return $('<div/>').text(text).html();
}

const domainConfig = { indexList: {}, indexHTML: {} };

function updatePivotPresentationData(id) {
  const types = ['row', 'filter', 'col', 'aggregation'];
  types.forEach((item) => {
    domainConfig.indexList[item] = Shiny.shinyapp.$inputValues[`${id}${item}IndexList:sortablejs.rank_list`];
    domainConfig.indexHTML[item] = domainConfig.indexList[item] == null ? '' : domainConfig.indexList[item].map((indexName) => {
      const filterElements = Shiny.shinyapp.$inputValues[`${id}filter_${indexName}`];
      const filterHTML = item === 'row' || filterElements == null ? ''
        : filterElements.map((filterElement) => `<div class="uel-item-presentation">${escapeHTML(filterElement)}</div>`).join('');
      const indexAlias = $(`#${id}${item}IndexList [data-rank-id='${indexName}']`).text();
      return `<li class="list-presentation">
                <div class="row row-presentation">
                  <div class="col-sm-4 column-presentation">
                    <div class="drop-index-item-presentation">${escapeHTML(indexAlias)}</div>
                  </div>
                  <div class="col-sm-8 column-presentation">
                  ${filterHTML}
                  </div>
                </div>
              </li>`;
    }).join('');
  });

  $(`#${id}rowsPresentation`).html(domainConfig.indexHTML.filter + domainConfig.indexHTML.row);
  $(`#${id}colsPresentation`).html(domainConfig.indexHTML.col);
  $(`#${id}aggPresentation`).html(domainConfig.indexHTML.aggregation);

  const aggregationType = $(`#${id}aggregationFunction`).next().find('.item')[0].innerText;
  $(`#${id}container .data-section-header`).slice(-1).html(aggregationType);
}

export function activateMiroPivotPresentation(id) {
  $(`#${id}pivotTable`).on('shiny:value', (e) => {
    if (e.value.x != null) {
      $(`#${id}container .presentation-show .bt-export-canvas`).hide();
      updatePivotPresentationData(id);
    }
  });
  $(`#${id}pivotChart`).on('shiny:value', (e) => {
    if (e.value.x != null) {
      $(`#${id}container .presentation-show .bt-export-canvas`).show();
      updatePivotPresentationData(id);
    }
  });

  if (document.getElementById(`${id}pivotTable`).getElementsByClassName('dataTable').length > 0) {
    $(`#${id}container .presentation-show .bt-export-canvas`).hide();
  } else {
    $(`#${id}container .presentation-show .bt-export-canvas`).show();
  }

  updatePivotPresentationData(id);
  $(`#${id}container .row`).slice(2, 3).children('.col-sm-2').animate({ width: 'hide' }, 250, () => {
    $(`#${id}container .row`).slice(2, 3).children('.col-sm-10').toggleClass('col-sm-10 col-12', 500);
    $(`#${id}container .row`).slice(0, 2).slideUp(500, () => {
      $(`#${id}container .row`).slice(0, 1).find('.presentation-hide').hide();
      $(`#${id}container .row`).slice(0, 1).children('.col-sm-2')[0].setAttribute('style', 'padding: 0px');
      $(`#${id}container .row`).slice(0, 1).find('.dropdown.presentation')[0].setAttribute('style', 'margin-top: 0px');
      $(`#${id}container .row`).slice(0, 1).find('.presentation-show').show(0, () => {
        $(`#${id}container .row`).slice(0, 1).slideDown(500, () => {
          $(`#${id}pivotTable`).data('datatable').draw();
        });
      });
    });
  });
  $(`#${id}container .row.data-section`).animate({ height: 'toggle' }, 750);
}

export function deactivateMiroPivotPresentation(id) {
  $(`#${id}pivotTable`).off('shiny:value');
  $(`#${id}pivotChart`).off('shiny:value');
  $(`#${id}container .row.data-section`).slideUp(350);

  $(`#${id}container .row`).slice(0, 1).slideUp(350, () => {
    $(`#${id}container .row`).slice(0, 1).find('.dropdown.presentation')[0].setAttribute('style', 'margin-top: 10px');
    $(`#${id}container .row`).slice(0, 1).children('.col-sm-2')[0].setAttribute('style', 'padding: 1em');
    $(`#${id}container .row`).slice(0, 1).find('.presentation-hide').show();

    if (['table', 'heatmap'].includes(Shiny.shinyapp.$inputValues[$(`#${id}pivotRenderer`)])) {
      $(`#${id}container .row`).slice(0, 1).find($(`#${id}downloadPng`)).hide();
    } else {
      $(`#${id}container .row`).slice(0, 1).find($(`#${id}downloadPng`)).show();
    }
    $(`#${id}container .row`).slice(0, 1).find('.presentation-show').slideUp(0, () => {
      $(`#${id}container .row`).slice(0, 2).slideDown(500, () => {
        $(`#${id}container .row`).slice(2, 3).children('.col-12').toggleClass('col-12 col-sm-10');
        $(`#${id}container .row`).slice(2, 3).children('.col-sm-2').animate({ width: 'show' }, 500, () => {
          $(`#${id}pivotTable`).data('datatable').draw();
        });
      });
    });
  });
}
