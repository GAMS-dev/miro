/* global $:false Shiny: false */

function escapeHTML(text) {
  return $('<div/>').text(text).html();
}

const domainConfig = { indexList: {}, indexHTML: {} };

function updatePivotPresentationData(id) {
  const types = ['row', 'filter', 'col', 'aggregation'];
  types.forEach((item) => {
    domainConfig.indexList[item] = Shiny.shinyapp
      .$inputValues[`${id}${item}IndexList:sortablejs.rank_list`];
    if (item === 'row') {
      domainConfig.indexList[item] = domainConfig.indexList[item]
        .filter((indexName) => $(`#${id}${item}IndexList [data-rank-id='${indexName}']`)[0].dataset.colHidden !== 'true');
    }
    domainConfig.indexHTML[item] = domainConfig.indexList[item] == null ? '' : domainConfig.indexList[item].map((indexName) => {
      const filterNode = document.getElementById(`${id}filter_${indexName}`);
      const filterElements = filterNode == null ? null : filterNode.selectize.getValue();
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

  const aggregationType = ` (${$(`#${id}aggregationFunction`).next().find('.item')[0].innerText})`;
  const aggregation = $(`#${id}container .data-section-header`).slice(-2, -1)[0].innerText.split(' (')[0];
  $(`#${id}container .data-section-header`).slice(-2, -1).html(aggregation + aggregationType);
}

export function activateMiroPivotPresentationObservers(id) {
  $(`#${id}pivotTable`).off('shiny:value');
  $(`#${id}pivotChart`).off('shiny:value');
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
}

export function activateMiroPivotPresentation(id) {
  activateMiroPivotPresentationObservers(id);

  if (document.getElementById(`${id}pivotTable`).getElementsByClassName('dataTable').length > 0) {
    $(`#${id}container .presentation-show .bt-export-canvas`).hide();
  } else {
    $(`#${id}container .presentation-show .bt-export-canvas`).show();
  }

  updatePivotPresentationData(id);
  $(`#${id}container .row.table-chart`).children('.col-sm-2').animate({ width: 'hide' }, 250, () => {
    $(`#${id}container .row.table-chart`).children('.col-sm-10').toggleClass('col-sm-10 col-sm-12', 500);
    $(`#${id}container .row.row-agg-filter`).slideUp(500);
    if ($(`#${id}container .row.table-chart .has-edit-buttons`).length > 0) {
      $(`#${id}container .row.table-chart`).children('.col-sm-12').animate({ 'margin-top': '30px' }, 500);
    }
    $(`#${id}container .row.col-filter`).slideUp(500, () => {
      $(`#${id}container .row.row-agg-filter`).find('.presentation-hide').hide();
      $(`#${id}container .row.row-agg-filter`).children('.col-sm-2')[0].setAttribute('style', 'padding: 0px');
      $(`#${id}container .row.row-agg-filter`).find('.dropdown.presentation')[0].setAttribute('style', 'margin-top: 0px');
      $(`#${id}container .dropdown.presentation .miro-pivot-view-button`).css('display', 'none');
      $(`#${id}container .row.row-agg-filter`).find('.presentation-show').show(0, () => {
        $(`#${id}container .row.row-agg-filter`).slideDown(500, () => {
          if (document.getElementById(`${id}pivotTable`).getElementsByClassName('dataTable').length > 0) {
            $(`#${id}pivotTable`).data('datatable').draw();
          }
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

  $(`#${id}container .row.row-agg-filter`).slideUp(350, () => {
    if ($(`#${id}container .row.table-chart .has-edit-buttons`).length > 0) {
      $(`#${id}container .row.table-chart`).children('.col-sm-12').animate({ 'margin-top': '' }, 350, () => {
        $(`#${id}container .row.table-chart`).children('.col-sm-12').css('margin-top', '');
      });
    }
    $(`#${id}container .dropdown.presentation .miro-pivot-view-button`).css('display', '');
    $(`#${id}container .row.row-agg-filter`).find('.dropdown.presentation')[0].setAttribute('style', 'margin-top: 10px');
    $(`#${id}container .row.row-agg-filter`).children('.col-sm-2')[0].setAttribute('style', 'padding: 1em');
    $(`#${id}container .row.row-agg-filter`).find('.presentation-hide').show();
    $(`#${id}container .row.row-agg-filter`).find('.presentation-show').slideUp(0, () => {
      $(`#${id}container .row.col-filter`).slideDown(500);
      $(`#${id}container .row.row-agg-filter`).slideDown(500, () => {
        $(`#${id}container .row.table-chart`).children('.col-sm-12').toggleClass('col-sm-12 col-sm-10');
        $(`#${id}container .row.table-chart`).children('.col-sm-2').animate({ width: 'show' }, 500, () => {
          if (document.getElementById(`${id}pivotTable`).getElementsByClassName('dataTable').length > 0) {
            $(`#${id}pivotTable`).data('datatable').draw();
          }
        });
      });
    });
  });
}
