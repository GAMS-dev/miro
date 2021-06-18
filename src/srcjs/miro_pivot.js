/* global $:false Shiny: false */

export function activateMiroPivotPresentation(id) {
  console.log(id);

  $(`#${id}container .row`).slice(2, 3).children('.col-sm-2').animate({ width: 'hide' }, 350, () => {
    $(`#${id}container .row`).slice(2, 3).children('.col-sm-10').removeClass('col-sm-10', 350)
      .addClass('col-12', 350);
    $(`#${id}container .row`).slice(0, 2).slideUp(500, () => {
      $(`#${id}container .row`).slice(0, 1).find('.presentation-hide').hide();

      $(`#${id}container .row`).slice(0, 1).children('.col-sm-2')[0].setAttribute('style', 'padding: 0px');
      $(`#${id}container .row`).slice(0, 1).find('.dropdown.presentation')[0].setAttribute('style', 'margin-top: 0px');
      $(`#${id}container .row`).slice(0, 1).find('.presentation-show').show(0, () => {
        $(`#${id}container .row`).slice(0, 1).slideDown(750);
      });
    });
  });
  $(`#${id}container .row.data-section`).animate({ height: 'toggle' }, 750);
}

export function deactivateMiroPivotPresentation(id) {
  console.log(id);

  $(`#${id}container .row.data-section`).slideUp(500);

  $(`#${id}container .row`).slice(0, 1).slideUp(500, () => {
    $(`#${id}container .row`).slice(0, 1).find('.dropdown.presentation')[0].setAttribute('style', 'margin-top: 10px');
    $(`#${id}container .row`).slice(0, 1).children('.col-sm-2')[0].setAttribute('style', 'padding: 1em');
    $(`#${id}container .row`).slice(0, 1).find('.presentation-hide').show();

    if (['table', 'heatmap'].includes(Shiny.shinyapp.$inputValues[$(`#${id}pivotRenderer`)])) {
      $(`#${id}container .row`).slice(0, 1).find($(`#${id}downloadCsv`)).show();
      $(`#${id}container .row`).slice(0, 1).find($(`#${id}downloadPng`)).hide();
    } else {
      $(`#${id}container .row`).slice(0, 1).find($(`#${id}downloadCsv`)).hide();
      $(`#${id}container .row`).slice(0, 1).find($(`#${id}downloadPng`)).show();
    }
    $(`#${id}container .row`).slice(0, 1).find('.presentation-show').slideUp(0, () => {
      $(`#${id}container .row`).slice(0, 2).slideDown(500, () => {
        $(`#${id}container .row`).slice(2, 3).children('.col-sm-2').animate({ width: 'show' }, 750);
        $(`#${id}container .row`).slice(2, 3).children('.col-12').toggleClass('col-12 col-sm-10', 750);
      });
    });
  });
}
