HTMLWidgets.widget({

  name: 'chartjs',

  type: 'output',

  initialize: function(el, width, height) {
    return {};
  },

  renderValue: function(el, x, instance) {
    if (x.debug) console.log(x);
    helpers = Chart.helpers;

    var datasets = [];
    var data = [];

    // Final chart data
    data  = {
      labels: x.data.labels,
      datasets: x.data.datasets,
      legendTitle: x.legendTitle
    };

    // Get element in page
    var canvas = document.getElementById(el.id);
    var ctx = canvas.getContext("2d");

    // If a previous chart exists, destroy it
    if (instance.cjs) {
      instance.cjs.destroy();
      instance.cjs = null;
    }

    // Handle options
    var chartOptions = x.options;

    // Handle scales
    if (x.scales != null){
      chartOptions.scales = {};
      if (x.scales.x != null) {
        chartOptions.scales.xAxes = x.scales.x;
      }
      if (x.scales.y != null) {
        chartOptions.scales.yAxes = x.scales.y;
      }
    }

    if (x.scale != null){
      chartOptions.scale = x.scale;
    }
//    chartOptions.dragData = true;
//    chartOptions.dragX = false;
//    chartOptions.dragDataRound = 6;
//    chartOptions.dragOptions = {
//      showTooltip: true
//    };
    if(chartOptions.dragData === true){
      chartOptions.onDrag = function (e, datasetIndex, index, value) {
        // change cursor style to grabbing during drag action
        e.target.style.cursor = 'grabbing';
        // where e = event
      };
      chartOptions.onDragEnd = function (e, datasetIndex, index, value) {
        // restore default cursor style upon drag release
        e.target.style.cursor = 'default';
        // where e = event
        Shiny.onInputChange(el.id, {col: datasetIndex + 1, row: index + 1, value: value});
      };
      chartOptions.hover = {
        onHover: function(e) {
          // indicate that a datapoint is draggable by showing the 'grab' cursor when hovered
          const point = this.getElementAtEvent(e);
          if (point.length) e.target.style.cursor = 'grab';
          else e.target.style.cursor = 'default';
        }
      };
    }

    // Create actual chart
    instance.cjs = new Chart(ctx, {
          type: x.type,
          data: data,
          options: chartOptions
          });
    if (x.debug) console.log(instance.cjs);
  },

  resize: function(el, width, height, instance) {
    if (instance.cjs)
      instance.cjs.resize();
  }

});
