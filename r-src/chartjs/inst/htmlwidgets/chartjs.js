HTMLWidgets.widget({
  name: 'chartjs',

  type: 'output',

  initialize: function (el, width, height) {
    if (
      window.matchMedia &&
      window.matchMedia('(prefers-color-scheme: dark)').matches
    ) {
      Chart.defaults.color = 'white';
      Chart.defaults.borderColor = '#9c9c9c';
    } else {
      Chart.defaults.color = '#666';
    }
    return {};
  },

  renderValue: function (el, x, instance) {
    if (x.debug) console.log(x);
    helpers = Chart.helpers;

    var data = [];

    // Final chart data
    data = {
      labels: x.data.labels,
      datasets: x.data.datasets,
      legendTitle: x.legendTitle,
    };

    // Get element in page
    var canvas = document.getElementById(el.id);
    var ctx = canvas.getContext('2d');

    // If a previous chart exists, destroy it
    if (instance.cjs) {
      instance.cjs.destroy();
      instance.cjs = null;
    }

    // Handle options
    var chartOptions = x.options;

    // Handle scales
    if (x.scales != null) {
      chartOptions.scales = x.scales;
    }
    //    chartOptions.dragData = true;
    //    chartOptions.dragX = false;
    //    chartOptions.dragDataRound = 6;
    //    chartOptions.dragOptions = {
    //      showTooltip: true
    //    };
    //    if(chartOptions.dragData === true){
    //      chartOptions.onDrag = function (e, datasetIndex, index, value) {
    //        // change cursor style to grabbing during drag action
    //        e.target.style.cursor = 'grabbing';
    //        // where e = event
    //      };
    //      chartOptions.onDragEnd = function (e, datasetIndex, index, value) {
    //        // restore default cursor style upon drag release
    //        e.target.style.cursor = 'default';
    //        // where e = event
    //        Shiny.onInputChange(el.id, {col: datasetIndex + 1, row: index + 1, value: value});
    //      };
    //      chartOptions.hover = {
    //        onHover: function(e) {
    //          // indicate that a datapoint is draggable by showing the 'grab' cursor when hovered
    //          const point = this.getElementAtEvent(e);
    //          if (point.length) e.target.style.cursor = 'grab';
    //          else e.target.style.cursor = 'default';
    //        }
    //      };
    //    }

    // Create actual chart
    instance.cjs = new Chart(ctx, {
      type: x.type,
      data: data,
      options: chartOptions,
    });
    if (canvas.onclick == null) {
      var lastClicked = null;
      canvas.onclick = function () {
        if (lastClicked == null) {
          lastClicked = new Date().getTime();
        } else {
          if (new Date().getTime() - lastClicked < 500) {
            instance.cjs.resetZoom();
            lastClicked = null;
          } else {
            lastClicked = new Date().getTime();
          }
        }
      };
    }
    if (x.debug) console.log(instance.cjs);
  },

  resize: function (el, width, height, instance) {
    if (instance.cjs) instance.cjs.resize();
  },
});
