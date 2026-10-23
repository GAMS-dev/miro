/* global Chart:false, Shiny:false */

const AI_CHART_TYPES = {
  bar: { type: 'bar' },
  stackedbar: { type: 'bar', stacked: true },
  horizontalbar: { type: 'bar', horizontal: true },
  horizontalstackedbar: { type: 'bar', horizontal: true, stacked: true },
  line: { type: 'line' },
  area: { type: 'line', fill: true },
  stackedarea: { type: 'line', fill: true, stacked: true },
  pie: { type: 'pie' },
  doughnut: { type: 'doughnut' },
};
const aiChartSpecs = new WeakMap();
let aiChartCounter = 0;

function getChatLogData() {
  return document.getElementById('miroAiChatLog')?.dataset || {};
}

function buildAiChartConfig(chart) {
  const chartType = AI_CHART_TYPES[chart.type];
  let palette = [];
  try {
    palette = JSON.parse(getChatLogData().chartPalette || '[]');
  } catch (err) {
    palette = [];
  }
  const paletteColor = (idx, dark) => (palette.length
    ? palette[(2 * idx + (dark ? 1 : 0)) % palette.length] : '#666');
  const color = (idx, dark) => chart.datasets[idx]?.color || paletteColor(idx, dark);
  const isPie = chartType.type === 'pie' || chartType.type === 'doughnut';
  const datasets = chart.datasets.map((dataset, idx) => {
    if (isPie) {
      return {
        ...dataset,
        backgroundColor: chart.labels.map((_, labelIdx) => dataset.colors?.[labelIdx]
          || paletteColor(labelIdx, false)),
      };
    }
    if (chartType.type === 'line') {
      let fill = false;
      if (chartType.fill) {
        fill = chartType.stacked && idx > 0 ? '-1' : 'origin';
      }
      return {
        ...dataset,
        borderColor: color(idx, true),
        backgroundColor: color(idx, chartType.fill),
        fill,
      };
    }
    return {
      ...dataset,
      backgroundColor: color(idx, false),
      hoverBackgroundColor: color(idx, true),
    };
  });
  return {
    type: chartType.type,
    data: { labels: chart.labels, datasets },
    options: {
      responsive: true,
      maintainAspectRatio: false,
      indexAxis: chartType.horizontal ? 'y' : 'x',
      plugins: {
        title: { display: Boolean(chart.title), text: chart.title },
        legend: { display: isPie || datasets.length > 1 },
      },
      scales: isPie ? {} : {
        x: { stacked: Boolean(chartType.stacked) },
        y: { stacked: Boolean(chartType.stacked) },
      },
    },
  };
}

function createAiChartElement(chart) {
  const texts = getChatLogData();
  const wrapper = document.createElement('div');
  wrapper.className = 'miro-ai-chart';
  if (!chart || !AI_CHART_TYPES[chart.type] || typeof Chart === 'undefined') {
    wrapper.className += ' miro-ai-chart-unavailable';
    wrapper.textContent = texts.chartUnavailableLabel || '';
    return wrapper;
  }
  aiChartCounter += 1;
  const canvas = document.createElement('canvas');
  canvas.id = `miroAiChart${aiChartCounter}`;
  const toolbar = document.createElement('div');
  toolbar.className = 'miro-ai-chart-toolbar';
  toolbar.style.textAlign = 'right';
  const downloadLink = document.createElement('a');
  downloadLink.className = 'btn btn-default bt-icon bt-export-canvas';
  downloadLink.href = '#';
  downloadLink.download = `${chart.title || chart.id}.png`;
  downloadLink.title = texts.chartDownloadLabel || '';
  downloadLink.dataset.canvasid = canvas.id;
  downloadLink.innerHTML = '<i class="fa fa-file-image" aria-hidden="true"></i>';
  if (chart.canOpenInPivot) {
    const openButton = document.createElement('button');
    openButton.type = 'button';
    openButton.className = 'btn btn-default bt-icon';
    openButton.style.marginRight = '10px';
    openButton.title = texts.chartOpenLabel || '';
    openButton.innerHTML = '<i class="fa fa-table" aria-hidden="true"></i>';
    openButton.addEventListener('click', () => {
      Shiny.setInputValue('miroAiOpenChart', chart.id, { priority: 'event' });
    });
    toolbar.appendChild(openButton);
  }
  toolbar.appendChild(downloadLink);
  const canvasWrapper = document.createElement('div');
  canvasWrapper.className = 'miro-ai-chart-canvas';
  canvasWrapper.style.height = '320px';
  canvasWrapper.style.position = 'relative';
  canvasWrapper.appendChild(canvas);
  wrapper.append(toolbar, canvasWrapper);
  aiChartSpecs.set(canvas, chart);
  return wrapper;
}

export function replaceAiChartBlocks(el) {
  let charts = [];
  try {
    charts = JSON.parse(el.dataset.charts || '[]');
  } catch (err) {
    charts = [];
  }
  delete el.dataset.charts;
  el.querySelectorAll('pre > code.language-mirachart').forEach((code) => {
    const chartId = code.textContent.trim();
    const chart = charts.find((candidate) => candidate.id === chartId);
    code.parentElement.replaceWith(createAiChartElement(chart));
  });
}

export function renderAiChartsIn(root) {
  if (typeof Chart === 'undefined') {
    return;
  }
  Object.values(Chart.instances).forEach((chartInstance) => {
    if (!chartInstance.canvas.isConnected && aiChartSpecs.has(chartInstance.canvas)) {
      chartInstance.destroy();
    }
  });
  root.querySelectorAll('.miro-ai-chart canvas').forEach((canvas) => {
    if (canvas.isConnected && aiChartSpecs.has(canvas) && !Chart.getChart(canvas)) {
      // eslint-disable-next-line no-new
      new Chart(canvas, buildAiChartConfig(aiChartSpecs.get(canvas)));
    }
  });
}
