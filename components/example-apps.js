'use strict'
const path  = require('path');

module.exports = function(miroVersion, apiVersion) {
  return [{
    id: 'pickstock',
    title: 'Stock Selection Optimization',
    description: `Optimization model to pick a small subset of the stocks together with \
some weights, such that this portfolio has a similar behavior to our \
overall Dow Jones index.`,
    logoPath: path.join('static_pickstock', 'pickstock_logo.png'),
    miroversion: miroVersion,
    apiversion: apiVersion,
    usetmpdir: true,
    modesAvailable: [ 'base', 'hcube' ]
  },
  {
    id: 'sudoku',
    title: 'Sudoku solver',
    description: `This model allows you to solve your Sudokus.`,
    logoPath: path.join('static_sudoku', 'sudoku_logo.png'),
    miroversion: miroVersion,
    apiversion: apiVersion,
    usetmpdir: true,
    modesAvailable: [ 'base' ]
  },
  {
    id: 'transport',
    title: 'Transportation problem',
    description: `This problem finds a least cost shipping schedule that meets
requirements at markets and supplies at factories.`,
    miroversion: miroVersion,
    apiversion: apiVersion,
    usetmpdir: true,
    modesAvailable: [ 'base', 'hcube' ]
  },
  {
    id: 'tsp',
    title: 'TSP',
    description: 'Traveling Salesman Problem',
    logoPath: path.join('static_tsp', 'tsp_logo.jpg'),
    miroversion: miroVersion,
    apiversion: apiVersion,
    usetmpdir: true,
    modesAvailable: [ 'base' ]
  },
  {
    id: 'cpack',
    title: 'Circle packing',
    description: 'Packing identical size circles in the unit circle',
    logoPath: path.join('static_cpack', 'cpack_logo.png'),
    miroversion: miroVersion,
    apiversion: apiVersion,
    usetmpdir: true,
    modesAvailable: [ 'base' ]
  },
  {
    id: 'farming',
    title: 'Farming model',
    description: `Crop plant alloction problem with stochastic crop yield`,
    logoPath: path.join('static_farming', 'farming_logo.png'),
    miroversion: miroVersion,
    apiversion: apiVersion,
    usetmpdir: true,
    modesAvailable: [ 'base', 'hcube' ]
  },
  {
    id: 'inscribedsquare',
    title: 'Inscribed Square',
    description: `Maximal area Inscribed Square Problem`,
    logoPath: path.join('static_inscribedsquare', 'inscribedsquare_logo.png'),
    miroversion: miroVersion,
    apiversion: apiVersion,
    usetmpdir: true,
    modesAvailable: [ 'base' ]
  }]
}
