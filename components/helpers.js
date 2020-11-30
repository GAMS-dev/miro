const portscanner = require('portscanner');
const treeKill = require('tree-kill');

const isNull = (el) => el === null;
/*
MIT License

Copyright (c) 2018 Dirk Schumacher, Noam Ross, Rich FitzJohn

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/
const randomInt = (min, max) => Math.round(Math.random() * ((max + 1) - min) + min);
const randomPort = async () => {
  /* eslint-disable no-constant-condition, no-await-in-loop, no-continue */
  // Those forbidden ports are in line with shiny
  // https://github.com/rstudio/shiny/blob/288039162086e183a89523ac0aacab824ef7f016/R/server.R#L734
  const forbiddenPorts = [3659, 4045, 6000, 6665, 6666, 6667, 6668, 6669, 6697];
  while (true) {
    let port = randomInt(3000, 5000);
    if (forbiddenPorts.includes(port)) continue;
    port = await portscanner.findAPortNotInUse(port, 8000, '127.0.0.1');
    if (forbiddenPorts.includes(port)) continue;
    return port;
  }
};

const waitFor = (milliseconds) => new Promise((resolve) => {
  setTimeout(resolve, milliseconds);
});

const isFalse = (val) => val === 'false'
  || val === false || val === 0;

const kill = (pid, signal = 'SIGTERM') => new Promise((resolve, reject) => treeKill(pid, signal, (killErr) => {
  if (killErr) return reject(killErr);
  return resolve({ pid });
}));

module.exports = {
  waitFor,
  randomPort,
  isNull,
  isFalse,
  kill,
};
