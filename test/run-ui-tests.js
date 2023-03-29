/* eslint-disable no-console */
const path = require('path');
const fs = require('fs');
const execa = require('execa');

let gamsSysDir = '';

if (typeof process.argv[2] === 'string' && process.argv[2].startsWith('gams_sys_dir')) {
  const gamsSysDirMatch = process.argv[2].match(/^gams_sys_dir="?([^"]+)"?$/);
  if (gamsSysDirMatch) {
    [, gamsSysDir] = gamsSysDirMatch;
  }
}

(async () => {
  if (process.platform === 'darwin' && fs.existsSync(path.join(__dirname, '..', 'library'))) {
    try {
      const subproc = execa('rm', ['-rf', path.join(__dirname, '..', 'r', 'library')], { shell: true });
      subproc.stderr.pipe(process.stderr);
      subproc.stdout.pipe(process.stderr);
      await subproc;
    } catch (e) {
      console.log(`Problems removing old library files. Error message: ${e.message}`);
    }
    try {
      const subproc = execa('mv', ['-f', path.join(__dirname, '..', 'library'),
        path.join(__dirname, '..', 'r', 'library')], { shell: true });
      subproc.stderr.pipe(process.stderr);
      subproc.stdout.pipe(process.stderr);
      await subproc;
    } catch (e) {
      console.log(`Problems replacing R library directory. Error message: ${e.message}`);
    }
  }
  try {
    let rPath = 'Rscript';
    const rEnv = {
      R_LIBS_USER: path.join(__dirname, '..', 'r', 'library'),
      GAMS_SYS_DIR: gamsSysDir,
    };
    if (process.platform === 'win32') {
      rPath = path.join(__dirname, '..', 'r', 'bin', 'Rscript');
    }
    if (process.platform === 'win32' || process.platform === 'darwin') {
      rEnv.R_LIBS_SITE = path.join(__dirname, '..', 'build', 'lib_devel');
    }
    const subproc = execa(
      rPath,
      [path.join(__dirname, '..', 'src', 'tests', 'testthat.R')],
      {
        env: rEnv,
        cwd: path.join(__dirname, '..', 'src'),
        stdio: 'inherit',
      },
    );
    await subproc;
    process.exit();
  } catch (e) {
    console.log(`Problems running R tests. Error message: ${e.message}`);
    process.exit(1);
  }
})();
