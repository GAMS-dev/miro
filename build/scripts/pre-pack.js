/* eslint-disable no-console */
const path = require('path');
const fs = require('fs-extra');
const execa = require('execa');
const https = require('https');

let rExists = false;
if (!fs.existsSync(path.join('.', 'r'))) {
  fs.mkdirSync(path.join('.', 'r'));
} else {
  console.log('R already exists. Skipping installation.');
  rExists = true;
}
const buildDocker = process.argv[2] === '--docker';

const buildConfig = JSON.parse(fs.readFileSync('build-config.json'));

const tryInstallRPackages = async (attempt = 0) => {
  if (attempt === 3) {
    process.exit(1);
  }
  try {
    let rPath = 'Rscript';
    if (process.platform === 'win32') {
      rPath = path.join('.', 'r', 'bin', 'Rscript');
    }
    const subproc = execa(rPath, [path.join('.', 'build', 'scripts', 'install-packages.R')],
      {
        env: {
          LIB_PATH: path.join('.', 'r', 'library'),
          BUILD_DOCKER: buildDocker ? 'true' : 'false',
        },
      });
    subproc.stderr.pipe(process.stderr);
    subproc.stdout.pipe(process.stderr);
    await subproc;
  } catch (e) {
    console.log(`Problems installing R packages. Error message: ${e.message}`);
    try {
      await tryInstallRPackages(attempt + 1);
    } catch (err) {
      console.log(`Problems installing R packages. Error message: ${err.message}`);
    }
  }
};
(async () => {
  if (process.platform === 'win32' && !rExists) {
    try {
      console.log('Installing R...');
      const file = fs.createWriteStream(path.join('r', 'latest_r.exe'));
      https.get(`https://cloud.r-project.org/bin/windows/base/R-${buildConfig.rVersion}-win.exe`, (response) => {
        response.pipe(file);

        file.on('finish', () => {
          file.close(async () => {
            const subproc = execa('innoextract', ['-e', 'latest_r.exe'],
              { cwd: path.join('.', 'r') });
            subproc.stderr.pipe(process.stderr);
            subproc.stdout.pipe(process.stderr);
            await subproc;
            try {
              await fs.move(path.join('.', 'r', 'app'), path.join('.', 'r-tmp'), {
                overwrite: true,
              });
              await fs.move(path.join('.', 'r-tmp'), path.join('.', 'r'), {
                overwrite: true,
              });
            } catch (e) {
              console.log(`Problems moving R. Error message: ${e.message}`);
              fs.remove(path.join('.', 'r')).catch((err) => {
                console.error(err);
              });
              process.exit(1);
            }
            try {
              await tryInstallRPackages();
            } catch (e) {
              console.log(`Problems installing R packages. Error message: ${e.message}`);
              process.exit(1);
            }
          });
        });
      }).on('error', async (e) => {
        console.log(`Problems installing R. Error message: ${e.message}`);
        fs.remove(path.join('.', 'r')).catch((err) => {
          console.error(err);
        });
        process.exit(1);
      });
    } catch (e) {
      console.log(`Problems installing R. Error message: ${e.message}`);
      fs.remove(path.join('.', 'r')).catch((err) => {
        console.error(err);
      });
      process.exit(1);
    }
  } else {
    if (process.platform === 'darwin' && !rExists) {
      try {
        const subproc = execa(path.join('.', 'build', 'scripts', 'get-r-mac.sh'),
          { shell: true,
            env: {
              R_BASE_VERSION: buildConfig.rVersion,
            }
          });
        subproc.stderr.pipe(process.stderr);
        subproc.stdout.pipe(process.stderr);
        await subproc;
      } catch (e) {
        console.log(`Problems installing R. Error message: ${e.message}`);
        process.exit(1);
      }
    }
    try {
      await tryInstallRPackages();
    } catch (e) {
      console.log(`Problems installing R packages. Error message: ${e.message}`);
      process.exit(1);
    }
  }
  if (buildDocker) {
    try {
      console.log('Building Docker images...');
      // `gamsmiro-ui:${process.env.npm_package_version}`
      const subproc = execa('docker', ['build',
        '--build-arg', `R_BASE_VERSION=${buildConfig.rVersion}`,
        '-t', 'gamsmiro-ui', '.']);
      subproc.stderr.pipe(process.stderr);
      subproc.stdout.pipe(process.stderr);
      await subproc;
      const subprocAdmin = execa('docker', ['build', '-t', 'gamsmiro-admin', '-f', 'Dockerfile-admin', '.']);
      subprocAdmin.stderr.pipe(process.stderr);
      subprocAdmin.stdout.pipe(process.stderr);
      await subprocAdmin;
      const subprocCi = execa('docker', ['build',
        '--build-arg', `GAMS_MAJOR=${buildConfig.gamsVersion.split('.')[0]}`,
        '--build-arg', `GAMS_MINOR=${buildConfig.gamsVersion.split('.')[1]}`,
        '--build-arg', `GAMS_MAINT=${buildConfig.gamsVersion.split('.')[2]}`,
        '-t', 'gamsmiro-ci', '.'],
        { cwd: path.join('.', 'ci') });
      subprocCi.stderr.pipe(process.stderr);
      subprocCi.stdout.pipe(process.stderr);
      await subprocCi;
    } catch (e) {
      console.log(`Problems building Docker images. Error message: ${e.message}`);
      process.exit(1);
    }
  }
})();
