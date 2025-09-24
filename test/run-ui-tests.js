/* eslint-disable no-console */
import path from 'node:path';
import fs from 'node:fs/promises';
import { XMLParser } from 'fast-xml-parser';
import { fileURLToPath } from 'node:url';
import { execa } from 'execa';

async function checkFileExists(file) {
  try {
    await fs.access(file, fs.constants.F_OK);
    return true;
  } catch (_) {
    return false;
  }
}

// eslint-disable-next-line no-underscore-dangle
const __dirname = path.dirname(fileURLToPath(import.meta.url));

let gamsSysDir = '';

if (
  typeof process.argv[2] === 'string' &&
  process.argv[2].startsWith('gams_sys_dir')
) {
  const gamsSysDirMatch = process.argv[2].match(/^gams_sys_dir="?([^"]+)"?$/);
  if (gamsSysDirMatch) {
    [, gamsSysDir] = gamsSysDirMatch;
  }
}

(async () => {
  if (
    process.platform === 'darwin' &&
    (await checkFileExists(path.join(__dirname, '..', 'library')))
  ) {
    try {
      const subproc = execa(
        'rm',
        ['-rf', path.join(__dirname, '..', 'r', 'library')],
        { shell: true },
      );
      subproc.stderr.pipe(process.stderr);
      subproc.stdout.pipe(process.stderr);
      await subproc;
    } catch (e) {
      console.log(
        `Problems removing old library files. Error message: ${e.message}`,
      );
    }
    try {
      const subproc = execa(
        'mv',
        [
          '-f',
          path.join(__dirname, '..', 'library'),
          path.join(__dirname, '..', 'r', 'library'),
        ],
        { shell: true },
      );
      subproc.stderr.pipe(process.stderr);
      subproc.stdout.pipe(process.stderr);
      await subproc;
    } catch (e) {
      console.log(
        `Problems replacing R library directory. Error message: ${e.message}`,
      );
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
    const xmlPath = path.join(__dirname, '..', 'src', 'test-out.xml');
    const xmlData = await fs.readFile(xmlPath, 'utf-8');
    const parser = new XMLParser({
      ignoreAttributes: false,
      attributeNamePrefix: '',
    });
    const testResult = parser.parse(xmlData);
    let jobShouldFail = false;
    let failureReason = '';
    const testsuites = Array.isArray(testResult.testsuites.testsuite)
      ? testResult.testsuites.testsuite
      : [testResult.testsuites.testsuite];

    // eslint-disable-next-line no-restricted-syntax
    for (const suite of testsuites) {
      if (parseInt(suite.failures, 10) > 0) {
        jobShouldFail = true;
        failureReason = `Test suite '${suite.name}' reported ${suite.failures} failure(s).`;
        break;
      }
      const testcases = Array.isArray(suite.testcase)
        ? suite.testcase
        : [suite.testcase];
      // eslint-disable-next-line no-restricted-syntax
      for (const testcase of testcases) {
        if (testcase?.failure) {
          jobShouldFail = true;
          failureReason = `Test case '${testcase.name}' failed.`;
          break;
        }
        if (testcase?.skipped) {
          const skipMessage = testcase.skipped.message || '';
          if (skipMessage.includes('{chromote} can not be started')) {
            jobShouldFail = true;
            failureReason = `Critical skip in '${testcase.name}': Chromote could not be started.`;
            break;
          }
        }
      }
      if (jobShouldFail) break;
    }
    if (jobShouldFail) {
      console.error(`\n❌ BUILD FAILED: ${failureReason}`);
      process.exit(1);
    } else {
      console.log('\n✅ All tests passed and no critical skips were found.');
      process.exit(0);
    }
  } catch (e) {
    console.log(`Problems running R tests. Error message: ${e.message}`);
    process.exit(1);
  }
})();
