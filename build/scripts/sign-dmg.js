/* eslint-disable no-console */
const execa = require('execa');
const path = require('path');

exports.default = async function signing(context) {
  const { electronPlatformName, appOutDir } = context;
  if (electronPlatformName !== 'darwin') {
    return;
  }
  const appName = context.packager.appInfo.productFilename;

  const codesignIdentity = process.env.CODESIGN_IDENTITY;

  if (!codesignIdentity) {
    console.log('Skipping codesign as CODESIGN_IDENTITY was not specified!');
    return;
  }

  const entitlementsFile = path.join(
    // eslint-disable-next-line no-underscore-dangle
    context.packager.info._buildResourcesDir,
    'entitlements.mac.plist',
  );
  const appFile = path.join(appOutDir, `${appName}.app`);
  const contentDir = path.join(appFile, 'Contents');

  try {
    console.log('Making R framework relocatable...');
    const subproc = execa('python3', [path.join('.', 'build', 'scripts', 'fw-relocatablizer.py'),
      path.join(contentDir, 'Resources', 'r')]);
    subproc.stderr.pipe(process.stderr);
    subproc.stdout.pipe(process.stderr);
    await subproc;
  } catch (e) {
    console.log(`Problems making R framework relocatable. Error message: ${e.message}`);
    throw e;
  }

  try {
    const signProc = execa(
      path.join('.', 'build', 'scripts', 'sign-dmg.sh'),
      [`"${contentDir}"`, codesignIdentity, `"${entitlementsFile}"`],
      { shell: true },
    );
    signProc.stderr.pipe(process.stderr);
    signProc.stdout.pipe(process.stderr);
    await signProc;
  } catch (e) {
    console.log(`Problems signing app. Error message: ${e.message}`);
    throw e;
  }
};
