/* eslint-disable no-console */
import execa from 'execa';
import path from 'node:path';

export default async function notarizing(context) {
  if (process.platform !== 'darwin' || !process.env.CODESIGN_IDENTITY) {
    return;
  }
  // skip if not on master, develop or rc
  if (process.env.SKIP_NOTARIZATION === 'true') {
    return;
  }
  const appFile = `"${context.artifactPaths.filter((el) => el.endsWith('.dmg'))[0]}"`;
  try {
    const notarizeProc = execa(
      path.join('.', 'build', 'scripts', 'notarize.sh'),
      [appFile],
      { shell: true },
    );
    notarizeProc.stderr.pipe(process.stderr);
    notarizeProc.stdout.pipe(process.stderr);
    await notarizeProc;
  } catch (e) {
    console.log(`Problems notarizing app. Error message: ${e.message}`);
    throw e;
  }
}
