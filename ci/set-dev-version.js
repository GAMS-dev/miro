// Turns non-master CI builds into builds for the dev update channel, e.g. 2.13.9999 -> 2.14.0-dev.<pipeline>.<branch>
import fs from 'node:fs';

const { CI_COMMIT_BRANCH, CI_PIPELINE_IID, CI_COMMIT_REF_SLUG } = process.env;

if (CI_COMMIT_BRANCH !== 'master') {
  const pkg = JSON.parse(fs.readFileSync('package.json', 'utf8'));
  const [major, minor, patch] = pkg.version.split('.').map(Number);
  // develop has the patch version 9999
  const base =
    patch >= 999 ? `${major}.${minor + 1}.0` : `${major}.${minor}.${patch}`;
  // numeric semver identifiers must not have leading zeros; the macOS updater treats file names with arm64 as ARM builds
  const slug = CI_COMMIT_REF_SLUG.replaceAll('arm64', 'arm-64');
  const branch = /^\d+$/.test(slug) ? `branch-${slug}` : slug;
  pkg.version = `${base}-dev.${CI_PIPELINE_IID}.${branch}`;
  pkg.build.buildVersion = `${base}.${CI_PIPELINE_IID}`;
  pkg.build.publish = {
    ...pkg.build.publish,
    url: pkg.build.publish.url.replace(/\/latest$/, '/dev'),
    channel: 'dev',
  };
  fs.writeFileSync('package.json', `${JSON.stringify(pkg, null, 2)}\n`);
  console.log(`Building version ${pkg.version} for the dev update channel`);
}
