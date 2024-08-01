import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'url';

/* eslint no-param-reassign: 0 max-len:0 */
// eslint-disable-next-line no-underscore-dangle
const __filename = fileURLToPath(import.meta.url);

const KEYS_TO_IGNORE = [
  'fixedColumns',
  'emptyUEL',
  'enableHideEmptyCols',
  'hideEmptyCols',
  'enablePersistentViews',
  'hidePivotControls',
  'externalDefaultView',
  'customChartColors',
];

function arraysEqual(a, b) {
  if (a === b) return true;
  if (a == null || b == null) return false;
  if (a.length !== b.length) return false;
  for (let i = 0; i < a.length; i += 1) {
    if (a[i] !== b[i]) return false;
  }
  return true;
}

function mergeObjects(obj1, obj2, keyPath = '') {
  Object.keys(obj1).forEach((key) => {
    const currentPath = keyPath ? `${keyPath}.${key}` : key;
    if (KEYS_TO_IGNORE.includes(currentPath)) {
      return;
    }
    if (Object.prototype.hasOwnProperty.call(obj2, key)) {
      if (obj1[key] == null) {
        if (obj2[key] == null) {
          return;
        }
        obj2[key] = obj1[key];
        return;
      }
      if (Array.isArray(obj1[key])) {
        if (Array.isArray(obj2[key]) && arraysEqual(obj1[key], obj2[key])) {
          return;
        }
        obj2[key] = obj1[key];
        return;
      }
      if (typeof obj1[key] === 'object') {
        if (typeof obj2[key] === 'object') {
          mergeObjects(obj1[key], obj2[key], currentPath);
          return;
        }
        obj2[key] = obj1[key];
        return;
      }
      if (obj2[key] !== obj1[key]) {
        if (key === 'description') {
          // we don't care about different description
          return;
        }
        obj2[key] = obj1[key];
      }
    } else {
      obj2[key] = obj1[key];
    }
  });
  return obj2;
}

let rootDir = '';
if (!path.basename(__filename) !== 'src') {
  rootDir = 'src';
}

const configSchemaPath = path.join(rootDir, 'conf', 'config_schema.json');
const configSchema = JSON.parse(fs.readFileSync(configSchemaPath, 'utf8'));

configSchema.definitions.dashboardOptions.properties.dataViewsConfig.additionalProperties.oneOf[1].properties = mergeObjects(
  configSchema.definitions.miroPivotOptions.properties,
  configSchema.definitions.dashboardOptions.properties.dataViewsConfig.additionalProperties.oneOf[1].properties,
);

fs.writeFileSync(
  configSchemaPath,
  `${JSON.stringify(configSchema, null, 2)}\n`,
  'utf8',
);
