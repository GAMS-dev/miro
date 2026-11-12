import { contextBridge } from 'electron';
import { miroRelease, miroVersion } from '../components/globals.js';

contextBridge.exposeInMainWorld('globals', {
  miroRelease: () => miroRelease,
  miroVersion: () => miroVersion,
});
