import {readFileSync} from 'node:fs'
import path from 'node:path';
import { ElectronApplication, Page } from 'playwright/test';

export const dragAndDropFile = async (
  page: Page,
  selector: string,
  filePath: string,
  fileName: string,
  fileType: string
) => {
  // 1. Get the absolute path and read the file as base64
  const absolutePath = path.resolve(filePath);
  const buffer = readFileSync(absolutePath).toString('base64');

  // 2. Create the DataTransfer object and patch Electron's webUtils inside the browser
  const dataTransfer = await page.evaluateHandle(
    async ({ bufferData, localFileName, localFileType, fullPath }) => {
      const dt = new DataTransfer();
      const blobData = await fetch(bufferData).then((res) => res.blob());
      const file = new File([blobData], localFileName, { type: localFileType });

      // Explicitly define the non-standard 'path' property expected in Electron
      Object.defineProperty(file, 'path', {
        value: fullPath,
        writable: false,
        configurable: true
      });

      // Intercept webUtils.getPathForFile if Electron's require context is accessible
      if (typeof window !== 'undefined' && (window as any).require) {
        try {
          const { webUtils } = (window as any).require('electron');
          if (webUtils && !webUtils._patched) {
            const originalGetPath = webUtils.getPathForFile;
            webUtils.getPathForFile = (f: any) => f.path || originalGetPath(f);
            webUtils._patched = true;
          }
        } catch (e) {
          console.warn("Context isolation restricted webUtils patching. Falling back to property injection.");
        }
      }

      dt.items.add(file);
      return dt;
    },
    {
      bufferData: `data:application/octet-stream;base64,${buffer}`,
      localFileName: fileName,
      localFileType: fileType,
      fullPath: absolutePath, // Pass the absolute path into the browser context
    }
  );

  // 3. Dispatch the complete lifecycle of events to satisfy Chromium's listener expectations
  await page.dispatchEvent(selector, 'dragenter', { dataTransfer });
  await page.dispatchEvent(selector, 'dragover', { dataTransfer });
  await page.dispatchEvent(selector, 'drop', { dataTransfer });
};

export async function getMainWindow(electronApp: ElectronApplication) {
  await electronApp.firstWindow();

  for (const window of electronApp.windows()) {
    const title = await window.title();
    const url = window.url();
    if (!url.startsWith('devtools://') && !title.includes('Developer Tools')) {
      return window;
    }
  }
  return await electronApp.waitForEvent('window', {
    predicate: async (win) => {
      const title = await win.title();
      return !title.includes('Developer Tools');
    }
  });
}
