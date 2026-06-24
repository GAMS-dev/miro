import { test as base, expect, _electron as electron } from '@playwright/test';
import * as path from 'path';
import fs from "fs/promises";
import { dragAndDropFile, getMainWindow } from './util';

const execPath =
  process.env.MIRO_EXEC_PATH ??
  (process.platform === 'win32'
    ? 'C:\\Program Files\\GAMS MIRO\\GAMS MIRO.exe'
    : '/Applications/GAMS MIRO.app/Contents/MacOS/GAMS MIRO');
const isCI = !!process.env.CI;

export const test = base.extend({
  electronApp: async ({}, use, testInfo) => {
    const safeName = testInfo.title.replace(/[^a-z0-9-_]/gi, "_");
    const testDir = path.join(
      "launcher-logs",
      `${safeName}-${testInfo.workerIndex}-${Date.now()}`
    );

    await fs.mkdir(testDir, { recursive: true });

    const app = await electron.launch({
      env: {
        ...process.env,
        E2E: 'true',
        MIRO_WORKSPACE_DIR: testDir,
      },
      ...(isCI ? { executablePath: execPath } : { args: ['main.js'] }),
    });

    await use(app);

    await app.close();
  },
});

test('App launches in MIRO Desktop', async ({ electronApp }) => {
  test.setTimeout(120_000);
  const main = await getMainWindow(electronApp);

  const AddAppBoxButton = main.locator('#addAppBox');
  await AddAppBoxButton.waitFor({ state: 'visible' });
  await AddAppBoxButton.click();
  const AddExampleAppsButton = main.getByRole('button', {
    name: 'Add Example Apps',
  });
  await AddExampleAppsButton.waitFor({ state: 'visible' });
  await AddExampleAppsButton.click();

  // check that version field of example app is 1.0.0
  const editButton = main.locator('#btEdit');
  const transportVersionField = main.locator(
    '#appBox_adHJhbnNwb3J0 .app-version-field',
  );
  await editButton.waitFor({ state: 'visible' });
  await editButton.click();
  await transportVersionField.waitFor({ state: 'visible' });
  await expect(transportVersionField).toHaveAttribute('title', '1.0.0');
  await editButton.click();

  await electronApp.evaluate(async ({ dialog }) => {
    dialog.showMessageBoxSync = (...args: any[]): number => {
      const firstArg = args[0];
      const options = (firstArg && typeof firstArg === 'object' && !firstArg.constructor.name.includes('Window'))
        ? firstArg
        : args[1] || {};
      const message = options.message || "";
      if (message.includes("overwrite")) {
        // Click button with index 1 ("Yes, overwrite") in overwrite data dialog
        return 1;
      }
      if (message.includes("fingerprint")) {
        // Approve fingerprint
        return 0;
      }
      return 1;
    };
  });

  // update transport with app with version 2.0.0
  const filePath = path.resolve(
    'server',
    'tests',
    'data',
    'transport_v2.miroapp',
  );
  await dragAndDropFile(
    main,
    '#appBox_adHJhbnNwb3J0',
    filePath,
    path.basename(filePath),
    'application/zip',
  );
  const loadingScreen = main.locator('#appSpinner_adHJhbnNwb3J0');
  await loadingScreen.waitFor({ state: 'visible' });
  await loadingScreen.waitFor({ state: 'hidden' });
  await main.locator('#btEdit').click();
  // Assert that version has been updated
  await expect(transportVersionField).toHaveAttribute('title', '2.0.0');
  await main.locator('#btEdit').click();

  // test that app starts
  const transportAppContainer = main.locator(
    '.launch-app-box[data-id="adHJhbnNwb3J0"]',
  );
  await transportAppContainer.click();
  const details = await electronApp.waitForEvent('window');
  await expect(
    details.locator('text=/A Transportation Problem /'),
  ).toBeVisible();
});

test('App launched with GAMS Engine backend', async ({ electronApp }) => {
  test.setTimeout(120_000);
  const main = await getMainWindow(electronApp);

  const windowPromise = electronApp.waitForEvent('window');

  // 1. Open the Preferences dialog
  await electronApp.evaluate(({ Menu }) => {
    const menu = Menu.getApplicationMenu();

    function findMenuItem(items) {
      for (const item of items) {
        if (
          item.accelerator === 'Cmd+,' ||
          item.accelerator === 'F7'
        ) {
          item.click();
          return true;
        }

        if (item.submenu?.items) {
          if (findMenuItem(item.submenu.items)) {
            return true;
          }
        }
      }
      return false;
    }

    if (menu?.items == null || !findMenuItem(menu.items)) {
      throw new Error('Preferences menu item not found');
    }
  });
  const prefsWindow = await windowPromise;
  await prefsWindow.waitForLoadState();

  // 2. Click on the "GAMS Engine" tab
  const gamsEngineTab = prefsWindow.locator('#engine-tab');
  await gamsEngineTab.waitFor({ state: 'visible' });
  await gamsEngineTab.click();

  // 3. Activate the "Execution of models on GAMS Engine?" checkbox
  const activateCheckbox = prefsWindow.locator('#remoteExecution');
  await activateCheckbox.check();

  // 4. Fill in the input fields using environment variables
  await prefsWindow.locator('#engineUrl').fill(process.env.ENGINE_URL ?? '');
  await prefsWindow.locator('#engineNs').fill(process.env.ENGINE_NS ?? '');

  // 5. Select "Username/Password" from the "Login via" dropdown
  const loginViaDropdown = prefsWindow.locator('#engineLoginMethod');
  await loginViaDropdown.selectOption({ label: 'Username/Password' });

  // 6. Enter credentials
  await prefsWindow.locator('#engineUsername').fill(process.env.ENGINE_USER ?? '');
  await prefsWindow.locator('#enginePassword').fill(process.env.ENGINE_PASSWORD ?? '');

  // 7. Click on the "Apply" button
  await prefsWindow.getByRole('button', { name: 'Apply' }).click();

  // 8. Wait until the JWT textarea is visible and contains a value
  const jwtTextArea = prefsWindow.locator('#engineJWT');
  await jwtTextArea.waitFor({ state: 'visible' });

  const jwtValue = await jwtTextArea.inputValue();
  expect(jwtValue.length).toBeGreaterThan(20);

  // 9. Click the "Close" button
  await prefsWindow.getByRole('button', { name: 'Close' }).click();

  // test that app starts
  await main.bringToFront();
  const AddAppBoxButton = main.locator('#addAppBox');
  await AddAppBoxButton.waitFor({ state: 'visible' });
  await AddAppBoxButton.click();

  const AddExampleAppsButton = main.getByRole('button', {
    name: 'Add Example Apps',
  });
  await AddExampleAppsButton.waitFor({ state: 'visible' });
  await AddExampleAppsButton.click();
  const transportAppContainer = main.locator(
    '.launch-app-box[data-id="adHJhbnNwb3J0"]',
  );
  await transportAppContainer.waitFor({ state: 'visible' });
  await transportAppContainer.click();
  const details = await electronApp.waitForEvent('window');
  await expect(
    details.locator('text=/A Transportation Problem /'),
  ).toBeVisible();
});
