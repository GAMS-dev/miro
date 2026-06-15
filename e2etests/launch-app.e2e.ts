import { test, expect, _electron as electron } from '@playwright/test';
import * as path from 'path';
import { dragAndDropFile, getMainWindow } from './util';

const execPath =
  process.env.MIRO_EXEC_PATH ??
  (process.platform === 'win32'
    ? 'C:\\Program Files\\GAMS MIRO\\GAMS MIRO.exe'
    : '/Applications/GAMS MIRO.app/Contents/MacOS/GAMS MIRO');
const isCI = !!process.env.CI;

test('App launches in MIRO Desktop', async () => {
  test.setTimeout(120_000);
  const app = await electron.launch({
    env: {
      ...process.env,
      E2E: 'true',
    },
    ...(isCI ? { executablePath: execPath } : { args: ['main.js'] }),
  });
  const main = await getMainWindow(app);

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

  await app.evaluate(async ({ dialog }) => {
    dialog.showMessageBoxSync = (...args: any[]): number => {
      // Click button with index 1 ("Yes, overwrite") in overwrite data dialog
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
  const details = await app.waitForEvent('window');
  await expect(
    details.locator('text=/A Transportation Problem /'),
  ).toBeVisible();

  await app.close();
});
