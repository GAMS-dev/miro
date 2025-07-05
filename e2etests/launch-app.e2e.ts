import { test, expect, _electron as electron } from '@playwright/test';

const execPath =
  process.env.MIRO_EXEC_PATH ??
  (process.platform === 'win32'
    ? 'C:\\Program Files\\GAMS MIRO\\GAMS MIRO.exe'
    : '/Applications/GAMS MIRO.app/Contents/MacOS/GAMS MIRO');

test('App launches in MIRO Desktop', async () => {
  test.setTimeout(120_000);
  const app = await electron.launch({
    executablePath: execPath,
    env: {
      ...process.env,
      E2E: 'true',
    },
   });
  const main = await app.firstWindow();
  const AddExampleAppsButton = main.getByRole('button', { name: 'Add example apps' });
  await AddExampleAppsButton.waitFor({ state: 'visible' });
  await AddExampleAppsButton.click();
  const transportAppContainer = main.locator('div[data-id="adHJhbnNwb3J0"]');
  await transportAppContainer.getByRole('button', { name: 'Launch' }).click();
  const details = await app.waitForEvent('window');
  await expect(
    details.locator('text=/A Transportation Problem /'),
  ).toBeVisible();

  await app.close();
});
