import os
import time
import unittest

from selenium import webdriver
from selenium.common.exceptions import (
    NoSuchElementException,
    ElementClickInterceptedException,
    StaleElementReferenceException,
)
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

from .util import get_image_hash, drop_file

# Load environment variables
ENGINE_USER = os.getenv("ENGINE_USER")
ENGINE_PASSWORD = os.getenv("ENGINE_PASSWORD")
UI_URL = os.getenv("MIRO_PROXY_URL", "http://docker:8080")
IN_CI = os.getenv("CI", "") != ""
RUN_K8S_TESTS = os.getenv("RUN_K8S_TESTS", "") != ""
print(os.getcwd())
if not ENGINE_USER or not ENGINE_PASSWORD:
    raise EnvironmentError(
        "Please set the ENGINE_USER and ENGINE_PASSWORD environment variables."
    )


class UITests(unittest.TestCase):
    def setUp(self):
        self.ui_url = UI_URL
        test_browser = os.getenv("TEST_BROWSER", "chrome")
        if test_browser == "firefox":
            if IN_CI:
                self.driver = webdriver.Remote(
                    options=webdriver.FirefoxOptions(),
                    command_executor="http://selenium__standalone-firefox:4444/wd/hub",
                )
            else:
                self.driver = webdriver.Firefox()
        elif test_browser == "chrome":
            if IN_CI:
                self.driver = webdriver.Remote(
                    options=webdriver.ChromeOptions(),
                    command_executor="http://selenium__standalone-chrome:4444/wd/hub",
                )
            else:
                self.driver = webdriver.Chrome()
        else:
            raise ValueError(f"{test_browser} is not chrome or firefox")

        self.driver.get(self.ui_url)

    def login(self):
        try:
            username_field = self.driver.find_element(By.ID, "username")
            password_field = self.driver.find_element(By.ID, "password")
        except NoSuchElementException:
            return

        username_field.clear()
        password_field.clear()

        username_field.send_keys(ENGINE_USER)
        password_field.send_keys(ENGINE_PASSWORD)

        self.driver.find_element(By.CSS_SELECTOR, "button[type='submit']").click()
        WebDriverWait(self.driver, 10).until(
            EC.visibility_of_element_located((By.ID, "navAdminPanel"))
        )
        self.assertEqual(
            self.driver.find_element(
                By.CSS_SELECTOR, "#navbar .navbar-right .navbar-text"
            ).text,
            ENGINE_USER,
        )

    def logout(self):
        self.driver.switch_to.default_content()
        wait = WebDriverWait(self.driver, 3)
        sign_out_element = wait.until(
            EC.presence_of_element_located((By.CSS_SELECTOR, ".navbar-right a"))
        )

        # Check if the element text matches "Sign Out" and if it is displayed
        if sign_out_element.is_displayed() and sign_out_element.text == "Sign Out":
            sign_out_element.click()

    def remove_apps(self):
        self.driver.switch_to.default_content()
        self.login()
        # open admin panel
        self.driver.get(f"{self.ui_url}/app/admin")
        WebDriverWait(self.driver, 30).until(
            EC.frame_to_be_available_and_switch_to_it((By.ID, "shinyframe"))
        )

        WebDriverWait(self.driver, 30).until(
            EC.visibility_of_element_located((By.ID, "addAppBox"))
        )

        wait = WebDriverWait(self.driver, 10)

        while True:
            time.sleep(1)
            wait.until(EC.invisibility_of_element((By.ID, "loading-screen")))
            delete_links = self.driver.find_elements(By.CLASS_NAME, "delete-app-button")
            if not delete_links:
                break
            delete_link = delete_links[0]
            delete_link.click()
            wait.until(
                EC.text_to_be_present_in_element(
                    (By.CLASS_NAME, "bootbox-body"),
                    "Are you sure you want to remove this app? This cannot be undone.",
                )
            )
            accept_buttons = self.driver.find_elements(By.CLASS_NAME, "bootbox-accept")
            visible_accept_button = next(
                btn for btn in accept_buttons if btn.is_displayed()
            )
            visible_accept_button.click()
            bootbox_input = wait.until(
                EC.visibility_of_element_located((By.CLASS_NAME, "bootbox-input-text"))
            )
            bootbox_input.send_keys("asd")
            accept_buttons = self.driver.find_elements(By.CLASS_NAME, "bootbox-accept")
            visible_accept_button = next(
                btn for btn in accept_buttons if btn.is_displayed()
            )
            visible_accept_button.click()
            self.assertTrue(
                self.driver.find_element(By.CLASS_NAME, "bootbox-body").is_displayed(),
                "Bootbox body disappeared unexpectedly after entering 'asd' when removing app data.",
            )
            bootbox_input.clear()
            bootbox_input.send_keys("test_app1")
            accept_buttons = self.driver.find_elements(By.CLASS_NAME, "bootbox-accept")
            visible_accept_button = next(
                btn for btn in accept_buttons if btn.is_displayed()
            )
            visible_accept_button.click()
            wait.until(EC.invisibility_of_element((By.CLASS_NAME, "bootbox-body")))
            time.sleep(2)
            wait.until(EC.invisibility_of_element((By.ID, "loading-screen")))

    def tearDown(self):
        self.remove_apps()
        self.logout()
        self.driver.quit()

    def test_login(self):
        # Wait for the form-signing element to be present
        WebDriverWait(self.driver, 10).until(
            EC.presence_of_element_located((By.CLASS_NAME, "form-signin"))
        )
        if RUN_K8S_TESTS:
            logo_hash = "7420e77f2fbc70ac06e3f39fa3094860"  # pragma: allowlist secret
        else:
            logo_hash = "2421dc42ba681eb41eefe56ab044bb0a"  # pragma: allowlist secret
        self.assertEqual(
            get_image_hash(self.driver, ".branding img"),
            logo_hash,
            "Custom logo not visible",
        )

        # Step 2: Attempt to sign in with the wrong password
        username_field = self.driver.find_element(By.ID, "username")
        password_field = self.driver.find_element(By.ID, "password")
        sign_in_button = self.driver.find_element(
            By.CSS_SELECTOR, "button[type='submit']"
        )

        username_field.clear()
        password_field.clear()

        # Enter the username from environment variable
        username_field.send_keys(ENGINE_USER)
        # Enter a bad password
        password_field.send_keys("wrong_password")

        # Click the sign-in button
        sign_in_button.click()

        # Wait for the alert-danger element to appear with the expected text
        alert_element = WebDriverWait(self.driver, 10).until(
            EC.visibility_of_element_located((By.CLASS_NAME, "alert-danger"))
        )
        self.assertTrue(
            "Invalid user name or password" in alert_element.text,
            "Could not find invalid password alert",
        )

        self.login()

    def test_add_app_unsigned(self):
        # adding app without signature should not work if forceSignedApps is enabled
        self.login()
        # open admin panel
        self.driver.find_element(By.ID, "navAdminPanel").click()

        WebDriverWait(self.driver, 30).until(
            EC.frame_to_be_available_and_switch_to_it((By.ID, "shinyframe"))
        )

        WebDriverWait(self.driver, 30).until(
            EC.visibility_of_element_located((By.ID, "addAppBox"))
        )

        wait = WebDriverWait(self.driver, 10)
        retry_count = 0
        while True:
            try:
                wait.until(EC.element_to_be_clickable((By.ID, "addApp"))).click()
                break
            except (
                StaleElementReferenceException,
                ElementClickInterceptedException,
            ) as exc:
                retry_count += 1
                time.sleep(1)
                self.assertLessEqual(
                    retry_count, 10, f"Couldn't click addApp box after 10 tries: {exc}"
                )
        file_input = wait.until(EC.presence_of_element_located((By.ID, "miroAppFile")))
        wait.until(
            EC.visibility_of_element_located(
                (By.CSS_SELECTOR, "label[for='miroAppFile']")
            )
        )

        file_input.send_keys(
            os.path.join(os.getcwd(), "tests", "data", "transport_unsigned.miroapp")
        )
        wait.until(
            EC.text_to_be_present_in_element_value(
                (By.ID, "newAppName"), "Transport test app"
            )
        )
        self.driver.find_element(By.ID, "btAddApp").click()
        if RUN_K8S_TESTS:
            wait.until(
                EC.text_to_be_present_in_element(
                    (By.CLASS_NAME, "bootbox-body"), "App is not signed!"
                )
            )
        else:
            WebDriverWait(self.driver, 30).until(
                EC.invisibility_of_element((By.ID, "expandedAddAppWrapper"))
            )
            wait.until(
                EC.text_to_be_present_in_element(
                    (By.ID, "staticAppTitle_1"), "Transport test app"
                )
            )
        self.driver.switch_to.default_content()

    def test_add_app_db_migrate(self):
        # test that database migration works
        self.login()
        # open admin panel
        self.driver.find_element(By.ID, "navAdminPanel").click()

        WebDriverWait(self.driver, 30).until(
            EC.frame_to_be_available_and_switch_to_it((By.ID, "shinyframe"))
        )

        WebDriverWait(self.driver, 30).until(
            EC.visibility_of_element_located((By.ID, "addAppBox"))
        )

        wait = WebDriverWait(self.driver, 10)
        retry_count = 0
        while True:
            try:
                wait.until(EC.element_to_be_clickable((By.ID, "addApp"))).click()
                break
            except (
                StaleElementReferenceException,
                ElementClickInterceptedException,
            ) as exc:
                retry_count += 1
                time.sleep(1)
                self.assertLessEqual(
                    retry_count, 10, f"Couldn't click addApp box after 10 tries: {exc}"
                )
        file_input = wait.until(EC.presence_of_element_located((By.ID, "miroAppFile")))
        wait.until(
            EC.visibility_of_element_located(
                (By.CSS_SELECTOR, "label[for='miroAppFile']")
            )
        )

        file_input.send_keys(
            os.path.join(os.getcwd(), "tests", "data", "transport.miroapp")
        )
        wait.until(
            EC.text_to_be_present_in_element_value(
                (By.ID, "newAppName"), "Transport test app"
            )
        )
        self.driver.find_element(By.ID, "btAddApp").click()
        WebDriverWait(self.driver, 30).until(
            EC.invisibility_of_element((By.ID, "expandedAddAppWrapper"))
        )
        file_input = wait.until(
            EC.presence_of_element_located((By.CLASS_NAME, "app-data-file-input"))
        )
        drop_file(
            self.driver,
            file_input,
            os.path.join(os.getcwd(), "tests", "data", "transport_db_schema.miroapp"),
        )
        wait.until(
            EC.text_to_be_present_in_element(
                (By.CLASS_NAME, "bootbox-body"), "overwrite"
            )
        )
        accept_buttons = self.driver.find_elements(By.CLASS_NAME, "bootbox-accept")
        visible_accept_button = next(
            btn for btn in accept_buttons if btn.is_displayed()
        )
        visible_accept_button.click()
        WebDriverWait(self.driver, 30).until(
            EC.visibility_of_element_located(
                (By.ID, "migrationForm1-btConfirmMigration")
            )
        )
        closeMigrationFormButton = wait.until(
            EC.presence_of_element_located((By.ID, "btCloseMigForm"))
        )
        closeMigrationFormButton.click()
        WebDriverWait(self.driver, 10).until(
            EC.invisibility_of_element_located((By.CLASS_NAME, "modal-dialog"))
        )
        WebDriverWait(self.driver, 2).until(
            EC.invisibility_of_element_located((By.CLASS_NAME, "app-spinner"))
        )
        file_input = wait.until(
            EC.presence_of_element_located((By.CLASS_NAME, "app-data-file-input"))
        )
        drop_file(
            self.driver,
            file_input,
            os.path.join(os.getcwd(), "tests", "data", "transport_db_schema.miroapp"),
        )
        wait.until(
            EC.text_to_be_present_in_element(
                (By.CLASS_NAME, "bootbox-body"), "overwrite"
            )
        )
        accept_buttons = self.driver.find_elements(By.CLASS_NAME, "bootbox-accept")
        visible_accept_button = next(
            btn for btn in accept_buttons if btn.is_displayed()
        )
        visible_accept_button.click()
        WebDriverWait(self.driver, 30).until(
            EC.visibility_of_element_located(
                (By.ID, "migrationForm1-btConfirmMigration")
            )
        )
        self.driver.execute_script(
            "$('#migrationForm1-dbMigrateTable_1_7')[0].selectize.addItem('cap');"
        )
        self.driver.find_element(By.ID, "migrationForm1-btConfirmMigration").click()
        time.sleep(2)
        WebDriverWait(self.driver, 30).until(
            EC.invisibility_of_element((By.ID, "loadingScreenProgressWrapper"))
        )
        self.driver.switch_to.default_content()
        wait.until(EC.element_to_be_clickable((By.CLASS_NAME, "navbar-brand"))).click()
        wait.until(EC.visibility_of_element_located((By.CLASS_NAME, "launch-app")))
        all_buttons = self.driver.find_elements(By.CLASS_NAME, "launch-app")
        visible_buttons = [btn for btn in all_buttons if btn.is_displayed()]
        visible_buttons[0].click()
        wait.until(EC.visibility_of_element_located((By.ID, "loading")))
        WebDriverWait(self.driver, 30).until(
            EC.frame_to_be_available_and_switch_to_it((By.ID, "shinyframe"))
        )
        wait.until(
            EC.text_to_be_present_in_element(
                (By.CLASS_NAME, "readme-wrapper"),
                "A Transportation Problem with multiple version LP/MIP/MINLP",
            )
        )
        retry_count = 0
        while True:
            try:
                self.driver.find_element(
                    By.CSS_SELECTOR, "a[data-value='outputData']"
                ).click()
                wait.until(
                    EC.element_to_be_clickable((By.ID, "outputTableView"))
                ).click()
                break
            except (
                StaleElementReferenceException,
                ElementClickInterceptedException,
            ) as exc:
                retry_count += 1
                time.sleep(1)
                self.assertLessEqual(
                    retry_count, 10, f"Couldn't click addApp box after 10 tries: {exc}"
                )
        table_container = wait.until(
            EC.presence_of_element_located((By.ID, "table_tab_1_1-datatable"))
        )
        rows = wait.until(
            EC.presence_of_all_elements_located(
                (By.CSS_SELECTOR, "#table_tab_1_1-datatable tbody tr")
            )
        )
        headers = table_container.find_elements(
            By.CSS_SELECTOR, "#table_tab_1_1-datatable thead th"
        )
        capacity_index = None
        for idx, header in enumerate(headers):
            if header.text.lower().strip() == "capacity":
                capacity_index = idx
                break

        self.assertIsNotNone(capacity_index, "Capacity column not found.")
        capacity_values = []
        for row in rows:
            cells = row.find_elements(By.CSS_SELECTOR, "td")
            if len(cells) > capacity_index:
                capacity_values.append(int(float(cells[capacity_index].text.strip())))
        self.assertEqual(
            capacity_values,
            [350, 350, 350, 600, 600, 600],
            "Migration of database did not work properly",
        )
        self.driver.switch_to.default_content()

    def test_add_app(self):
        self.login()
        # open admin panel
        self.driver.find_element(By.ID, "navAdminPanel").click()

        WebDriverWait(self.driver, 30).until(
            EC.frame_to_be_available_and_switch_to_it((By.ID, "shinyframe"))
        )

        WebDriverWait(self.driver, 30).until(
            EC.visibility_of_element_located((By.ID, "addAppBox"))
        )

        wait = WebDriverWait(self.driver, 10)
        retry_count = 0
        while True:
            try:
                wait.until(EC.element_to_be_clickable((By.ID, "addApp"))).click()
                break
            except (
                StaleElementReferenceException,
                ElementClickInterceptedException,
            ) as exc:
                retry_count += 1
                time.sleep(1)
                self.assertLessEqual(
                    retry_count, 10, f"Couldn't click addApp box after 10 tries: {exc}"
                )
        file_input = wait.until(EC.presence_of_element_located((By.ID, "miroAppFile")))
        wait.until(
            EC.visibility_of_element_located(
                (By.CSS_SELECTOR, "label[for='miroAppFile']")
            )
        )

        file_input.send_keys(
            os.path.join(os.getcwd(), "tests", "data", "transport.miroapp")
        )
        wait.until(
            EC.text_to_be_present_in_element_value(
                (By.ID, "newAppName"), "Transport test app"
            )
        )

        new_app_desc = self.driver.find_element(By.ID, "newAppDesc")
        self.assertTrue(
            new_app_desc.get_attribute("value").strip() == "Transport app for UI tests",
            "newAppDesc value does not match the expected value.",
        )
        new_app_env = self.driver.find_element(By.ID, "newAppEnv")
        new_app_env.clear()
        new_app_env.send_keys('{"MIRO_LANG":"de"}')
        new_app_name_input = self.driver.find_element(By.ID, "newAppName")
        new_app_name_input.clear()
        new_app_name_input.send_keys("Transport app")
        self.driver.find_element(By.ID, "btAddApp").click()
        WebDriverWait(self.driver, 30).until(
            EC.invisibility_of_element((By.ID, "expandedAddAppWrapper"))
        )
        wait.until(
            EC.text_to_be_present_in_element(
                (By.ID, "staticAppTitle_1"), "Transport app"
            )
        )
        static_app_desc = self.driver.find_element(By.ID, "staticAppDesc_1")
        self.assertTrue(
            static_app_desc.text.strip() == "Transport app for UI tests",
            "description text does not match the expected value.",
        )
        self.driver.switch_to.default_content()
        wait.until(EC.element_to_be_clickable((By.CLASS_NAME, "navbar-brand"))).click()
        wait.until(EC.visibility_of_element_located((By.CLASS_NAME, "launch-app")))
        all_buttons = self.driver.find_elements(By.CLASS_NAME, "launch-app")
        visible_buttons = [btn for btn in all_buttons if btn.is_displayed()]
        self.assertTrue(
            len(visible_buttons) == 1,
            "More than one visible 'launch-app' button found.",
        )
        self.assertTrue(
            visible_buttons[0].get_attribute("href").endswith("/app/test_app1"),
            "The href of the launch-app link is not '/app/test_app1'.",
        )
        visible_buttons[0].click()
        wait.until(EC.visibility_of_element_located((By.ID, "loading")))
        WebDriverWait(self.driver, 30).until(
            EC.frame_to_be_available_and_switch_to_it((By.ID, "shinyframe"))
        )
        wait.until(
            EC.text_to_be_present_in_element(
                (By.CLASS_NAME, "readme-wrapper"),
                "A Transportation Problem with multiple version LP/MIP/MINLP",
            )
        )
        self.assertTrue(
            self.driver.find_element(By.ID, "btSolve").text.strip() == "Modell lösen",
            "Solve button has correct text in German (MIRO_LANG applied correctly)",
        )
        self.driver.switch_to.default_content()
