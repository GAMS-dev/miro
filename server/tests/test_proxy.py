import os
import random
import string
import time
import unittest

import requests
from selenium import webdriver
from selenium.common.exceptions import (
    NoSuchElementException,
    ElementClickInterceptedException,
    StaleElementReferenceException,
    TimeoutException,
)
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

from .util import (
    drop_file,
    get_image_hash,
    get_image_hash_background_style_b64,
    get_selectize_available_options,
    get_selectize_options,
    select_selectize_options,
)

# Load environment variables
ENGINE_URL = os.getenv("ENGINE_URL")
ENGINE_NS = os.getenv("ENGINE_NS")
ENGINE_USER = os.getenv("ENGINE_USER")
ENGINE_PASSWORD = os.getenv("ENGINE_PASSWORD")
UI_URL = os.getenv("MIRO_PROXY_URL", "http://docker:8080")
IN_CI = os.getenv("CI", "") != ""
RUN_K8S_TESTS = os.getenv("RUN_K8S_TESTS", "") != ""
TEST_ID = "".join(random.choices(string.ascii_lowercase, k=5))
print(os.getcwd())
if not ENGINE_USER or not ENGINE_PASSWORD or not ENGINE_NS or not ENGINE_URL:
    raise EnvironmentError(
        "Please set the ENGINE_URL and ENGINE_NS and ENGINE_USER and ENGINE_PASSWORD environment variables."
    )


def add_groups(user_groups):
    for user_group in user_groups:
        response = requests.post(
            f"{ENGINE_URL}/namespaces/{ENGINE_NS}/user-groups",
            data={"label": user_group},
            timeout=20,
            auth=(ENGINE_USER, ENGINE_PASSWORD),
        )
        response.raise_for_status()


def remove_groups():
    for user_group in requests.get(
        f"{ENGINE_URL}/namespaces/{ENGINE_NS}/user-groups",
        timeout=20,
        auth=(ENGINE_USER, ENGINE_PASSWORD),
    ).json():
        response = requests.delete(
            f"{ENGINE_URL}/namespaces/{ENGINE_NS}/user-groups",
            params={"label": user_group["label"]},
            timeout=20,
            auth=(ENGINE_USER, ENGINE_PASSWORD),
        )
        response.raise_for_status()


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
        remove_groups()
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
        bootbox_input = wait.until(
            EC.visibility_of_element_located((By.CLASS_NAME, "bootbox-input-text"))
        )
        bootbox_input.send_keys("asd")
        visible_accept_button.click()
        self.assertTrue(
            self.driver.find_element(By.CLASS_NAME, "bootbox-body").is_displayed(),
            "Bootbox body disappeared unexpectedly after entering 'asd' when overwriting app data.",
        )
        bootbox_input.clear()
        bootbox_input.send_keys("test_app1")
        visible_accept_button.click()
        WebDriverWait(self.driver, 30).until(
            EC.visibility_of_element_located(
                (By.ID, "migrationForm1-btConfirmMigration")
            )
        )
        time.sleep(2)
        self.driver.find_element(By.ID, "btCloseMigForm").click()
        WebDriverWait(self.driver, 10).until(
            EC.invisibility_of_element_located((By.ID, "btCloseMigForm"))
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
        bootbox_input = wait.until(
            EC.visibility_of_element_located((By.CLASS_NAME, "bootbox-input-text"))
        )
        bootbox_input.send_keys("test_app1")
        visible_accept_button.click()
        WebDriverWait(self.driver, 30).until(
            EC.visibility_of_element_located(
                (By.ID, "migrationForm2-btConfirmMigration")
            )
        )
        self.driver.execute_script(
            "$('#migrationForm2-dbMigrateTable_1_7')[0].selectize.addItem('cap');"
        )
        self.driver.find_element(By.ID, "migrationForm2-btConfirmMigration").click()
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
                TimeoutException,
            ) as exc:
                retry_count += 1
                time.sleep(1)
                self.assertLessEqual(
                    retry_count,
                    10,
                    f"Couldn't switch to output data section after 10 tries: {exc}",
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
        add_groups([f"{x}{TEST_ID}" for x in ["test", "test2"]])
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
        self.driver.find_element(By.ID, "newAppEnv").click()
        wait.until(
            EC.text_to_be_present_in_element(
                (By.CLASS_NAME, "bootbox-body"), "Scenario Permissions"
            )
        )
        remove_row_buttons = self.driver.find_elements(By.CLASS_NAME, "remove-row")
        self.assertTrue(
            len(remove_row_buttons) == 0,
            "Should not have remove-row buttons when no variables are defined.",
        )
        new_app_env_input = self.driver.find_elements(By.CLASS_NAME, "env-name")
        self.assertTrue(
            len(new_app_env_input) == 1,
            "More than one 'env-name' fields found.",
        )
        new_app_env_input[0].send_keys("9not_valid")
        time.sleep(0.5)
        new_app_desc_input = self.driver.find_elements(By.CLASS_NAME, "env-description")
        self.assertTrue(
            len(new_app_desc_input) == 2,
            "After entering new environment name, no new row was added.",
        )
        remove_row_buttons = self.driver.find_elements(By.CLASS_NAME, "remove-row")
        self.assertTrue(
            len(remove_row_buttons) == 1,
            "Should have remove-row button after entering environment variable name.",
        )
        new_app_desc_input[0].send_keys("test description")
        new_app_val_input = self.driver.find_element(By.CLASS_NAME, "env-value")
        new_app_val_input.send_keys("de")
        self.driver.find_element(By.CSS_SELECTOR, ".modal-footer .confirm-btn").click()
        wait.until(
            EC.text_to_be_present_in_element(
                (By.ID, "form-error"), "must match the pattern"
            )
        )
        new_app_env_input[0].clear()
        new_app_env_input[0].send_keys("MIRO_LANG")
        self.driver.find_element(By.ID, "conf-permissions-tab").click()
        wait.until(
            EC.visibility_of_element_located(
                (By.CLASS_NAME, "access-perm-selector-container")
            )
        )
        select_selectize_options(
            self.driver,
            self.driver.find_element(By.ID, "scenPermReadSelector"),
            "#users",
        )
        select_selectize_options(
            self.driver,
            self.driver.find_element(By.ID, "scenPermWriteSelector"),
            "#admins",
        )
        select_selectize_options(
            self.driver,
            self.driver.find_element(By.ID, "scenPermExecuteSelector"),
            ["#users", "#admins"],
        )
        self.driver.find_element(By.CSS_SELECTOR, ".modal-footer .confirm-btn").click()

        wait.until(EC.invisibility_of_element((By.CLASS_NAME, "bootbox-body")))

        new_app_name_input = self.driver.find_element(By.ID, "newAppName")
        new_app_name_input.clear()
        new_app_name_input.send_keys("Transport app")
        self.assertCountEqual(
            get_selectize_available_options(
                self.driver, self.driver.find_element(By.ID, "newAppGroups")
            ),
            [f"test{TEST_ID}".upper(), f"test2{TEST_ID}".upper()],
        )
        select_selectize_options(
            self.driver,
            self.driver.find_element(By.ID, "newAppGroups"),
            f"test2{TEST_ID}".upper(),
        )
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
        retry_count = 0
        while True:
            try:
                wait.until(
                    EC.element_to_be_clickable((By.CLASS_NAME, "btRemove"))
                ).click()
                break
            except (
                StaleElementReferenceException,
                ElementClickInterceptedException,
            ) as exc:
                retry_count += 1
                time.sleep(1)
                self.assertLessEqual(
                    retry_count,
                    10,
                    f"Couldn't click btRemove box after 10 tries: {exc}",
                )
        wait.until(EC.visibility_of_element_located((By.CLASS_NAME, "bt-gms-confirm")))
        self.driver.find_element(By.CLASS_NAME, "bt-gms-confirm").click()
        wait.until(EC.invisibility_of_element_located((By.CLASS_NAME, "modal-body")))
        [
            x
            for x in self.driver.find_elements(By.CLASS_NAME, "dropdown-toggle")
            if x.is_displayed() and x.text.strip() == "Szenario"
        ][0].click()
        self.driver.find_element(By.ID, "btSave").click()
        wait.until(EC.visibility_of_element_located((By.ID, "editMetaReadPerm-label")))
        self.assertCountEqual(
            get_selectize_options(
                self.driver, self.driver.find_element(By.ID, "editMetaReadPerm")
            ),
            ["#users", ENGINE_USER],
        )
        self.assertCountEqual(
            get_selectize_available_options(
                self.driver, self.driver.find_element(By.ID, "editMetaReadPerm")
            ),
            ["#users", "#admins", f"#test2{TEST_ID}", ENGINE_USER],
        )
        self.assertCountEqual(
            get_selectize_options(
                self.driver, self.driver.find_element(By.ID, "editMetaWritePerm")
            ),
            ["#admins", ENGINE_USER],
        )
        self.assertCountEqual(
            get_selectize_options(
                self.driver, self.driver.find_element(By.ID, "editMetaExecPerm")
            ),
            ["#admins", "#users", ENGINE_USER],
        )

        self.driver.switch_to.default_content()

    def test_update_app_meta(self):
        # test that updating metadata works
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
        self.driver.find_element(By.CLASS_NAME, "input-group").click()
        WebDriverWait(self.driver, 30).until(
            EC.visibility_of_element_located((By.CLASS_NAME, "btn-save-changes"))
        )
        title_input = wait.until(EC.presence_of_element_located((By.ID, "appTitle_1")))
        title_input.clear()
        title_input.send_keys("bla123")
        WebDriverWait(self.driver, 30).until(
            EC.element_to_be_clickable((By.CLASS_NAME, "btn-save-changes"))
        ).click()
        wait.until(
            EC.text_to_be_present_in_element((By.ID, "staticAppTitle_1"), "bla123")
        )
        static_app_desc = self.driver.find_element(By.ID, "staticAppDesc_1")
        self.assertTrue(
            static_app_desc.text.strip() == "Transport app for UI tests",
            "description text does not match the expected value.",
        )
        self.assertEqual(
            get_image_hash_background_style_b64(self.driver, ".app-logo"),
            "c1f874ff6b82b6566e7ac3491ca766b1",  # pragma: allowlist secret
            "Default logo not displayed",
        )
        self.driver.find_element(By.CLASS_NAME, "input-group").click()
        WebDriverWait(self.driver, 30).until(
            EC.visibility_of_element_located((By.CLASS_NAME, "btn-save-changes"))
        )
        logo_input = wait.until(
            EC.presence_of_element_located((By.ID, "updateMiroAppLogo"))
        )
        logo_input.send_keys(
            os.path.join(os.getcwd(), "tests", "data", "gams_logo.png")
        )
        wait.until(
            EC.text_to_be_present_in_element(
                (By.ID, "updateMiroAppLogo_progress"), "Upload complete"
            )
        )
        self.assertEqual(
            get_image_hash_background_style_b64(self.driver, ".app-logo"),
            "7420e77f2fbc70ac06e3f39fa3094860",  # pragma: allowlist secret
            "Updating logo did not work properly",
        )
        WebDriverWait(self.driver, 30).until(
            EC.element_to_be_clickable((By.CLASS_NAME, "btn-save-changes"))
        ).click()
        wait.until(
            EC.text_to_be_present_in_element((By.ID, "staticAppTitle_1"), "bla123")
        )
        static_app_desc = self.driver.find_element(By.ID, "staticAppDesc_1")
        self.assertTrue(
            static_app_desc.text.strip() == "Transport app for UI tests",
            "description text does not match the expected value.",
        )
        self.assertEqual(
            get_image_hash_background_style_b64(self.driver, ".app-logo"),
            "7420e77f2fbc70ac06e3f39fa3094860",  # pragma: allowlist secret
            "Updating logo did not work properly",
        )
        self.driver.switch_to.default_content()
        wait.until(EC.element_to_be_clickable((By.CLASS_NAME, "navbar-brand"))).click()
        wait.until(EC.visibility_of_element_located((By.CLASS_NAME, "launch-app")))

        def get_url_from_style(style: str) -> str:
            start_index = style.find("url(") + len("url(")
            end_index = style.find(")", start_index)
            return self.ui_url + style[start_index:end_index].strip("\"'")

        self.assertEqual(
            get_image_hash(
                self.driver,
                ".app-logo",
                attribute="style",
                cookies={
                    cookie["name"]: cookie["value"]
                    for cookie in self.driver.get_cookies()
                },
                transformer_fn=get_url_from_style,
            ),
            "7420e77f2fbc70ac06e3f39fa3094860",  # pragma: allowlist secret
            "Updated logo not displayed correctly in gallery",
        )
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

    def test_app_environment(self):
        """Test that environment dialog opens with pre-defined environment variables in app_info.json"""
        self.login()
        self.assertEqual(
            get_image_hash(
                self.driver,
                '//link[contains(@rel, "icon")]',
                attribute="href",
                xpath=True,
            ),
            "05b572547194e2dd1700ded2fb5afa89",  # pragma: allowlist secret
            "Favicon not correct",
        )
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
            os.path.join(os.getcwd(), "tests", "data", "transport_environment.miroapp")
        )
        wait.until(
            EC.text_to_be_present_in_element_value(
                (By.ID, "newAppName"), "Transport test app"
            )
        )
        wait.until(
            EC.text_to_be_present_in_element(
                (By.CLASS_NAME, "bootbox-body"), "Scenario Permissions"
            )
        )
        self.assertTrue(
            EC.text_to_be_present_in_element(
                (By.CLASS_NAME, "env-rows"),
                "MIRO_IMPORTER_API_KEY",
            )
        )
        remove_row_buttons = self.driver.find_elements(By.CLASS_NAME, "remove-row")
        self.assertTrue(
            len(remove_row_buttons) == 1,
            "Should have remove-row buttons when opening dialog with pre-defined environment variables.",
        )
        env_name_inputs = self.driver.find_elements(By.CLASS_NAME, "env-name")
        self.assertEqual(
            len(
                [
                    x
                    for x in env_name_inputs
                    if (
                        not x.is_displayed()
                        and x.get_attribute("value") == "MIRO_IMPORTER_API_KEY"
                    )
                ]
            ),
            1,
        )
        self.assertEqual(
            self.driver.find_element(By.CLASS_NAME, "env-description").get_attribute(
                "value"
            ),
            "API key for custom importer",
        )
        self.assertEqual(
            self.driver.find_element(By.CLASS_NAME, "env-value").get_attribute("value"),
            "super_secret_api_key",
        )
        self.assertTrue(
            len(self.driver.find_elements(By.CLASS_NAME, "env-name")) == 2,
            "Should have two environment rows (one pre-filled, one empty).",
        )
        time.sleep(0.5)
        self.driver.find_element(By.CSS_SELECTOR, ".modal-footer .confirm-btn").click()

        wait.until(EC.invisibility_of_element((By.CLASS_NAME, "bootbox-body")))

        self.driver.find_element(By.ID, "btAddApp").click()
        WebDriverWait(self.driver, 30).until(
            EC.invisibility_of_element((By.ID, "expandedAddAppWrapper"))
        )
        wait.until(
            EC.text_to_be_present_in_element(
                (By.ID, "staticAppTitle_1"), "Transport test app"
            )
        )
        self.assertTrue(
            len(
                [
                    x
                    for x in self.driver.find_elements(
                        By.CLASS_NAME, "app-version-field"
                    )
                    if x.text.strip() == "1.0.0"
                ]
            )
            == 1,
            "app version not found.",
        )
        self.assertTrue(
            len(
                [
                    x
                    for x in self.driver.find_elements(
                        By.CLASS_NAME, "app-authors-field"
                    )
                    if x.text.strip() == "by GAMS Development Corp."
                ]
            )
            == 1,
            "app authors not found.",
        )
        self.driver.switch_to.default_content()
        wait.until(EC.element_to_be_clickable((By.CLASS_NAME, "navbar-brand"))).click()
        wait.until(EC.visibility_of_element_located((By.CLASS_NAME, "launch-app")))
        all_buttons = self.driver.find_elements(By.CLASS_NAME, "launch-app")
        visible_buttons = [btn for btn in all_buttons if btn.is_displayed()]
        visible_buttons[0].click()
        wait.until(EC.visibility_of_element_located((By.ID, "loading")))
        self.assertEqual(
            get_image_hash(
                self.driver,
                '//link[contains(@rel, "icon") and not(contains(@rel, "apple-touch"))]',
                attribute="href",
                xpath=True,
                cookies={
                    cookie["name"]: cookie["value"]
                    for cookie in self.driver.get_cookies()
                },
            ),
            "19c0456e4ab146f8e47b5a409cc2541c",  # pragma: allowlist secret
            "Favicon not correct for app",
        )
        self.driver.get(f"{self.ui_url}/app_direct/test_app1")
        wait.until(EC.visibility_of_element_located((By.ID, "loading-screen")))
        self.assertEqual(
            get_image_hash(
                self.driver,
                '//link[contains(@rel, "icon") and not(contains(@rel, "apple-touch"))]',
                attribute="href",
                xpath=True,
                cookies={
                    cookie["name"]: cookie["value"]
                    for cookie in self.driver.get_cookies()
                },
            ),
            "19c0456e4ab146f8e47b5a409cc2541c",  # pragma: allowlist secret
            "Favicon not correct for app_direct",
        )
        self.driver.get(self.ui_url)
