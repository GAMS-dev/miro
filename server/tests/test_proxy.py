import os
import time
import unittest

from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

# Load environment variables
ENGINE_USER = os.getenv("ENGINE_USER")
ENGINE_PASSWORD = os.getenv("ENGINE_PASSWORD")
UI_URL = os.getenv('MIRO_PROXY_URL', 'http://docker:8080')
IN_CI = os.getenv('CI', '') != ''
print(os.getcwd())
if not ENGINE_USER or not ENGINE_PASSWORD:
    raise EnvironmentError(
        "Please set the ENGINE_USER and ENGINE_PASSWORD environment variables.")


class UITests(unittest.TestCase):
    def setUp(self):
        self.ui_url = UI_URL
        test_browser = os.getenv('TEST_BROWSER', 'chrome')
        if test_browser == 'firefox':
            if IN_CI:
                self.driver = webdriver.Remote(
                    options=webdriver.FirefoxOptions(),
                    command_executor='http://selenium__standalone-firefox:4444/wd/hub',
                )
            else:
                self.driver = webdriver.Firefox()
        elif test_browser == 'chrome':
            if IN_CI:
                self.driver = webdriver.Remote(
                    options=webdriver.ChromeOptions(),
                    command_executor='http://selenium__standalone-chrome:4444/wd/hub',
                )
            else:
                self.driver = webdriver.Chrome()
        else:
            raise ValueError(f'{test_browser} is not chrome or firefox')

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

        self.driver.find_element(
            By.CSS_SELECTOR, "button[type='submit']").click()
        WebDriverWait(self.driver, 10).until(
            EC.visibility_of_element_located((By.ID, "navAdminPanel"))
        )
        self.assertEqual(self.driver.find_element(
            By.CSS_SELECTOR, "#navbar .navbar-right .navbar-text").text, ENGINE_USER)

    def logout(self):
        self.driver.switch_to.default_content()
        wait = WebDriverWait(self.driver, 3)
        sign_out_element = wait.until(
            EC.presence_of_element_located(
                (By.CSS_SELECTOR, ".navbar-right a"))
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
            delete_links = self.driver.find_elements(
                By.CLASS_NAME, "delete-app-button")
            if not delete_links:
                break
            delete_link = delete_links[0]
            delete_link.click()
            wait.until(
                EC.text_to_be_present_in_element(
                    (By.CLASS_NAME, "bootbox-body"),
                    "Are you sure you want to remove this app? This cannot be undone."
                )
            )
            accept_buttons = self.driver.find_elements(
                By.CLASS_NAME, "bootbox-accept")
            visible_accept_button = next(
                btn for btn in accept_buttons if btn.is_displayed())
            visible_accept_button.click()
            bootbox_input = wait.until(
                EC.visibility_of_element_located(
                    (By.CLASS_NAME, "bootbox-input-text"))
            )
            bootbox_input.send_keys("asd")
            accept_buttons = self.driver.find_elements(
                By.CLASS_NAME, "bootbox-accept")
            visible_accept_button = next(
                btn for btn in accept_buttons if btn.is_displayed())
            visible_accept_button.click()
            self.assertTrue(self.driver.find_element(By.CLASS_NAME, "bootbox-body").is_displayed(),
                            "Bootbox body disappeared unexpectedly after entering 'asd' when removing app data.")
            bootbox_input.clear()
            bootbox_input.send_keys("test_app1")
            accept_buttons = self.driver.find_elements(
                By.CLASS_NAME, "bootbox-accept")
            visible_accept_button = next(
                btn for btn in accept_buttons if btn.is_displayed())
            visible_accept_button.click()
            wait.until(EC.invisibility_of_element(
                (By.CLASS_NAME, "bootbox-body")))
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

        # Step 2: Attempt to sign in with the wrong password
        username_field = self.driver.find_element(By.ID, "username")
        password_field = self.driver.find_element(By.ID, "password")
        sign_in_button = self.driver.find_element(
            By.CSS_SELECTOR, "button[type='submit']")

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
            EC.visibility_of_element_located(
                (By.CLASS_NAME, "alert-danger"))
        )
        self.assertTrue("Invalid user name or password" in alert_element.text,
                        "Could not find invalid password alert")

        self.login()

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
        time.sleep(1)

        wait.until(
            EC.element_to_be_clickable((By.ID, "addApp"))).click()
        file_input = wait.until(
            EC.presence_of_element_located((By.ID, "miroAppFile"))
        )
        wait.until(
            EC.visibility_of_element_located(
                (By.CSS_SELECTOR, "label[for='miroAppFile']"))
        )

        file_input.send_keys(os.path.join(
            os.getcwd(), "tests", "data", "transport.miroapp"))
        wait.until(
            EC.text_to_be_present_in_element_value(
                (By.ID, "newAppName"), "Transport test app")
        )

        new_app_desc = self.driver.find_element(By.ID, "newAppDesc")
        self.assertTrue(new_app_desc.get_attribute("value").strip() == "Transport app for UI tests",
                        "newAppDesc value does not match the expected value.")
        new_app_env = self.driver.find_element(By.ID, "newAppEnv")
        new_app_env.clear()
        new_app_env.send_keys('{"MIRO_LANG":"de"}')
        new_app_name_input = self.driver.find_element(By.ID, "newAppName")
        new_app_name_input.clear()
        new_app_name_input.send_keys("Transport app")
        self.driver.find_element(By.ID, "btAddApp").click()
        WebDriverWait(self.driver, 30).until(EC.invisibility_of_element(
            (By.ID, "expandedAddAppWrapper")))
        wait.until(
            EC.text_to_be_present_in_element(
                  (By.ID, "staticAppTitle_1"), "Transport app")
        )
        static_app_desc = self.driver.find_element(By.ID, "staticAppDesc_1")
        self.assertTrue(static_app_desc.text.strip() == "Transport app for UI tests",
                        "description text does not match the expected value.")
        self.driver.switch_to.default_content()
        wait.until(EC.element_to_be_clickable(
            (By.CLASS_NAME, "navbar-brand"))).click()
        wait.until(
            EC.visibility_of_element_located((By.CLASS_NAME, "launch-app"))
        )
        all_buttons = self.driver.find_elements(By.CLASS_NAME, "launch-app")
        visible_buttons = [btn for btn in all_buttons if btn.is_displayed()]
        self.assertTrue(len(
            visible_buttons) == 1, "More than one visible 'launch-app' button found.")
        self.assertTrue(visible_buttons[0].get_attribute("href").endswith("/app/test_app1"),
                        "The href of the launch-app link is not '/app/test_app1'.")
        visible_buttons[0].click()
        wait.until(
            EC.visibility_of_element_located((By.ID, "loading"))
        )
        WebDriverWait(self.driver, 30).until(EC.frame_to_be_available_and_switch_to_it(
            (By.ID, "shinyframe")))
        wait.until(
            EC.text_to_be_present_in_element(
                (By.CLASS_NAME, "readme-wrapper"),
                "A Transportation Problem with multiple version LP/MIP/MINLP"
            )
        )
        self.assertTrue(self.driver.find_element(By.ID, "btSolve").text.strip() == "Modell lösen",
                        "Solve button has correct text in German (MIRO_LANG applied correctly)")
        self.driver.switch_to.default_content()
