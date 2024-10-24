import os
import unittest

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

# Load environment variables
ENGINE_USER = os.getenv("ENGINE_USER")
ENGINE_PASSWORD = os.getenv("ENGINE_PASSWORD")
UI_URL = os.getenv('MIRO_PROXY_URL', 'http://docker:8080')
IN_CI = os.getenv('CI', '') != ''

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

    def tearDown(self):
        self.driver.quit()

    def test_ui_loads(self):
        self.driver.get(self.ui_url)
        self.assertTrue(self.driver.find_element(
            By.CLASS_NAME, 'form-signin').is_displayed())

    def test_login(self):
        self.driver.get(self.ui_url)

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

        # Step 3: Sign in with the correct password
        username_field = self.driver.find_element(By.ID, "username")
        password_field = self.driver.find_element(By.ID, "password")

        username_field.clear()
        password_field.clear()

        # Enter the correct credentials
        username_field.send_keys(ENGINE_USER)
        password_field.send_keys(ENGINE_PASSWORD)

        self.driver.find_element(
            By.CSS_SELECTOR, "button[type='submit']").click()

        # Wait for the noAppsInfo element to be visible
        WebDriverWait(self.driver, 10).until(
            EC.visibility_of_element_located((By.ID, "noAppsInfo"))
        )
        self.assertEqual(self.driver.find_element(
            By.CSS_SELECTOR, "#navbar .navbar-right .navbar-text").text, ENGINE_USER)

        # open admin panel
        self.driver.find_element(By.ID, "navAdminPanel").click()

        WebDriverWait(self.driver, 10).until(
            EC.frame_to_be_available_and_switch_to_it((By.ID, "shinyframe"))
        )

        WebDriverWait(self.driver, 30).until(
            EC.visibility_of_element_located((By.ID, "addAppBox"))
        )
