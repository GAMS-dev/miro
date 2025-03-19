import base64
import hashlib

import requests
from selenium.webdriver.common.by import By


def get_image_hash(driver, css_selector, attribute="src", xpath=False, cookies=None):
    image_element = driver.find_element(By.XPATH if xpath else By.CSS_SELECTOR, css_selector)

    image_url = image_element.get_attribute(attribute)
    print(image_url)
    if not image_url:
        raise ValueError("Image URL not found.")

    session = requests.Session()
    if cookies:
        session.cookies.update(cookies)
    response = session.get(image_url, timeout=20)
    response.raise_for_status()
    image_content = response.content

    return hashlib.md5(image_content).hexdigest()


def select_selectize_options(driver, target_element, options):
    """Selects options in selectize dropdown menu"""
    js_code = """
    var target = arguments[0];
    target.selectize.setValue(arguments[1]);
    """
    driver.execute_script(js_code, target_element, options)


def get_selectize_options(driver, target_element):
    """Selects options in selectize dropdown menu"""
    return driver.execute_script(
        "return arguments[0].selectize.getValue();", target_element
    )


def drop_file(driver, target_element, file_path):
    """
    Simulates dropping a file onto a target element with an 'on-drop' handler.

    Args:
        driver: Selenium WebDriver instance.
        target_element: The WebElement to drop the file onto.
        file_path: Path to the file to be dropped.

    Returns:
        None
    """
    with open(file_path, "rb") as file:
        file_content = file.read()
    data_url = base64.b64encode(file_content).decode()

    js_code = """
    var target = arguments[0];
    var dataTransfer = new DataTransfer();

    // Add the file to the DataTransfer object
    var file = new File([new Uint8Array(atob(arguments[1]).split('').map(c => c.charCodeAt(0)))], arguments[2]);
    dataTransfer.items.add(file);

    // Create and dispatch the drop event
    var dropEvent = new DragEvent('drop', {
        dataTransfer: dataTransfer,
        bubbles: true,
        cancelable: true
    });
    target.dispatchEvent(dropEvent);
    """
    driver.execute_script(js_code, target_element, data_url, file_path.split("/")[-1])
