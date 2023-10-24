from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver import Firefox
import urllib.request
import requests
from PIL import Image
from io import StringIO

driver = webdriver.Firefox(executable_path="C:/Users/sberry5/Documents/geckodriver-v0.30.0-win64/geckodriver.exe")
driver.get("http://inside.nd.edu")
driver.get("https://onlinephoto.nd.edu/#!/instructor/index/termcode/202120/course/70310-01/courseid/21AA592BF50D414E5565FFCBF7559978/page/1/view/faces")

students = []

for element in driver.find_elements_by_class_name('face.student'):
    students.append(element.get_attribute("first-name") + "_" + element.get_attribute("last-name"))

photo_locations = []

for element in driver.find_elements_by_class_name('face-image.get-details'):
    photo_locations.append(element.get_attribute("src"))

for i in range(len(students)):
    file_name = "C:/Users/sberry5/Documents/teaching/UDA/data/student_images/" +  students[i] + ".png"
    driver.get(photo_locations[i])
    driver.save_screenshot(file_name)