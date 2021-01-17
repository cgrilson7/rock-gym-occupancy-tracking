import requests
from bs4 import BeautifulSoup
import json
import re
import ast
import time

def scrape_counter(gym_id):

    headers = {
        'User-Agent': "Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/84.0.4147.68 Mobile Safari/537.36",
    }

    url = "https://portal.rockgympro.com/portal/public/" + gym_id + "/occupancy"

    r = requests.get(url, headers = headers)

    soup = BeautifulSoup(r.text, 'html.parser')

    data_script = soup.find('body').find('script').string
    data_dict = ast.literal_eval(data_script.partition("var data = ")[2].partition(";")[0])['AAA']

    out = {
        "gym_id": gym_id,
        "accessed_at": time.time(),
        "count": data_dict['count'],
        "capacity": data_dict['capacity']
    }

    return(out)

with open("rgp_gyms.json", "r") as read_file:
    data = json.load(read_file)

results = []
for gym in data:
    current_scrape = scrape_counter(gym['id'])
    results.append(current_scrape)
    time.sleep(2)

with open("rgp_counter_results.json") as data_file:
    past_results = json.load(data_file)

results = past_results + results

with open('rgp_counter_results.json', 'w') as outfile:
    json.dump(results, outfile, indent=4)
