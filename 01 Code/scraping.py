#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Oct  6 21:38:12 2020

@author: paulmora
"""

import pandas as pd
import requests
from bs4 import BeautifulSoup
from tqdm import tqdm
import re

# Paths
main_path = r"/Users/paulmora/Documents/projects/polarized_senate"
raw_path = r"{}/00 Raw".format(main_path)
code_path = r"{}/01 Code".format(main_path)
data_path = r"{}/02 Data".format(main_path)
output_path = r"{}/03 Output".format(main_path)

# %% Scraping

"""
We start by scraping all the links of the senate and the house over the years
for that we look for certain gap fillers in our links.
"""

base_url = "https://www.congress.gov/roll-call-votes"
page = requests.get(base_url)
soup = BeautifulSoup(page.content, "html.parser")
str_soup = str(soup)
parsed_soup = str_soup.split("href")

dict_pattern = {
    "house": r'http://clerk.house.gov/evs/(.*)/index.asp',
    "senate": (r'http://www.senate.gov/legislative/LIS/'
               r'roll_call_lists/vote_menu_(.*).htm')
    }
links_additions = {"house": [], "senate": []}
for key in links_additions:
    pattern = dict_pattern[key]
    for snippet in parsed_soup:
        try:
            link_string = re.search(pattern, snippet).group(1)
            links_additions[key].append(link_string)
        except AttributeError:
            pass

# %% Senate

headers = {'User-Agent': 'Mozilla/5.0 (Windows NT 6.1; WOW64) ' +
           'AppleWebKit/537.36 (KHTML, like Gecko) ' +
           'Chrome/56.0.2924.76 Safari/537.36',
           'Accept-Language': 'en-US,en;q=0.8'
           }
voting_url = (r"https://www.senate.gov/legislative/LIS/roll_call_lists/"
              r"roll_call_vote_cfm.cfm?congress={}&session={}&vote={}")
num_url = ("https://www.senate.gov/legislative/LIS/"
           "roll_call_lists/vote_menu_{}.htm")
num_pattern = "\((.*)\) "
main_df_list = []
for congress in tqdm(links_additions["senate"]):

    try:
        url = num_url.format(congress)
        page = requests.get(url, headers=headers)
        soup = BeautifulSoup(page.content, "html.parser")
        table_rows = soup.find_all("tr")
        row_content = []
        for tr in table_rows:
            td = tr.find_all("td")
            row = [tr.text for tr in td]
            row_content.append(row)
        num_decisions = int(re.search(num_pattern, row_content[1][0]).group(1))

        for num in tqdm(range(1, num_decisions+1)):
            try:
                string_num = ("0" * (5-len(str(num))) + str(num))
                congress_num = congress.split("_")[0]
                session_num = congress.split("_")[1]
                url = voting_url.format(congress_num,
                                        session_num,
                                        string_num)
                page = requests.get(url, headers=headers)
                soup = BeautifulSoup(page.content, "html.parser")
                str_soup = str(soup)

                table_part = str_soup\
                    .split("Alphabetical by Senator Name")[1]\
                    .split("Grouped By Vote Position")[0]\
                    .split('class="contenttext">')[1]\
                    .split("</span>")[0]

                cleaned_table = table_part\
                    .replace(", <b>", " ")\
                    .replace("<br/>", " ")\
                    .replace("</b>\n", "|")\
                    .split("| ")

                df_cleaned_table = pd.DataFrame(cleaned_table,
                                                columns=["info"])
                df_cleaned_table.loc[:, "congress"] = congress
                df_cleaned_table.loc[:, "bill_num"] = num

                main_df_list.append(df_cleaned_table)
            except:
                True
    except:
        True

main_df = pd.concat(main_df_list)
main_df.to_excel(r"{}/senate_data.xlsx".format(data_path))
