# -*- coding: utf-8 -*-
"""
Created on Wed May 23 1:18:41 2018

@author: HJohnson
"""

import requests
import json
import math

root = "https://www.googleapis.com/youtube/v3"
key = ''
channelID = 'UCvsye7V9psc-APX6wV1twLg'

http_endpoint = ("{0}/search"
                "?part=id"
                "&channelId={1}"
                "&maxResults=50"
                "&order=date"
                "&publishedAfter=2015-01-01T00%3A00%3A00Z"
                "&publishedBefore=2018-05-04T00%3A00%3A00Z"
                "&type=video"
                "&fields=items%2Fid%2FvideoId%2CnextPageToken%2CpageInfo"
                "&key={2}".format(root, channelID, key))

def get_vid_ids(pageToken = None):
    """
    Function notes here
    """
    if pageToken:
        r = requests.get(http_endpoint + "&pageToken=" + pageToken)
    else: 
        r = requests.get(http_endpoint)
    json_data = r.json()
    vidItems = [piece['id']['videoId'] for piece in json_data['items']]
    npages = math.ceil(json_data['pageInfo']['totalResults']/json_data['pageInfo']['resultsPerPage'])
    pageToken = json_data.get('nextPageToken')
    return vidItems, npages, pageToken
       
def get_all_pages():
    all_items = []
    first_items, npages, pageToken = get_vid_ids()
    all_items.extend(first_items)
    for i in range(npages):
        items, n, nextPageToken = get_vid_ids(pageToken)
        all_items += items
        pageToken = nextPageToken
    return all_items

vIds = get_all_pages()       

def get_data(vid_search_url):
    r = requests.get(vid_search_url)
    if r.status_code == 200:
         data = r.json()
         return data
    else:
        print(r.text)
        print(r.status_code)
        
full_data = []
for id in vIds:
    base_url = ("{0}/videos"
                "?part=statistics,snippet"
                "&key={1}".format(root, key))
    vid_search_url = base_url + "&id=" + id
    full_data.append(get_data(vid_search_url))

# save raw .json file 
with open('vid_details.json', 'w') as outfile:
    json.dump(full_data, outfile, sort_keys = True, indent = 4)
     






















