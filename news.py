#!/usr/bin/env python
# coding: utf-8

# In[84]:


import requests
from bs4 import BeautifulSoup

#Set URL to the C4ISRNET - artificial intelligence topic page
url = "https://www.c4isrnet.com/artificial-intelligence/"

#Establish request
r1 = requests.get(url)
news = r1.content

#Establish BeautifulSoup
soup = BeautifulSoup(news, "html5lib")

#For this, I inspected the HTML elements of the webpage and identified the relevent HTML elements to scrape i.e. headlines
headlines = soup.find_all("h4", class_ = " headline")

#Determines number of headlines
article_length = len(headlines)

#Create empty headline list
headline_list = []

#Create for loop that iterates through each article, extracts headline, and appends each headline to the list above
for i in range(article_length):
    x = headlines[i].get_text()
    headline_list.append(x)
    

