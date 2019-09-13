#!/usr/bin/env python
# coding: utf-8

# In[84]:


import requests
from bs4 import BeautifulSoup

url = "https://www.c4isrnet.com/artificial-intelligence/"
r1 = requests.get(url)
coverpage = r1.content
soup1 = BeautifulSoup(coverpage, "html5lib")



headlines = soup1.find_all("h4", class_ = " headline")

#Determines number of headlines
article_length = len(headlines)



# In[87]:


titles_list = []

for i in range(article_length):
    x = headlines[i].get_text()
    titles_list.append(x)
    
titles_list

