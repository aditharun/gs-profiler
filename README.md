### Publication profiles of researchers from their Google Scholar pages

A researcher's Google Scholar page allows you to see a snapshot of their publication history and has a graph with the number of citations their papers have received on a yearly basis. It may be of interest to learn more about a researcher's publication profile. This tool may be useful particularly for junior researchers trying to understand the trajectory and current state of published research output from more senior researchers. 


### Overview

The code in this repository displays the following information for a given researcher: 
  - graph of the number of citations for each year
  - graph of the number of papers published for each year
  - graph and csv of the number of times the researcher has published in a given journal
  - graph of the number of times the researcher has published in a given journal segmented by year

All of this output is saved on the user's computer within the folder the code lives in, under the subfolder ```results/{userid}``` where userid is the unique id associated with a researchers google scholar profile. It can be found in the url to their google scholar. For example, ```l8WuQJgAAAAJ``` is the user id for the profile with the following url:  ```https://scholar.google.com/citations?user=l8WuQJgAAAAJ&hl=en```

### Usage

Download or clone this github repository to your local machine. Open the terminal and navigate to this folder ```gs-profiler```. Make sure you have R and python3 on your machine - if you are unsure run the command ```Rscript --version``` and ```python3 --version``` and make sure it returns a valid version number and not an error. 

If you do not have the following packages installed, do the following: 
- python:  ```pip3 install scholarly```
- R: 
  Open R in your terminal (simply type `R`) or Rstudio
  Install any packages you do not have already installed: 
    ```install.packages(c("tidyverse", "cowplot", "gridExtra"))```

Get one of the following (preferably one of the first two, though they all work): 
  a) URL to their google scholar
  b) User ID to their google scholar (a part of the URL)
  c) Researcher's name (as it appears on their Google Scholar profile)

The exact command to run for each of the following three scenarios: 
  a) ```Rscript main.R --url '{url}'``` where {url} is the url of the google scholar page surrounded by single quotes
  b) ```Rscript main.R --user_id '{user_id}'``` where {user_id} is the user id of the google scholar page surrounded by single quotes
  c) ```Rscript main.R --name '{name}'``` where {name} is the name of the researcher as it appears on their profile (e.g., 'Andrej Karpathy')
  
