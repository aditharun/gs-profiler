### Scholar Trends: Visualizing publication trends for scientists using google scholar data

Scholar Trends is a Shiny application that generates visualizations of a scientists publication trends over time using their google scholar data. Input a google scholar URL and, for that author, it outputs plots summarizing their indexed scholarly activity. 

Visualizations include a plot of the number of publications each year, cumulative number of publications, citations each year, cumulative citations, breakdown of most frequently published journals, heatmap of number of articles published in each journal for each year, and number of lead authorships. 

A demo video can be found [here](https://www.youtube.com/watch?v=cZQcAU8D-BM)

#### How to run

This app is hosted at [https://scholartrends.shinyapps.io/scholartrends/](https://scholartrends.shinyapps.io/scholartrends/). When hosted locally, the app runs smoothly. However, when hosted on shinyapps whenever a url is input, the data from the url cannot be retrieved `httr::GET(url, handle = scholar_handle())`. The url is valid and so there must be an issue with the handle argument or some other latent variable that is triggering the error. 
