### Scholar Trends: Visualizing publication trends for scientists using google scholar data

Scholar Trends is a Shiny application that generates visualizations of a scientists publication trends over time using their google scholar data. Input a google scholar URL and, for that author, it outputs plots summarizing their indexed scholarly activity. 

Visualizations include a plot of the number of publications each year, cumulative number of publications, citations each year, cumulative citations, breakdown of most frequently published journals, heatmap of number of articles published in each journal for each year, and number of lead authorships. 

A demo video of the app can be found [here](https://www.youtube.com/watch?v=cZQcAU8D-BM)

#### How to run

Clone this repository onto your local machine. Navigate to the folder `gs-profiler` on your terminal. Then do `cd scholartrends` folder, and open up an R instance. Make sure you have `tidyverse, scholar, scales, cowplot, shiny, and htmltools` downloaded. If not, run `install.packages(c("tidyverse, scholar, scales, cowplot, shiny, and htmltools"))`. Then run `shiny::runApp("app.R")`.


#### Problems with deployment

This app is hosted at [https://scholartrends.shinyapps.io/scholartrends/](https://scholartrends.shinyapps.io/scholartrends/). When hosted locally, the app runs smoothly. However, when hosted on shinyapps whenever a url is input, the data from the url cannot be retrieved `httr::GET(url, handle = scholar_handle())`. The url is valid and so there must be an issue with the handle argument or some other latent variable that is triggering the error. Removing the `scholar_handle()` dependency by rewriting the function `get_scholar_resp()` in `scholartrends/tweak-scholar.R` which performs the GET call returns the same situation (locally ok, server fail). I do not have a solution to this yet but need to find one. If you find one or have ideas, please let me know. My email is [adith.3.arun@gmail.com](mailto::adith.3.arun@gmail.com)
