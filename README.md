# ST558---Project-2
Creating predictive models and automating Markdown.

Authors: Michael Bradshaw and Yejun Han.
Date: July 9, 2023

### Brief description of the purpose of this repository:
This repository conducts data summarization and analysis on the [online news popularity data set](https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity). This online news data is subset based on data channel and an analytic report is created for each data channel. We split the data into training and test dataset with the goal to predict the number of shares using predictive modeling techniques, including linear regression and ensemble based tree methods. The best modeling approach is determined by having the lowest root mean square error (RMSE).

### List of R packages used:
The following packages to complete this analysis:

-   tidyverse: for data manipulation (dplyr), and vizualization (ggplot2). 
-   caret: to split data, train and compare models, perform data preprocessing, and tune hyperparameters.
-   rmarkdown: to render code to console and add enhancements to R Markdown
-   randomForest: For classification and regression with Random Forest
-   doParallel: provides support for parallel computing
-   MASS: wide range of statistical functions for various statistical analyses and modeling tasks.
-   leaps: for computing forward/backward subsets of variables in linear regression models
-   gbm : provides functions for fitting and predicting using boosting models
-   GGally: for pairwise plots
  
### Links to the generated analyses:  

-   [Lifestyle article is available here](Lifestyle.html)
-   [Entertainment article is available here](Entertainment.html)
-   [Business article is available here](Business.html)
-   [Social Media article is available here](SocialMedia.html)
-   [Tech article is available here](Tech.html)
-   [World article is available here](World.html)

### Code used to create the analyses of each data channel:
To automate the creation of our markdown documents for each data channel, we first created a data frame that had a vector of channel file names in the first column, and a list of parameters in the second column. 

``` r
# Purpose: Setup automation of markdown reports by data channel
library(rmarkdown) 

# create data channel parameters
channel <- c('Lifestyle', 'Entertainment', 'Business', 'SocialMedia', 'Tech', 'World')

# create .md filenames for each data channel
output_files <- paste0(channel, ".md")

# create a list for each channel with just the channel parameter
params = lapply(channel, FUN = function(x){list(channel = x)})

# put corresponding filenames and channel parameter into a data frame 
reports <- tibble(output_files, params)
```

Next, for each combination of file name and data channel (parameter), we looped through each data channel to create six analytic reports, one for each data channel.

``` r
# Create a report for each channel parameter:
apply(reports, MARGIN = 1,
      FUN = function(x){
        render(input = "ST558_Project-2.Rmd", 
               output_file = x[[1]], 
               params = x[[2]],
               output_format="github_document", 
               output_options=list(html_preview=FALSE, toc=TRUE, toc_depth=2))
      })
```


