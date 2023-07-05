# ST558---Project-2
Creating predictive models and automating Markdown.

Authors: Michael Bradshaw and Yejun Han.
Date: July 9, 2023

### Brief description of the purpose of this repository:

### List of R packages used:
The following packages to complete this analysis:

-   Tidyverse: for data manipulation (dplyr), and vizualization (ggplot2). 
-   Caret: to split data, train and compare models, perform data preprocessing, and tune hyperparameters.
-   Rmarkdown: to render code to console

### Links to the generated analyses:  

-   [Lifestyle article is available here](Lifestyle.html)
-   [Entertainment article is available here](Entertainment.html)
-   [Business article is available here](Business.html)
-   [Social Media article is available here](SocialMedia.html)
-   [Tech article is available here](Tech.html)

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


