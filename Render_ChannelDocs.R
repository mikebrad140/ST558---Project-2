# Purpose: setup automation of markdown reports by data channel

# create data channel variable 
channel <- c('Lifestyle', 'Entertainment', 'Business', 'SocialMedia', 'Tech', 'World')

# create .md filenames for each data channel
output_files <- paste0(channel, ".md")

# create a list for each channel with just the channel parameter
params = lapply(channel, FUN = function(x){list(channel = x)})

# put into a data frame 
reports <- tibble(output_files, params)

library(rmarkdown) 

apply(reports, MARGIN = 1,
      FUN = function(x){
        render(input = "ST558_Project-2.Rmd", 
               output_file = x[[1]], 
               params = x[[2]],
               output_format="github_document", 
               output_options=list(html_preview=FALSE, toc=TRUE, toc_depth=2))
      })

