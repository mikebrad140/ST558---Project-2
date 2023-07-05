ST558: Project 2
================
Michael Bradshaw and Yejun Han
2023-07-05

- <a href="#channel-of-interest-world"
  id="toc-channel-of-interest-world">Channel of Interest: World</a>
  - <a href="#introduction-to-the-project-to-be-completed-by-yejun"
    id="toc-introduction-to-the-project-to-be-completed-by-yejun">Introduction
    to the Project (To be Completed by Yejun)</a>
  - <a href="#import-the-data" id="toc-import-the-data">Import the Data</a>
  - <a href="#splitting-the-data-into-test-and-training-datasets"
    id="toc-splitting-the-data-into-test-and-training-datasets">Splitting
    the data into test and training datasets</a>
  - <a href="#summarizations-both-michael-and-yejun"
    id="toc-summarizations-both-michael-and-yejun">Summarizations (Both
    Michael and Yejun)</a>
  - <a href="#modeling-both-michael-and-yejun"
    id="toc-modeling-both-michael-and-yejun">Modeling (Both Michael and
    Yejun)</a>

# Channel of Interest: World

## Introduction to the Project (To be Completed by Yejun)

## Import the Data

In this section, we first import the raw online news popularity dataset.
We remove the non-predictor variables named url and timedelta. Next, we
create three new variables: the first is called *channel* based on the
values of the *data_channel_is\_* variables. The second new variable is
called *day_of_week* based on the values of the *weekday_is\_*
variables. The third new variable is called *content_length* based on
the values of the *n_tokens_content* variable. The *content_length
*variable is assigned values based on the conditionals provided,
categorizing the length of the content as “Very Short,” “Short,”
“Medium,” or “Long.”

Lastly, we subset the imported newsData into separate data frames for
each specific channel.

``` r
#Import the newsData csv file:
newsData <- read.csv(file="..//OnlineNewsPopularity//OnlineNewsPopularity.csv")

# Create single variable for data channel: 
newsData <- newsData %>% 
  dplyr::select(-url, -timedelta) %>%
  mutate(channel = ifelse(data_channel_is_lifestyle == 1, "Lifestyle",
                   ifelse(data_channel_is_entertainment == 1, "Entertainment",
                   ifelse(data_channel_is_bus == 1, "Business",
                   ifelse(data_channel_is_socmed == 1, "SocialMedia",
                   ifelse(data_channel_is_tech == 1, "Tech",
                   ifelse(data_channel_is_world == 1, "World", "Other")))))),
         day_of_week = ifelse(weekday_is_monday == 1, "Monday",
                       ifelse(weekday_is_tuesday == 1, "Tuesday",
                       ifelse(weekday_is_wednesday == 1, "Wednesday",
                       ifelse(weekday_is_thursday == 1, "Thursday",
                       ifelse(weekday_is_friday == 1, "Friday",
                       ifelse(weekday_is_saturday == 1, "Saturday", "Sunday")))))),
         content_length = ifelse(n_tokens_content <= 250, "Very Short",
                        ifelse(n_tokens_content <= 410, "Short",      
                        ifelse(n_tokens_content <= 750, "Medium", "Long")))
         )
newsData$channel <- as.factor(newsData$channel) #Converting to factor
newsData$day_of_week <- factor(newsData$day_of_week,
                               levels = c("Sunday", "Monday", "Tuesday", 
                                          "Wednesday", "Thursday", 
                                          "Friday", "Saturday"))
newsData$content_length <- as.factor(newsData$content_length)
# Subset the data for each data channel
newsData_channel <- newsData %>% filter(channel == params$channel)
```

## Splitting the data into test and training datasets

This section creates training and test indices based on the *shares*
variable, and splits the data frames for each channel into separate
training and test sets for modeling.

``` r
# Set the seed for reproducibility
set.seed(717)

# Create the training and test indices
trainIndices <- createDataPartition(newsData_channel$shares, p = 0.7, list = FALSE)

# Split the data into training and test sets
train_Data <- newsData_channel[trainIndices, ]
test_Data <- newsData_channel[-trainIndices, ]
```

## Summarizations (Both Michael and Yejun)

In this first example, we look at a table summarizing the statistics of
the shares variable in our training dataset. We then create a histogram
to visualize the distribution of shares.

In a normal distribution,the histogram will have a symmetric shape with
a peak at the center and tails that extend symmetrically in both
directions. In a skewed distribution, the data can be either skewed to
the right or skewed to the left. In a right skewed distribution, the
histogram will have a long tail on the right side and a shorter tail on
the left side. The majority of observations will be concentrated on the
left side. In a left skewed distribution, the histogram will have a long
tail on the left side and a shorter tail on the right side, with the
majority of observations concentrated on the right side.

``` r
summary(train_Data$shares)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      42     827    1100    2199    1800  111300

``` r
# Histogram of shares
ggplot(train_Data , aes(x = shares)) +
  geom_histogram(binwidth = 2500, fill = "blue") +
  labs(x = "Shares", y = "Frequency") +
  ggtitle("Distribution of Shares") +
  theme_classic()
```

![](World_files/figure-gfm/Summarization1-1.png)<!-- -->

In this second table, we display summary statistics for the shares
variable within our training dataset by day of the week. We display the
count (n), the total sum of shares (total_shares), the average shares
(average_shares), the standard deviation of shares (sd_shares), the
minimum shares (min_shares), and the maximum shares (max_shares) for
each group.

Next, we examine the is_weekend categorical variable by the shares
variable. We calculate the mean shares for each group (weekend
vs. non-weekend days).

Lastly, we create a bar plot to visualize the average shares by day of
the week. Higher bars indicate more shares on that day of the week.

``` r
#Shares by day of the week
average_shares <- train_Data %>%
  group_by(day_of_week) %>%
  summarise(n = n(),
            total_shares = sum(shares),
            average_shares = mean(shares),
            sd_shares = sd(shares),
            min_shares = min(shares),
            max_shares = max(shares))
average_shares
```

    ## # A tibble: 7 × 7
    ##   day_of_week     n total_shares average_shares sd_shares min_shares max_shares
    ##   <fct>       <int>        <int>          <dbl>     <dbl>      <int>      <int>
    ## 1 Sunday        395      1056801          2675.     4966.         89      55600
    ## 2 Monday        954      2235862          2344.     5853.         43     108400
    ## 3 Tuesday      1083      2319276          2142.     4699.         42      69300
    ## 4 Wednesday    1114      2131154          1913.     3298.         48      53500
    ## 5 Thursday     1072      2413749          2252.     4563.         42      67700
    ## 6 Friday        914      1876015          2053.     4604.         70     111300
    ## 7 Saturday      368       940019          2554.     3431.         52      26900

``` r
weekend_mean_shares <- train_Data %>%
  group_by(is_weekend) %>%
  rename(Weekend = is_weekend) %>%
  mutate(Weekend = ifelse(Weekend == 0, "No", "Yes")) %>%
  summarize(mean_shares = mean(shares))
  
weekend_mean_shares
```

    ## # A tibble: 2 × 2
    ##   Weekend mean_shares
    ##   <chr>         <dbl>
    ## 1 No            2137.
    ## 2 Yes           2617.

``` r
ggplot(average_shares, aes(x = day_of_week, y = average_shares, 
                           fill = day_of_week)) +
  geom_col() +
  labs(x = "Day of the Week", y = "Average Shares", fill = "Day of the Week") +
  ggtitle("Average Shares by Day of the Week") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](World_files/figure-gfm/Summarization2-1.png)<!-- -->

These tables display the average number of shares for different content
length categories. Summary statistics such as the count of observations,
the average number of shares, the standard deviation of shares, the
minimum number of shares (min_shares), and the maximum number of shares
(max_shares), and the range of shares (range_shares) are computed for
each content length category.

Next, a horizontal bar chart is created to help us visualize these
results by showing the average number of shares on the x-axis and the
content length categories (content_length) on the y-axis.

``` r
# Calculate the average shares by content length category
average_shares_byContent <- train_Data %>%
  group_by(content_length) %>%
  summarise(n = n(),
            average_shares = mean(shares),
            sd_shares = sd(shares),
            min_shares = min(shares),
            max_shares = max(shares),
            range_shares = max_shares - min_shares)
average_shares_byContent
```

    ## # A tibble: 4 × 7
    ##   content_length     n average_shares sd_shares min_shares max_shares range_shares
    ##   <fct>          <int>          <dbl>     <dbl>      <int>      <int>        <int>
    ## 1 Long            1541          2047.     4066.         42     108400       108358
    ## 2 Medium          2197          1979.     3531.         42      69300        69258
    ## 3 Short           1348          2355.     5409.         57     111300       111243
    ## 4 Very Short       814          2820.     6261.         57      96500        96443

``` r
# Create the horizontal bar chart
ggplot(average_shares_byContent, aes(x = average_shares, y = content_length, fill = content_length)) +
  geom_bar(stat = "identity") +
  labs(x = "Average Shares", y = "Content Length Category", fill = "Content Length") +
  ggtitle("Average Shares by Content Length Category") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 0.5))
```

![](World_files/figure-gfm/Summarization3-1.png)<!-- -->

**Yejun to add more.**

## Modeling (Both Michael and Yejun)

Each group member should contribute a linear regression model and an
ensemble tree-based model.

### Linear Regression Models

Linear regression is a modeling technique that attempts to form a linear
relationship between a response (dependent variable (i.e. shares)) and
one or more predictor (independent variables). In simple linear
regression, there is one predictor or independent variable as opposed to
multiple linear regression where there are multiple predictors. Linear
regression seeks to explain the values of our response variable based on
our predictor variables by fitting a straight line to the data that
minimizes the sum of the squared differences between the observed and
predicted values.

Our online news dataset contains a large number of predictors and we
don’t have a great sense of their relationship to the shares variable.
These relationships may change between data channels as well. If we look
at correlations between shares and the rest of the possible numeric
variables, we see a mix of small positive relationship, small negative
relationships, and some with little to no relationship.

``` r
# Calculate correlations between shares and all numeric variables
correlations <- cor(train_Data[, sapply(train_Data, is.numeric)], 
                    train_Data$shares)
correlations
```

    ##                                        [,1]
    ## n_tokens_title                 0.0363140305
    ## n_tokens_content              -0.0218942533
    ## n_unique_tokens               -0.0086914321
    ## n_non_stop_words              -0.0317782004
    ## n_non_stop_unique_tokens      -0.0159244643
    ## num_hrefs                      0.0390584178
    ## num_self_hrefs                 0.0052523846
    ## num_imgs                       0.0921676878
    ## num_videos                     0.0402585283
    ## average_token_length          -0.0556185438
    ## num_keywords                   0.0360912471
    ## data_channel_is_lifestyle                NA
    ## data_channel_is_entertainment            NA
    ## data_channel_is_bus                      NA
    ## data_channel_is_socmed                   NA
    ## data_channel_is_tech                     NA
    ## data_channel_is_world                    NA
    ## kw_min_min                     0.0085365107
    ## kw_max_min                     0.0243046564
    ## kw_avg_min                     0.0222351299
    ## kw_min_max                     0.0128643362
    ## kw_max_max                     0.0037676457
    ## kw_avg_max                     0.0152142658
    ## kw_min_avg                     0.0121263283
    ## kw_max_avg                     0.0587380474
    ## kw_avg_avg                     0.1074988330
    ## self_reference_min_shares      0.0198678553
    ## self_reference_max_shares      0.0354566023
    ## self_reference_avg_sharess     0.0323713186
    ## weekday_is_monday              0.0138454929
    ## weekday_is_tuesday            -0.0059082840
    ## weekday_is_wednesday          -0.0299962394
    ## weekday_is_thursday            0.0054178396
    ## weekday_is_friday             -0.0136263517
    ## weekday_is_saturday            0.0199577196
    ## weekday_is_sunday              0.0277830640
    ## is_weekend                     0.0350771130
    ## LDA_00                         0.0412199465
    ## LDA_01                         0.0451248476
    ## LDA_02                        -0.1188707827
    ## LDA_03                         0.1021802460
    ## LDA_04                         0.0301423078
    ## global_subjectivity            0.0327718530
    ## global_sentiment_polarity      0.0280152081
    ## global_rate_positive_words     0.0247260901
    ## global_rate_negative_words    -0.0059814563
    ## rate_positive_words           -0.0007675457
    ## rate_negative_words           -0.0330440574
    ## avg_positive_polarity          0.0100715998
    ## min_positive_polarity          0.0099493197
    ## max_positive_polarity          0.0056978001
    ## avg_negative_polarity         -0.0126013867
    ## min_negative_polarity          0.0044351972
    ## max_negative_polarity         -0.0128877070
    ## title_subjectivity             0.0252576157
    ## title_sentiment_polarity       0.0290981344
    ## abs_title_subjectivity        -0.0336520912
    ## abs_title_sentiment_polarity   0.0336207418
    ## shares                         1.0000000000

In general, we see that the number of videos (num_videos), the number of
images (num_imgs) and the number of links (num_hrefs) have small
positive correlations. We also know from our descriptive analysis that
day of the week seems to influence the number of shares as well, but to
eliminate redundancy lets remove the weekday_is variables and the
is_weekend variables. Other variables that we might expect to influence
number of shares include the number of words in the title
(n_tokens_title), and the number of words in the content
(n_tokens_content). The LDA topic also appears correlated across
different data channels, so let’s include all of these variables since
they probably vary across different data channels. Lastly, in the
overall news dataset we notice that average keyword (kw_avg_avg) had a
positive correlation with shares, so let’s keep this in our model.

We can also remove all the data_channel variables as this information is
not needed within each data channel analysis.

``` r
# subset the data: 
train_Data_model <- dplyr::select(train_Data, shares, num_videos, n_tokens_content, n_tokens_title, num_imgs, num_hrefs, self_reference_min_shares, LDA_00, LDA_01, LDA_02, LDA_03, LDA_04, kw_avg_avg,day_of_week)

# all predictors from our subset:
lm_fit1 <- train(shares ~ . , data = train_Data_model, 
                            method = "lm", preProcess = c("center", "scale"),
                            trControl = trainControl(method = "cv", number = 5))
```

Here, we assess how well our first linear model fits the data based on
the ability to accurately predict the shares variable. The RMSE provides
an evaluation of the model’s performance.

``` r
# check the fit of our first linear model:
predslinear1 <- predict(lm_fit1, newdata = test_Data)
# See how well the model fits
postResample(predslinear1, obs = test_Data$shares)
```

    ##         RMSE     Rsquared          MAE 
    ## 8.551741e+03 1.720967e-02 2.083770e+03

Now, let’s suppose we were way off with all of our assumptions in terms
of the key variables to include. Let’s try a different appraoch and use
the leapSeq algorithm. This approach evaluates different subsets of
features to determine the subset that produces the best model
performance. And in this situation, we can do this with all of the
possible predictor variables, not just our subset of key variables. This
may require more computational power, so let’s do parallel processing.

``` r
# Parallel Processing
cores <- 10
cl <- makeCluster(cores)
registerDoParallel(cl)

step_model_seq <- train(shares ~ . , data = train_Data, 
                 method = "leapSeq", preProcess = c("center", "scale"),
                 tuneGrid = data.frame(nvmax = 1:10), # up to 10 predictors max
                 trControl = trainControl(method = "cv", number = 5))
```

    ## Reordering variables and trying again:

``` r
step_model_seq$results
```

    ##    nvmax     RMSE     Rsquared      MAE   RMSESD   RsquaredSD    MAESD
    ## 1      1 4435.281 0.0003920796 1804.903 1352.518 0.0003622015 115.7691
    ## 2      2 4434.920 0.0038729239 1796.635 1340.706 0.0056909032 120.2659
    ## 3      3 4422.402 0.0102704623 1795.147 1333.142 0.0071987214 119.8382
    ## 4      4 4424.859 0.0097572662 1795.528 1331.395 0.0072148442 117.4081
    ## 5      5 4430.563 0.0087189214 1799.275 1320.902 0.0106246137 112.4196
    ## 6      6 4423.574 0.0118060142 1797.437 1332.320 0.0137689097 115.5293
    ## 7      7 4412.504 0.0171284077 1792.412 1327.092 0.0129419058 115.8020
    ## 8      8 4412.971 0.0157734325 1795.863 1319.554 0.0147662537 105.5710
    ## 9      9 4411.945 0.0162284267 1792.064 1317.475 0.0153379222 103.9021
    ## 10    10 4392.839 0.0237089853 1789.962 1322.822 0.0090077668 102.8728

``` r
step_model_seq$bestTune
```

    ##    nvmax
    ## 10    10

``` r
# Stop parallel processing
stopCluster(cl)
registerDoSEQ()
```

Here, we assess how well our second linear model fits the data based on
the ability to accurately predict the shares variable. The RMSE provides
an evaluation of the model’s performance.

``` r
predslinear2 <- predict(step_model_seq, newdata = test_Data)
# See how well the model fits
postResample(predslinear2, obs = test_Data$shares)
```

    ##         RMSE     Rsquared          MAE 
    ## 8.560540e+03 1.514919e-02 2.096793e+03

### Ensemble Tree-Based Models

### Random Forest Model:

Random Forest is an ensemble learning technique that uses decision tress
to make predictions. The idea is to construct an ensemble of decision
trees by training each tree on a random subset of the data and a random
subset of the predictors. Each individual tree independently make
predictions, but the final prediction is determined by aggregating the
results.

``` r
# Parallel Processing
cores <- 10
cl <- makeCluster(cores)
registerDoParallel(cl)

rfFit <- train(shares ~ ., data = train_Data_model, method = "rf",
               trControl = trainControl(method = "cv", number = 5, repeats = 3),
               preProcess = c("center", "scale"),
               tuneGrid = data.frame(mtry = c(1:10)))

# Stop parallel processing
stopCluster(cl)
registerDoSEQ()

#review results:
rfFit$results
```

    ##    mtry     RMSE   Rsquared      MAE   RMSESD RsquaredSD    MAESD
    ## 1     1 4350.634 0.04408033 1747.125 1372.196 0.03120180 142.0015
    ## 2     2 4373.822 0.03968593 1779.722 1334.180 0.02591872 138.6762
    ## 3     3 4400.510 0.03649262 1807.274 1318.172 0.02351051 143.3394
    ## 4     4 4429.941 0.03418351 1827.099 1300.422 0.02283607 135.0833
    ## 5     5 4457.269 0.03166596 1843.992 1284.605 0.02206760 140.1893
    ## 6     6 4469.601 0.03136533 1860.128 1281.875 0.02450152 133.6230
    ## 7     7 4484.867 0.02880815 1865.207 1277.826 0.01868733 140.6752
    ## 8     8 4504.055 0.02899456 1878.669 1262.699 0.02196849 137.7297
    ## 9     9 4505.007 0.02796074 1878.686 1263.546 0.01819311 140.9286
    ## 10   10 4520.457 0.02780308 1887.687 1257.402 0.02092818 141.4134

``` r
rfFit$bestTune
```

    ##   mtry
    ## 1    1

Finally, we make predictions on test data using the trained Random
Forest model. The model fit diagnostics compares the predicted values
with the observed shares values from the test data. The RMSE provides an
evaluation of the model’s performance.

``` r
# Make predictions with random forest model
predsRf <- predict(rfFit, newdata = test_Data)
# Check model fit diagnostics
postResample(predsRf, obs = test_Data$shares)
```

    ##         RMSE     Rsquared          MAE 
    ## 8544.2597776    0.0261734 2038.9229187

### Comparison of Models (Yejun)
