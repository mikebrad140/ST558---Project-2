ST558: Project 2
================
Michael Bradshaw and Yejun Han
2023-07-05

- <a href="#channel-of-interest-business"
  id="toc-channel-of-interest-business">Channel of Interest: Business</a>
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

# Channel of Interest: Business

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

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ##      1.0    952.2   1400.0   3146.7   2500.0 690400.0

``` r
# Histogram of shares
ggplot(train_Data , aes(x = shares)) +
  geom_histogram(binwidth = 2500, fill = "blue") +
  labs(x = "Shares", y = "Frequency") +
  ggtitle("Distribution of Shares") +
  theme_classic()
```

![](Business_files/figure-gfm/Summarization1-1.png)<!-- -->

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
    ## 1 Sunday        237       860626          3631.     5562.        692      56900
    ## 2 Monday        813      3656536          4498.    33633.          1     690400
    ## 3 Tuesday       853      2541849          2980.    12086.         44     310800
    ## 4 Wednesday     884      2434089          2753.     8690.         63     158900
    ## 5 Thursday      842      2209977          2625.    11362.         81     306100
    ## 6 Friday        591      1289821          2182.     2322.         22      21500
    ## 7 Saturday      162       795968          4913.    12265.        516     144400

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
    ## 1 No            3046.
    ## 2 Yes           4152.

``` r
ggplot(average_shares, aes(x = day_of_week, y = average_shares, 
                           fill = day_of_week)) +
  geom_col() +
  labs(x = "Day of the Week", y = "Average Shares", fill = "Day of the Week") +
  ggtitle("Average Shares by Day of the Week") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](Business_files/figure-gfm/Summarization2-1.png)<!-- -->

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
    ## 1 Long            1036          4453.    24352.          1     690400       690399
    ## 2 Medium          1099          2342.     5611.         63     144400       144337
    ## 3 Short           1082          2108.     4263.         44      78600        78556
    ## 4 Very Short      1165          3709.    22397.         22     652900       652878

``` r
# Create the horizontal bar chart
ggplot(average_shares_byContent, aes(x = average_shares, y = content_length, fill = content_length)) +
  geom_bar(stat = "identity") +
  labs(x = "Average Shares", y = "Content Length Category", fill = "Content Length") +
  ggtitle("Average Shares by Content Length Category") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 0.5))
```

![](Business_files/figure-gfm/Summarization3-1.png)<!-- -->

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
    ## n_tokens_title                 0.0060752010
    ## n_tokens_content               0.0306023930
    ## n_unique_tokens                0.0038733749
    ## n_non_stop_words               0.0018274017
    ## n_non_stop_unique_tokens       0.0143448763
    ## num_hrefs                      0.0342477099
    ## num_self_hrefs                 0.0197741532
    ## num_imgs                       0.0152757471
    ## num_videos                     0.0500110597
    ## average_token_length          -0.0286475194
    ## num_keywords                   0.0177288368
    ## data_channel_is_lifestyle                NA
    ## data_channel_is_entertainment            NA
    ## data_channel_is_bus                      NA
    ## data_channel_is_socmed                   NA
    ## data_channel_is_tech                     NA
    ## data_channel_is_world                    NA
    ## kw_min_min                    -0.0012390328
    ## kw_max_min                     0.0399404574
    ## kw_avg_min                     0.0436035967
    ## kw_min_max                    -0.0009498878
    ## kw_max_max                     0.0069598024
    ## kw_avg_max                     0.0255781124
    ## kw_min_avg                     0.0485062938
    ## kw_max_avg                     0.0482062937
    ## kw_avg_avg                     0.0824052407
    ## self_reference_min_shares      0.1555022658
    ## self_reference_max_shares      0.0873117485
    ## self_reference_avg_sharess     0.1334625344
    ## weekday_is_monday              0.0380843296
    ## weekday_is_tuesday            -0.0048443561
    ## weekday_is_wednesday          -0.0116761810
    ## weekday_is_thursday           -0.0150386292
    ## weekday_is_friday             -0.0224891299
    ## weekday_is_saturday            0.0204463823
    ## weekday_is_sunday              0.0068450697
    ## is_weekend                     0.0187920436
    ## LDA_00                        -0.0048449995
    ## LDA_01                         0.0038999546
    ## LDA_02                        -0.0197943351
    ## LDA_03                         0.0633575753
    ## LDA_04                        -0.0187340864
    ## global_subjectivity            0.0434918645
    ## global_sentiment_polarity     -0.0101081841
    ## global_rate_positive_words     0.0116549704
    ## global_rate_negative_words     0.0324344167
    ## rate_positive_words           -0.0133245041
    ## rate_negative_words            0.0153133547
    ## avg_positive_polarity          0.0174264118
    ## min_positive_polarity         -0.0146206568
    ## max_positive_polarity          0.0205422078
    ## avg_negative_polarity         -0.0598643490
    ## min_negative_polarity         -0.0599143923
    ## max_negative_polarity         -0.0272109076
    ## title_subjectivity             0.0079589057
    ## title_sentiment_polarity       0.0132212945
    ## abs_title_subjectivity         0.0242431769
    ## abs_title_sentiment_polarity   0.0106802531
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
    ## 9.845878e+03 3.758342e-03 2.644262e+03

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

    ##    nvmax     RMSE    Rsquared      MAE   RMSESD  RsquaredSD    MAESD
    ## 1      1 15311.67 0.001414168 2895.662 8081.915 0.002131938 320.9400
    ## 2      2 15309.56 0.001782838 2891.854 8086.912 0.003383037 330.1716
    ## 3      3 15261.07 0.028094017 2897.700 8024.790 0.057107217 314.7994
    ## 4      4 15306.76 0.022129855 2918.560 7963.928 0.043530720 307.6556
    ## 5      5 15296.34 0.022715318 2896.860 7969.401 0.043207812 321.1857
    ## 6      6 15310.68 0.012750915 2915.637 7956.459 0.024292504 319.2062
    ## 7      7 15421.19 0.014561241 2911.621 7792.935 0.024585557 334.9123
    ## 8      8 15417.54 0.011929717 2914.635 7813.985 0.021075700 325.4002
    ## 9      9 15402.21 0.016297677 2915.921 7799.419 0.020223675 311.2152
    ## 10    10 15439.11 0.013997698 2956.065 7769.217 0.021674848 265.9767

``` r
step_model_seq$bestTune
```

    ##   nvmax
    ## 3     3

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
    ## 9.270947e+03 1.408241e-04 2.577798e+03

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
    ## 1     1 13231.89 0.02607792 2813.262 11678.21 0.02358189 553.2820
    ## 2     2 13445.33 0.03136562 2881.079 11392.47 0.02769197 539.0044
    ## 3     3 13576.32 0.03205686 2922.865 11255.75 0.02879067 528.7880
    ## 4     4 13754.90 0.03411179 2983.772 11095.38 0.03316588 508.7481
    ## 5     5 13924.02 0.03193976 3004.029 11054.93 0.02988508 528.3656
    ## 6     6 14043.97 0.03716902 3024.726 10922.49 0.03976895 509.1362
    ## 7     7 14302.08 0.03834744 3062.109 10818.76 0.04677020 531.0386
    ## 8     8 14340.90 0.03576483 3044.708 10761.36 0.03981379 529.6378
    ## 9     9 14624.41 0.03554661 3083.529 10590.57 0.03940745 524.8704
    ## 10   10 14866.60 0.03608726 3120.442 10498.98 0.04220540 518.1679

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
    ## 9.273001e+03 9.880154e-03 2.481311e+03

### Comparison of Models (Yejun)
