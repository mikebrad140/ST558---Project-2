ST558: Project 2
================
Michael Bradshaw and Yejun Han
2023-07-05

- <a href="#channel-of-interest-tech"
  id="toc-channel-of-interest-tech">Channel of Interest: Tech</a>
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

# Channel of Interest: Tech

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
    ##      36    1100    1700    3180    3000  663600

``` r
# Histogram of shares
ggplot(train_Data , aes(x = shares)) +
  geom_histogram(binwidth = 2500, fill = "blue") +
  labs(x = "Shares", y = "Frequency") +
  ggtitle("Distribution of Shares") +
  theme_classic()
```

![](Tech_files/figure-gfm/Summarization1-1.png)<!-- -->

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
    ## 1 Sunday        266      1065070          4004.     6230.        206      83300
    ## 2 Monday        859      2490531          2899.     4217.        205      51000
    ## 3 Tuesday      1028      3027706          2945.     5243.        162      88500
    ## 4 Wednesday     991      3644334          3677.    21563.         36     663600
    ## 5 Thursday      920      2528833          2749.     4241.         86      55200
    ## 6 Friday        704      2202936          3129.     5724.         82     104100
    ## 7 Saturday      377      1402969          3721.     6097.        119      96100

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
    ## 1 No            3086.
    ## 2 Yes           3838.

``` r
ggplot(average_shares, aes(x = day_of_week, y = average_shares, 
                           fill = day_of_week)) +
  geom_col() +
  labs(x = "Day of the Week", y = "Average Shares", fill = "Day of the Week") +
  ggtitle("Average Shares by Day of the Week") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](Tech_files/figure-gfm/Summarization2-1.png)<!-- -->

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
    ## 1 Long            1227          4064.    19801.        213     663600       663387
    ## 2 Medium          1321          3194.     5171.         36      71800        71764
    ## 3 Short           1372          2787.     4543.         86      83300        83214
    ## 4 Very Short      1225          2720.     4312.         82      96100        96018

``` r
# Create the horizontal bar chart
ggplot(average_shares_byContent, aes(x = average_shares, y = content_length, fill = content_length)) +
  geom_bar(stat = "identity") +
  labs(x = "Average Shares", y = "Content Length Category", fill = "Content Length") +
  ggtitle("Average Shares by Content Length Category") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 0.5))
```

![](Tech_files/figure-gfm/Summarization3-1.png)<!-- -->

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
    ## n_tokens_title                -0.0019483094
    ## n_tokens_content               0.0796168041
    ## n_unique_tokens               -0.0553115696
    ## n_non_stop_words              -0.0025427026
    ## n_non_stop_unique_tokens      -0.0445049393
    ## num_hrefs                      0.0836824834
    ## num_self_hrefs                 0.0016261599
    ## num_imgs                       0.0065593810
    ## num_videos                     0.0374201177
    ## average_token_length           0.0028795256
    ## num_keywords                   0.0242797231
    ## data_channel_is_lifestyle                NA
    ## data_channel_is_entertainment            NA
    ## data_channel_is_bus                      NA
    ## data_channel_is_socmed                   NA
    ## data_channel_is_tech                     NA
    ## data_channel_is_world                    NA
    ## kw_min_min                     0.0103700481
    ## kw_max_min                    -0.0009039664
    ## kw_avg_min                    -0.0024813292
    ## kw_min_max                    -0.0047150442
    ## kw_max_max                     0.0039028270
    ## kw_avg_max                    -0.0167294790
    ## kw_min_avg                     0.0245171939
    ## kw_max_avg                     0.0199228999
    ## kw_avg_avg                     0.0457548517
    ## self_reference_min_shares      0.0087672337
    ## self_reference_max_shares      0.0114311395
    ## self_reference_avg_sharess     0.0136950294
    ## weekday_is_monday             -0.0119639303
    ## weekday_is_tuesday            -0.0111718682
    ## weekday_is_wednesday           0.0231022285
    ## weekday_is_thursday           -0.0191563685
    ## weekday_is_friday             -0.0019347089
    ## weekday_is_saturday            0.0144763324
    ## weekday_is_sunday              0.0182986107
    ## is_weekend                     0.0236597216
    ## LDA_00                         0.0290964496
    ## LDA_01                        -0.0091917490
    ## LDA_02                        -0.0019266498
    ## LDA_03                         0.0071467655
    ## LDA_04                        -0.0139093914
    ## global_subjectivity            0.0061240102
    ## global_sentiment_polarity     -0.0293489446
    ## global_rate_positive_words    -0.0202638311
    ## global_rate_negative_words     0.0283254209
    ## rate_positive_words           -0.0354277712
    ## rate_negative_words            0.0358081548
    ## avg_positive_polarity          0.0013068142
    ## min_positive_polarity         -0.0330459299
    ## max_positive_polarity          0.0229406841
    ## avg_negative_polarity         -0.0308999002
    ## min_negative_polarity         -0.0328560519
    ## max_negative_polarity          0.0257172595
    ## title_subjectivity             0.0145448993
    ## title_sentiment_polarity       0.0191576373
    ## abs_title_subjectivity        -0.0182427743
    ## abs_title_sentiment_polarity   0.0182031957
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
    ## 3.754688e+03 1.053923e-02 2.164777e+03

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
    ## 1      1 8169.890 0.007129508 2538.734 7401.237 0.006140015 309.8758
    ## 2      2 8182.464 0.006309482 2528.164 7402.505 0.008869467 327.7189
    ## 3      3 8170.342 0.014365473 2528.316 7404.460 0.018165590 322.4351
    ## 4      4 8169.996 0.015394160 2536.017 7393.713 0.019102363 316.4568
    ## 5      5 8171.131 0.016178354 2534.185 7390.883 0.018961545 312.6849
    ## 6      6 8174.516 0.016508808 2538.618 7387.986 0.019850733 307.2522
    ## 7      7 8197.339 0.010721897 2546.099 7377.386 0.011412266 305.9237
    ## 8      8 8172.232 0.016060950 2536.047 7389.307 0.019076616 309.6595
    ## 9      9 8168.158 0.018230399 2529.937 7393.211 0.022908800 312.1042
    ## 10    10 8170.752 0.019502956 2529.448 7389.370 0.024299710 305.0990

``` r
step_model_seq$bestTune
```

    ##   nvmax
    ## 9     9

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
    ## 3.736170e+03 9.801106e-03 2.160316e+03

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
    ## 1     1 8162.742 0.01469515 2485.783 7372.934 0.01237298 400.7080
    ## 2     2 8208.790 0.02136526 2525.663 7339.672 0.02059843 391.7706
    ## 3     3 8313.921 0.01772321 2575.753 7281.794 0.01527032 377.5665
    ## 4     4 8361.702 0.01773710 2601.310 7253.364 0.01482022 381.5042
    ## 5     5 8454.617 0.01820863 2627.375 7221.973 0.02070412 374.0209
    ## 6     6 8488.221 0.01576692 2639.781 7200.506 0.01553614 380.6203
    ## 7     7 8584.464 0.01456641 2659.565 7153.278 0.01360775 376.3773
    ## 8     8 8617.547 0.01406495 2667.544 7142.966 0.01445157 383.9455
    ## 9     9 8609.862 0.01333426 2663.063 7144.294 0.01240302 385.6925
    ## 10   10 8816.074 0.01219042 2686.163 7061.762 0.01279736 381.5836

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
    ## 3658.2028801    0.0215168 2108.1412528

### Comparison of Models (Yejun)
