ST558: Project 2
================
Michael Bradshaw and Yejun Han
2023-07-07

- <a href="#channel-of-interest-tech"
  id="toc-channel-of-interest-tech">Channel of Interest: Tech</a>
  - <a href="#introduction-to-the-project"
    id="toc-introduction-to-the-project">Introduction to the Project</a>
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

## Introduction to the Project

The working data set is about the online news popularity, and nearly 60
kinds of variables are included, such as n_tokens_title,
n_unique_tokens, num_imgs, num_videos, average_token_length,
num_keywords, weekday_is\_, is_weekend, rate_positive_words,
max_negative_polarity, title_subjectivity, and shares. The work intends
to analyze the data and fitting model with the shares as target variable
under one of data_channels, and then apply to other data_channels. The
data was firstly imported and non-predictor variables such as url and
timedelta were removed. The data was then summarized, and the variables
of day_of_week, content_length, avg_positive_polarity, num_keywords, and
length of title were respectively analyzed through with statistics and
plots. In modeling, both linear regression model and ensemble tree-based
models were adopted. The four created models were comparatively
analyzed, and the optimum model was selected.

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
                        ifelse(n_tokens_content <= 750, "Medium", "Long"))),
         title_length = ifelse(n_tokens_title <= 8, "Short",
                               ifelse(n_tokens_title <= 12, "Medium",      
                               ifelse(n_tokens_title <= 15, "Long","Very Long"))),
         avg_positive_polarity_rate = ifelse(avg_positive_polarity <= 0.2, "Low",
                               ifelse(avg_positive_polarity <= 0.3, "Medium",      
                               ifelse(avg_positive_polarity <= 0.4, "High","Very High")))
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

To analyze the connection between avg_positive_polarity and
average_shares. The numeric data was conveted to categorical data
avg_positive_polarity_rate (“Low”, “Medium”,“High”,“Very High” ). It’s
interesting that the low avg_positive_polarity_rate displyed the highest
average_shares. The high and medium avg_positive_polarity are similar in
average_shares.

``` r
# Calculate the average shares by avg_positive_polarity_rate category
average_shares_byavg_positive_polarity_rate <- train_Data %>%
  group_by(avg_positive_polarity_rate) %>%
  summarise(n = n(),
            average_shares = mean(shares),
            sd_shares = sd(shares),
            min_shares = min(shares),
            max_shares = max(shares),
            range_shares = max_shares - min_shares)
average_shares_byavg_positive_polarity_rate
```

    ## # A tibble: 4 × 7
    ##   avg_positive_polarity_rate     n average_shares sd_shares min_shares max_shares
    ##   <chr>                      <int>          <dbl>     <dbl>      <int>      <int>
    ## 1 High                        2876          3354.    13339.         36     663600
    ## 2 Low                           97          2818.     3695.        511      22100
    ## 3 Medium                       875          2899.     5261.        116      96100
    ## 4 Very High                   1297          3011.     4908.        181      83300
    ## # ℹ 1 more variable: range_shares <int>

``` r
# Create the horizontal bar chart
ggplot(average_shares_byavg_positive_polarity_rate, aes(x = avg_positive_polarity_rate, y = average_shares, fill = avg_positive_polarity_rate)) +
  geom_bar(stat = "identity") +
  labs(x = "avg_positive_polarity_rate Category", y = "Average Shares", fill = "avg_positive_polarity_rate") +
  ggtitle("Average Shares by avg_positive_polarity_rate Category") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 0.6))
```

![](Tech_files/figure-gfm/Summarization4-1.png)<!-- -->

Here is the summary plot of average_share versus num_keywords. When the
number of keywords is less than 8, the average_shares increase with the
increase of keywords, except when there are 5 keywords.When there are
more than 8 keywords, the average_share will gradually decrease.It
suggests that a moderate number of keywords will increase
average_shares.

``` r
# Calculate the average shares by number of keywords
average_shares_bynum_keywords <- train_Data %>%
  group_by(num_keywords) %>%
  summarise(n = n(),
            average_shares = mean(shares),
            sd_shares = sd(shares),
            min_shares = min(shares),
            max_shares = max(shares),
            range_shares = max_shares - min_shares)
average_shares_bynum_keywords
```

    ## # A tibble: 9 × 7
    ##   num_keywords     n average_shares sd_shares min_shares max_shares range_shares
    ##          <dbl> <int>          <dbl>     <dbl>      <int>      <int>        <int>
    ## 1            2     2          2550      1202.       1700       3400         1700
    ## 2            3    21          2294.     2731.        119      10900        10781
    ## 3            4   117          3636.     5495.        498      47300        46802
    ## 4            5   401          3023.     4284.        413      52600        52187
    ## 5            6   714          2967.     3878.         64      33900        33836
    ## 6            7  1022          2799.     4199.        116      67800        67684
    ## 7            8   919          2961.     5140.         36      96100        96064
    ## 8            9   799          3266.     6448.        220      83300        83080
    ## 9           10  1150          3793.    20238.         82     663600       663518

``` r
# Create the horizontal bar chart
ggplot(average_shares_bynum_keywords, aes(x = num_keywords, y = average_shares, fill = num_keywords)) +
  geom_bar(stat = "identity") +
  labs(x = "num_keywords", y = "Average Shares", fill = "num_keywords") +
  ggtitle("Average Shares by number of keywords") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 0.5))
```

![](Tech_files/figure-gfm/Summarization5-1.png)<!-- -->

Besides the length of content, the effect of length of title on
average_shares was also analyzed. In the plot of average_shares versus
length of title. The title with medium length showed the highest
average_shares, while which is just slightly higher than that of Long
and short length of title. It is obvious that very long titles will
reduce average_shares.

``` r
# Calculate the average shares by title length category
average_shares_byTitle <- train_Data %>%
  group_by(title_length) %>%
  summarise(n = n(),
            average_shares = mean(shares),
            sd_shares = sd(shares),
            min_shares = min(shares),
            max_shares = max(shares),
            range_shares = max_shares - min_shares)
average_shares_byTitle
```

    ## # A tibble: 4 × 7
    ##   title_length     n average_shares sd_shares min_shares max_shares range_shares
    ##   <chr>        <int>          <dbl>     <dbl>      <int>      <int>        <int>
    ## 1 Long           673          3613.    25660.         36     663600       663564
    ## 2 Medium        3341          3045.     5310.         64     104100       104036
    ## 3 Short         1096          3272.     5085.         82      70200        70118
    ## 4 Very Long       35          4927.     9718.        591      53200        52609

``` r
# Create the horizontal bar chart
ggplot(average_shares_byTitle, aes(x = title_length, y = average_shares, fill = title_length)) +
  geom_bar(stat = "identity") +
  labs(x = "Title Length Category", y = "Average Shares", fill = "Title Length") +
  ggtitle("Average Shares by title Length Category") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 0.5))
```

![](Tech_files/figure-gfm/Summarization6-1.png)<!-- -->

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
num_cores <- detectCores()-1
cl <- makeCluster(num_cores)
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
    ## 6      6 8174.862 0.016468179 2538.769 7387.711 0.019826900 307.0571
    ## 7      7 8197.744 0.010674541 2545.619 7377.112 0.011330356 306.0953
    ## 8      8 8178.162 0.015974321 2539.716 7384.909 0.019101938 309.0418
    ## 9      9 8174.501 0.017866624 2536.154 7388.675 0.022573096 312.1816
    ## 10    10 8178.427 0.018971026 2537.887 7386.724 0.024017454 310.5219

``` r
step_model_seq$bestTune
```

    ##   nvmax
    ## 1     1

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
    ## 3.703922e+03 1.071103e-02 2.146951e+03

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
num_cores <- detectCores()-1
cl <- makeCluster(num_cores)
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

### Boosted tree model:

Boosted tree model, also known as gradient boosting, combines multiple
weak prediction models to create a stronger and more accurate model.
Each tree is dependent on prior trees, i.e. fitting the residual of the
trees that preceded it, and improve the final accuracy.

``` r
# Boosted: Define the control parameters for cross-validation  
ctrl <- trainControl(method = "cv", number = 5)

# Define the tuning parameters:
boosted_grid <- expand.grid(n.trees = c(25, 50, 100, 150, 200),
                            interaction.depth = c(1, 2, 3, 4),
                            shrinkage = 0.1,
                            n.minobsinnode = 10)

# Specify the boosted tree model using the gbm package

# Set the number of CPU cores to use
num_cores <- detectCores()-1
# Set up parallel processing
cl <- makeCluster(num_cores)
registerDoParallel(cl)
boostFit <- train(shares ~ ., data = lifestyle_train_Data_model,
                       method = "gbm",
                       trControl = trainControl(method = "repeatedcv", 
                                                number = 5, repeats = 3),
                       tuneGrid = boosted_grid,
                       #suppress output
                       verbose = FALSE )
# Stop parallel processing
stopCluster(cl)
registerDoSEQ()
```

Then, the boosted tree model was evaluated, the performance across
test_Data was calculates.

``` r
# Make predictions with Boosted tree model
predsBf <- predict(boostFit, newdata = test_Data)

# Check model fit diagnostics
postResample(predsBf, obs = test_Data$shares)
```

    ##         RMSE     Rsquared          MAE 
    ## 3.815163e+03 2.178882e-03 2.172907e+03

### Comparison of Models

``` r
# Function to determine the best model
find_best <- function(lm1, lm2, rf, boost){
  # Put all the fit results in a data frame
  results <- data.frame(rbind("Linear Model 1"= postResample(lm1, test_Data$shares),
                                  "Linear Model 2"= postResample(lm2, test_Data$shares),
                                  "Random Forest"= postResample(rf, test_Data$shares),
                                  "Boosted Tree" = postResample(boost, test_Data$shares)))

  # Determine the name of the model with the lowest RMSE
  model_winner <- row.names(results)[results$RMSE == min(results$RMSE)]
  # Return both the data frame of results, as well as the row name of best model name in a list
  return(list(results, model_winner))
}

# search through to find the best model
best_model <- find_best(predslinear1, predslinear2, predsRf, predsBf)
# Print out the data frame of RMSE, Rsquared, and MAE
best_model[[1]]
```

    ##                    RMSE    Rsquared      MAE
    ## Linear Model 1 3754.688 0.010539230 2164.777
    ## Linear Model 2 3703.922 0.010711027 2146.951
    ## Random Forest  3658.203 0.021516802 2108.141
    ## Boosted Tree   3815.163 0.002178882 2172.907

``` r
# Print out a message that tells us which model is the best based on lowest RMSE
print(paste("The best model by finding the RMSE on the test data is the", best_model[[2]], "model.")) 
```

    ## [1] "The best model by finding the RMSE on the test data is the Random Forest model."
