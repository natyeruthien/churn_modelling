Churn_Mondelling
================
Elena PatsalouNatalia KoraiWaqar Aziz Sulaiman
2023-05-05

``` r
#install.packages('caTools')
#install.packages("klaR")
#install.packages('caret')
#install.packages('ROSE')
#install.packages('naivebayes')
```

``` r
# Libraries
library(e1071)
```

    ## Warning: package 'e1071' was built under R version 4.2.3

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.0     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.1     ✔ tibble    3.1.8
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the ]8;;http://conflicted.r-lib.org/conflicted package]8;; to force all conflicts to become errors

``` r
library(corrplot)
```

    ## corrplot 0.92 loaded

``` r
library(dplyr)
library(ROSE)
```

    ## Warning: package 'ROSE' was built under R version 4.2.3

    ## Loaded ROSE 0.0-4

``` r
library(caret)
```

    ## Warning: package 'caret' was built under R version 4.2.3

    ## Loading required package: lattice
    ## 
    ## Attaching package: 'caret'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
library(boot)
```

    ## 
    ## Attaching package: 'boot'
    ## 
    ## The following object is masked from 'package:lattice':
    ## 
    ##     melanoma

``` r
library(glmnet)
```

    ## Warning: package 'glmnet' was built under R version 4.2.3

    ## Loading required package: Matrix
    ## 
    ## Attaching package: 'Matrix'
    ## 
    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack
    ## 
    ## Loaded glmnet 4.1-7

``` r
library(naivebayes)
```

    ## Warning: package 'naivebayes' was built under R version 4.2.3

    ## naivebayes 0.9.7 loaded

``` r
library(caTools)
```

    ## Warning: package 'caTools' was built under R version 4.2.3

``` r
library(MASS)
```

    ## 
    ## Attaching package: 'MASS'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

``` r
library(pROC)
```

    ## Warning: package 'pROC' was built under R version 4.2.3

    ## Type 'citation("pROC")' for a citation.
    ## 
    ## Attaching package: 'pROC'
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     cov, smooth, var

``` r
library(class)
library(partition)
```

    ## Warning: package 'partition' was built under R version 4.2.3

    ## 
    ## Attaching package: 'partition'
    ## 
    ## The following object is masked from 'package:boot':
    ## 
    ##     corr

``` r
library(klaR)
```

    ## Warning: package 'klaR' was built under R version 4.2.3

## 1. Read the Data

``` r
df <- read.csv("C:/Users/ntbna/Downloads/Spring Semester 2023/DSC532/Classification PROJECT/Churn_Modelling.csv")
# 10000 observations and 14 columns
head(df)
```

    ##   RowNumber CustomerId  Surname CreditScore Geography Gender Age Tenure
    ## 1         1   15634602 Hargrave         619    France Female  42      2
    ## 2         2   15647311     Hill         608     Spain Female  41      1
    ## 3         3   15619304     Onio         502    France Female  42      8
    ## 4         4   15701354     Boni         699    France Female  39      1
    ## 5         5   15737888 Mitchell         850     Spain Female  43      2
    ## 6         6   15574012      Chu         645     Spain   Male  44      8
    ##     Balance NumOfProducts HasCrCard IsActiveMember EstimatedSalary Exited
    ## 1      0.00             1         1              1       101348.88      1
    ## 2  83807.86             1         0              1       112542.58      0
    ## 3 159660.80             3         1              0       113931.57      1
    ## 4      0.00             2         0              0        93826.63      0
    ## 5 125510.82             1         1              1        79084.10      0
    ## 6 113755.78             2         1              0       149756.71      1

``` r
# Preprocessing
cat('Duplicates:', nrow(df[duplicated(df), ]),'\n') # check for duplicates rows
```

    ## Duplicates: 0

``` r
cat('Duplilcates in the column CustomerId:', sum(duplicated(df$CustomerId)), '\n') # check for duplicates for ID
```

    ## Duplilcates in the column CustomerId: 0

``` r
cat('Missing Values for each column\n')
```

    ## Missing Values for each column

``` r
sapply(df, function(x) sum(is.na(x))) # check for any missing values
```

    ##       RowNumber      CustomerId         Surname     CreditScore       Geography 
    ##               0               0               0               0               0 
    ##          Gender             Age          Tenure         Balance   NumOfProducts 
    ##               0               0               0               0               0 
    ##       HasCrCard  IsActiveMember EstimatedSalary          Exited 
    ##               0               0               0               0

``` r
str(df) # check the type of each column
```

    ## 'data.frame':    10000 obs. of  14 variables:
    ##  $ RowNumber      : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ CustomerId     : int  15634602 15647311 15619304 15701354 15737888 15574012 15592531 15656148 15792365 15592389 ...
    ##  $ Surname        : chr  "Hargrave" "Hill" "Onio" "Boni" ...
    ##  $ CreditScore    : int  619 608 502 699 850 645 822 376 501 684 ...
    ##  $ Geography      : chr  "France" "Spain" "France" "France" ...
    ##  $ Gender         : chr  "Female" "Female" "Female" "Female" ...
    ##  $ Age            : int  42 41 42 39 43 44 50 29 44 27 ...
    ##  $ Tenure         : int  2 1 8 1 2 8 7 4 4 2 ...
    ##  $ Balance        : num  0 83808 159661 0 125511 ...
    ##  $ NumOfProducts  : int  1 1 3 2 1 2 2 4 2 1 ...
    ##  $ HasCrCard      : int  1 0 1 0 1 1 1 1 0 1 ...
    ##  $ IsActiveMember : int  1 1 0 0 1 0 1 0 1 1 ...
    ##  $ EstimatedSalary: num  101349 112543 113932 93827 79084 ...
    ##  $ Exited         : int  1 0 1 0 0 1 0 1 0 0 ...

## 2.1. Pre-processing

## 2.1.1 Unique Values

``` r
# number of unique values for each column
number_uniques = c()
for (i in 1:14){
  number_uniques[i] = length(unique(df[,i])) #how many unique values each column has
}
number_uniques = as.data.frame(number_uniques)
number_uniques$col = colnames(df)
number_uniques$index = 1:14
number_uniques
```

    ##    number_uniques             col index
    ## 1           10000       RowNumber     1
    ## 2           10000      CustomerId     2
    ## 3            2932         Surname     3
    ## 4             460     CreditScore     4
    ## 5               3       Geography     5
    ## 6               2          Gender     6
    ## 7              70             Age     7
    ## 8              11          Tenure     8
    ## 9            6382         Balance     9
    ## 10              4   NumOfProducts    10
    ## 11              2       HasCrCard    11
    ## 12              2  IsActiveMember    12
    ## 13           9999 EstimatedSalary    13
    ## 14              2          Exited    14

## 2.1.2 Drop Columns RowNumber, CustomerID, and Surname

``` r
df <- df[-c(1,2,3)]
names(df)
```

    ##  [1] "CreditScore"     "Geography"       "Gender"          "Age"            
    ##  [5] "Tenure"          "Balance"         "NumOfProducts"   "HasCrCard"      
    ##  [9] "IsActiveMember"  "EstimatedSalary" "Exited"

The only columns which are categorical are Geography and Gender. They
will be transformed into dummy variables, since they contain 3 and 2
unique values, respectively

## 2.1.3 Outliers

``` r
# check for outliers for numerical variables, excluding the sets of {0,1}
find_outliers <- function(x){
  H=1.5 * IQR(x)
  number <- sum(x< (quantile(x)[2]-H)) + sum(x> (quantile(x)[4]+H))
  number
}
df_continuous <- df[,c(1, 4, 6, 10)]
outliers=c()
for (i in 1:4){
  outliers[i]=find_outliers(df_continuous[1:10000,i])
}
# percentage of outliers for each of the above columns
outliers_tois100=as.data.frame(round(((outliers/10000)*100),2)) # Percentage of outliers for each feature
outliers_tois100$col=colnames(df_continuous) # Add the name of each column
outliers_tois100
```

    ##   round(((outliers/10000) * 100), 2)             col
    ## 1                               0.15     CreditScore
    ## 2                               3.59             Age
    ## 3                               0.00         Balance
    ## 4                               0.00 EstimatedSalary

Although we have less than 4% of outliers in column Age we cannot be
sure if they are negligible. We can drop them later on.

``` r
# check for the Age if the values make sense, since there are 70 unique ones; 18 years as the minimun and 92 year as the
# maximum are logic
summary(df['Age'])
```

    ##       Age       
    ##  Min.   :18.00  
    ##  1st Qu.:32.00  
    ##  Median :37.00  
    ##  Mean   :38.92  
    ##  3rd Qu.:44.00  
    ##  Max.   :92.00

## 2.1.4 Exploratory Analysis

``` r
# continuous variables
for (i in 1:4){
  boxplot(df_continuous[,i], ylab = names(df_continuous)[i])
}
```

![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-8-4.png)<!-- -->

``` r
# zoom in for the CreditScore to see how far the outliers are
boxplot(df_continuous$CreditScore, ylim=range(200:500),ylab = 'Credit Score')
```

![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-8-5.png)<!-- -->

We verify that CreditScore and Age has some outliers; specifically the
CreditScore has its outliers lower than the lower whisker, whereas the
Age has greater values than the upper whisker. Also, the values of
CreditScore are not quite much far from the lower whisker, hence we
decide not to drop them. Similarly, the outliers of the Age are not very
extreme since they are located close to the whisker.

``` r
# discrete variables, but not the {0,1}s
library(ggplot2)
ggplot(df, aes(x=Geography)) +
  geom_bar(fill = 'lightblue')
```

![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
ggplot(df, aes(x=NumOfProducts)) +
  geom_bar(fill = 'lightblue')
```

![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

``` r
par(mfrow=c(1,2))


# Histogram
hist(df$Age, prob = TRUE, col = "white",
    main = "Histogram of Age", xlab = 'Age', ylab = 'Density')
lines(density(df$Age), col = 4, lwd = 2)
hist(df$CreditScore, prob = TRUE, col = "white",
    main = "Histogram of CreditScore", xlab = 'CreditScore', ylab = 'Density')
lines(density(df$CreditScore), col = 4, lwd = 2)
```

![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-9-3.png)<!-- -->

``` r
# table of the {0,1} variables
table(df$Gender)
```

    ## 
    ## Female   Male 
    ##   4543   5457

``` r
table(df$HasCrCard)
```

    ## 
    ##    0    1 
    ## 2945 7055

``` r
table(df$IsActiveMember)
```

    ## 
    ##    0    1 
    ## 4849 5151

``` r
table(df$Exited)
```

    ## 
    ##    0    1 
    ## 7963 2037

``` r
# check skewness for continuous variables that seem to be skewed
skewness(df$Age)
```

    ## [1] 1.011017

``` r
skewness(df$CreditScore)
```

    ## [1] -0.07158513

``` r
# males = 55 %, females = 45 %
# customers with credit card = 70 %
# active customers = 51 %
# exited customers = 20 %
```

- Approximately 2500 customers are from Germany, 2500 from Spain, while
  the rest 5000 customers are from France.
- The most of customers utilize either 1 or 2 products of the bank (1
  product is more common); just over 250 people have 3 products and 60
  individuals have 4.
- The 55% of customers of the bank are males and the rest 45% are
  females.
- The most of customers (70%) hold a credit card of the bank, whereas
  51% are active members.
- 20% of the customers decided to churn.
- The most of customers of the Bank are approximately from 25 to 50
  years old, and few people are older than 60 years, whereas even fewer
  customers are younger than 25; the distributio of Age is slightly
  right skewed.

## 2.1.4.1 Comparison between Continuous and Categorical Variables

Credit Score vs \[Geography, Gender, NumOfProducts, IsActiveMember,
HasCrCard, Exited, Tenure\]

``` r
glimpse(df)
```

    ## Rows: 10,000
    ## Columns: 11
    ## $ CreditScore     <int> 619, 608, 502, 699, 850, 645, 822, 376, 501, 684, 528,…
    ## $ Geography       <chr> "France", "Spain", "France", "France", "Spain", "Spain…
    ## $ Gender          <chr> "Female", "Female", "Female", "Female", "Female", "Mal…
    ## $ Age             <int> 42, 41, 42, 39, 43, 44, 50, 29, 44, 27, 31, 24, 34, 25…
    ## $ Tenure          <int> 2, 1, 8, 1, 2, 8, 7, 4, 4, 2, 6, 3, 10, 5, 7, 3, 1, 9,…
    ## $ Balance         <dbl> 0.00, 83807.86, 159660.80, 0.00, 125510.82, 113755.78,…
    ## $ NumOfProducts   <int> 1, 1, 3, 2, 1, 2, 2, 4, 2, 1, 2, 2, 2, 2, 2, 2, 1, 2, …
    ## $ HasCrCard       <int> 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, …
    ## $ IsActiveMember  <int> 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1, …
    ## $ EstimatedSalary <dbl> 101348.88, 112542.58, 113931.57, 93826.63, 79084.10, 1…
    ## $ Exited          <int> 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, …

We can see that there are many discrete variables that we want to
explore integer as their type. That could result in creating one big
boxplot for each category, and not multiple. We must change them into
characters so the program recognise them as binary values.

``` r
# Convert discrete variables from integers to characters in order to create boxplots
df$NumOfProducts = as.character(df$NumOfProducts)
df$IsActiveMember = as.character(df$IsActiveMember)
df$HasCrCard = as.character(df$HasCrCard)
df$Exited = as.character(df$Exited)
df$Tenure = as.character(df$Tenure)
```

``` r
# Create two for loops, that generate boxplots for all categorical variables
# The first for loop is without hue
# The second for loop is with hue
# Take into consideration that boxplots within the big boxplots are ordered from smaller to larger medians, to draw into conclusions more easily

# Create list of variables to iterate over
my_list <- c("Geography", "Gender", "NumOfProducts", "IsActiveMember", "HasCrCard", "Exited", "Tenure")

# First for loop
draw_boxplots <- function(df, y_var, my_list) {
  for (var in my_list) {
    var_ordered <- reorder(df[[var]], df[[y_var]], median)
    df[[var]] <- factor(df[[var]], levels = levels(var_ordered))
    p <- ggplot(df, aes(x = !!sym(var), y = !!sym(y_var))) + geom_boxplot() +
      ggtitle(paste("Plot of", y_var, "by", var))
    print(p)
  }
}

# Second for loop
draw_boxplots1 <- function(df, y_var, my_list) {
  for (var in my_list) {
    var_ordered <- reorder(df[[var]], df[[y_var]], median)
    df[[var]] <- factor(df[[var]], levels = levels(var_ordered))
    p <- ggplot(df, aes(x = !!sym(var), y = !!sym(y_var), fill = factor(Exited))) + geom_boxplot() +
      ggtitle(paste("Plot of", y_var, "by", var))
    print(p)
  }
}
```

*without using ‘Exited’ as a fill*

``` r
draw_boxplots(df, "CreditScore", my_list)
```

![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-13-3.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-13-4.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-13-5.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-13-6.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-13-7.png)<!-- -->

*Using ‘Exited’ as a fill*

``` r
draw_boxplots1(df, "CreditScore", my_list)
```

![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-14-3.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-14-4.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-14-5.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-14-6.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-14-7.png)<!-- -->

EstimatedSalary vs \[Geography, Gender, NumOfProducts, IsActiveMember,
HasCrCard, Exited, Tenure\]

*without using ‘Exited’ as a fill*

``` r
draw_boxplots(df, "EstimatedSalary", my_list)
```

![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-15-3.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-15-4.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-15-5.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-15-6.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-15-7.png)<!-- -->

*Using ‘Exited’ as a fill*

``` r
draw_boxplots1(df, "EstimatedSalary", my_list)
```

![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-16-3.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-16-4.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-16-5.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-16-6.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-16-7.png)<!-- -->

Balance vs \[Geography, Gender, NumOfProducts, IsActiveMember,
HasCrCard, Exited, Tenure\]

*without using ‘Exited’ as a fill*

``` r
draw_boxplots(df, "Balance", my_list)
```

![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-17-2.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-17-3.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-17-4.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-17-5.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-17-6.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-17-7.png)<!-- -->

*using ‘Exited’ as a fill*

``` r
draw_boxplots1(df, "Balance", my_list)
```

![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-18-2.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-18-3.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-18-4.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-18-5.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-18-6.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-18-7.png)<!-- -->

## 2.1.4.2 Comparison between Two Continuous Variables

``` r
qplot(CreditScore, Balance,  data = df, color = Gender)
```

    ## Warning: `qplot()` was deprecated in ggplot2 3.4.0.

![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
qplot(CreditScore, Balance,  data = df, color = Geography)
```

![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-19-2.png)<!-- -->

``` r
qplot(CreditScore, Balance,  data = df, color = Exited)
```

![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-19-3.png)<!-- -->

Observing the plots of Balance and CreditScore we cannot conclude to a
clear statement based on the Geography, Gender and the decision to exit
from the bank.

``` r
qplot(EstimatedSalary, Balance,  data = df, color = HasCrCard)
```

![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
qplot(EstimatedSalary, Balance,  data = df, color = IsActiveMember)
```

![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-20-2.png)<!-- -->

``` r
qplot(EstimatedSalary, Balance,  data = df, color = NumOfProducts)
```

![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-20-3.png)<!-- -->

Similarly. observing the above plots of Balance and EstimatedSalary we
cannot conclude to a pattern based on the possesion of credit card,
whether the customer is an active member, and the number of products
they utilise.

## Create dummy variable

This is needed for the following plots

``` r
# The features of Gender and Geography could be tranformed into dummy variables.
df$France <- ifelse(df$Geography == 'France', 1, 0)
df$Germany <- ifelse(df$Geography == 'Germany', 1, 0)
df$Female <- ifelse(df$Gender == 'Female', 1, 0 )
```

``` r
df <- df[,-c(2,3)]
names(df)
```

    ##  [1] "CreditScore"     "Age"             "Tenure"          "Balance"        
    ##  [5] "NumOfProducts"   "HasCrCard"       "IsActiveMember"  "EstimatedSalary"
    ##  [9] "Exited"          "France"          "Germany"         "Female"

## 2.1.4.3 Comparison between Two Categorical Variables

``` r
# visualise the tables (counts of each variable), to understand in a better way than tables
new <- df[,c(5,6,7,9,10,11,12)]


for (i in 1:7){
  for (j in 1:7){
    if ((i != j) & (i < j)){ # in order to have neither pairs (i,i) nor repetitions
      counts <- table(new[,i], new[,j])
      mosaicplot(counts, xlab=names(new)[i], ylab=names(new)[j],
            col=c('lightblue','pink'))
    }
  }
}
```

![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-23-2.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-23-3.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-23-4.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-23-5.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-23-6.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-23-7.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-23-8.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-23-9.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-23-10.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-23-11.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-23-12.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-23-13.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-23-14.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-23-15.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-23-16.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-23-17.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-23-18.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-23-19.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-23-20.png)<!-- -->![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-23-21.png)<!-- -->

- NumOfProducts vs
  - HasCreditCard: The most of customers have credit card and the
    possession is not affected from the number of products which are
    held by them.
  - IsActiveMember: Active people with 2 items of the bank are more than
    those who are not active. Moreover, there are less active customers
    who utilise 3 products than those are not active.
  - Exited: The most of people who hold 3 items decided to exit the bank
    whereas people who have either 1 or 2 did not exit. Also, all people
    who possess 4 products have churned.
  - France: Half of the customers who have 1, or 2, or 4 products are
    from France whereas the highest percentage of those who utilise 3
    items are not French.
  - Germany: In contrary, customers from German constitutes lower
    percentage on possessing of bank’s products. (French are the 50%,
    while Spanish and Germans are both 25%.)
  - Female: More women utilise either 3 or 4 products in comparison with
    men.
- HasCrCard vs:
  - IsActiveMember: Most of customers have credit card. We have
    approximately same number of active and not active membes.
  - Exited: The same percentage of customers based on the possession of
    credit card decided not to churn.
- IsActiveMember vs:
  - Exited: The most of customers who decided to churn were not active
    members (approximately 2/3).
  - Female: The most of the active members are males.
- Exited vs:
  - France: Although 50% of customers are from France the most of exited
    ones are from either Germany or Spain.
  - Female: The most of exited customers are women.
- France vs:
  - Female: The most French customers are males. (Overall, the men
    customers are around 1000 more than women ones)
- Germany vs:
  - Female: The most German customers are males.

## 2.1.4.4 Correlation between variables

``` r
# Convert discrete variables from characters to integers
df$NumOfProducts = as.integer(df$NumOfProducts)
df$IsActiveMember = as.integer(df$IsActiveMember)
df$HasCrCard = as.integer(df$HasCrCard)
df$Exited = as.integer(df$Exited)
df$Tenure = as.integer(df$Tenure)
corrplot(cor(df))
```

![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

- Germany is (positively) correlated with Balance (and negatively with
  France, due their definition).  
- Balance is correlated with France (negatively), Germany and
  NumOfProducts (negatively)(Elena: I don’t see any correlation between
  Germany and NumOfProducts). (Natalia: I mean the 1st one (Balance) is
  correlated with these 3 variables)
- The target variable Exited is correlated with Age, Balance,
  IsActiveMember(negatively), France(negatively), Germany and Female.
- IsActiveMember is slightely (positively) correlated with Age, which
  means as the age of a client increases, the higher the possibility is
  for him to be active with the Bank (Maybe because older people are
  more interested in saving their money, and more responsible. Also get
  higher salaries and have the opportunity to be involved with the Bank,
  buy products or insert money to their account)

``` r
pairs(df)
```

![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

``` r
# exclude the binary features (not Exited)
pairs(df[,c(1,2,3,4,5,9)])
```

![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

We cannot conlude to a pattern through the above pair plots, neither to
a statement where we had to transform any predictor into another form,
such as log or polynomial of order greater than 1.

## 2.1.5 Balancing the Data

A new dataset will be created, that fixes the inbalance between the two
classes

## 2.1.5.1 Oversampling

``` r
oversampled_data <- ovun.sample(Exited ~ ., data = df, method = "over")
# Extract the oversampled data
oversampled_churn_data <- oversampled_data$data

# Check the distribution of classes in the oversampled data
table(oversampled_churn_data$Exited)
```

    ## 
    ##    0    1 
    ## 7963 8006

``` r
#summary(oversampled_churn_data)
```

## 2.1.6 Standardized the Data

We created two different scaled dataframes of the imbalanced dataset,
each one for different purposes. All models will use one of those, which
fit them the best. We also scale the oversampled dataset.

``` r
head(df)
```

    ##   CreditScore Age Tenure   Balance NumOfProducts HasCrCard IsActiveMember
    ## 1         619  42      2      0.00             1         1              1
    ## 2         608  41      1  83807.86             1         0              1
    ## 3         502  42      8 159660.80             3         1              0
    ## 4         699  39      1      0.00             2         0              0
    ## 5         850  43      2 125510.82             1         1              1
    ## 6         645  44      8 113755.78             2         1              0
    ##   EstimatedSalary Exited France Germany Female
    ## 1       101348.88      1      1       0      1
    ## 2       112542.58      0      0       0      1
    ## 3       113931.57      1      1       0      1
    ## 4        93826.63      0      1       0      1
    ## 5        79084.10      0      0       0      1
    ## 6       149756.71      1      0       0      0

The range of the columns varies too much hence we have to standardize
all columns, in order for the machine learning algoriths to perform
better.

First DataFrame - imbalanced

``` r
df_scaled <-  df %>% mutate_at (c('CreditScore', 'Age', 'Tenure', 'Balance', 'NumOfProducts', 'EstimatedSalary'), ~(scale(.) %>% as.vector))
head(df_scaled)
```

    ##   CreditScore         Age    Tenure    Balance NumOfProducts HasCrCard
    ## 1 -0.32620511 0.293502747 -1.041708 -1.2257864    -0.9115379         1
    ## 2 -0.44001395 0.198153924 -1.387468  0.1173442    -0.9115379         0
    ## 3 -1.53671734 0.293502747  1.032856  1.3329867     2.5269303         1
    ## 4  0.50149556 0.007456278 -1.387468 -1.2257864     0.8076962         0
    ## 5  2.06378057 0.388851570 -1.041708  0.7856886    -0.9115379         1
    ## 6 -0.05720239 0.484200392  1.032856  0.5972987     0.8076962         1
    ##   IsActiveMember EstimatedSalary Exited France Germany Female
    ## 1              1       0.0218854      1      1       0      1
    ## 2              1       0.2165229      0      0       0      1
    ## 3              0       0.2406749      1      1       0      1
    ## 4              0      -0.1089125      0      1       0      1
    ## 5              1      -0.3652575      0      0       0      1
    ## 6              0       0.8636071      1      0       0      0

Second Dataframe - imbalanced

``` r
df_scaled_1 <- df

# 'Age' variable is right skewed, therefore it may be better to use a scaling method that is less affected by outliers, such as Robust scaling
df_scaled_1$Age <- scale(df$Age, median(df$Age), scale = IQR(df$Age))

# 'EstimatedSalary' variable does not contain any outliers, so it would be best to scale it with Standardization scaling that center the values around 0 and scale them to have a standard deviation of 1
df_scaled_1$EstimatedSalary <- scale(df_scaled_1$EstimatedSalary)

# 'Balance' column shows that 25% of the data lies to the right of the upper quartile value, it suggests that the distribution of the data is skewed to the right. In this case, it may be appropriate to use a scaling method that is robust to skewness, 
# such Robust Scaling
df_scaled_1$Balance <- scale(df$Balance, median(df$Balance), scale = IQR(df$Balance))

df_scaled_1$Tenure <- scale(df_scaled_1$Tenure)

df_scaled_1$NumOfProducts <- scale(df_scaled_1$NumOfProducts)

# 'CreditScore' column by the boxplots from the begining of this notebook, seems to be left skewed. So for the same reasons as the 'Balance' column, we will scale the data with the Robust scaler
df_scaled_1$CreditScore <- scale(df$CreditScore, median(df$CreditScore), scale = IQR(df$CreditScore))

head(df_scaled_1)
```

    ##   CreditScore       Age    Tenure    Balance NumOfProducts HasCrCard
    ## 1 -0.24626866 0.4166667 -1.041708 -0.7614800    -0.9115379         1
    ## 2 -0.32835821 0.3333333 -1.387468 -0.1049063    -0.9115379         0
    ## 3 -1.11940299 0.4166667  1.032856  0.4893465     2.5269303         1
    ## 4  0.35074627 0.1666667 -1.387468 -0.7614800     0.8076962         0
    ## 5  1.47761194 0.5000000 -1.041708  0.2218062    -0.9115379         1
    ## 6 -0.05223881 0.5833333  1.032856  0.1297140     0.8076962         1
    ##   IsActiveMember EstimatedSalary Exited France Germany Female
    ## 1              1       0.0218854      1      1       0      1
    ## 2              1       0.2165229      0      0       0      1
    ## 3              0       0.2406749      1      1       0      1
    ## 4              0      -0.1089125      0      1       0      1
    ## 5              1      -0.3652575      0      0       0      1
    ## 6              0       0.8636071      1      0       0      0

Third Dataframe - balanced

``` r
df_sampled <- oversampled_data$data
df_sampled_scaled <- df_sampled

# 'Age' variable is right skewed, therefore it may be better to use a scaling method that is less affected by outliers, such as Robust scaling
df_sampled_scaled$Age <- scale(df_sampled$Age, median(df_sampled$Age), scale = IQR(df_sampled$Age))

# 'EstimatedSalary' variable does not contain any outliers, so it would be best to scale it with Standardization scaling that center the values around 0 and scale them to have a standard deviation of 1
df_sampled_scaled$EstimatedSalary <- scale(df_sampled_scaled$EstimatedSalary)

# 'Balance' column shows that 25% of the data lies to the right of the upper quartile value, it suggests that the distribution of the data is skewed to the right. In this case, it may be appropriate to use a scaling method that is robust to skewness, 
# such Robust Scaling
df_sampled_scaled$Balance <- scale(df_sampled$Balance, median(df_sampled$Balance), scale = IQR(df_sampled$Balance))

df_sampled_scaled$Tenure <- scale(df_sampled_scaled$Tenure)

df_sampled_scaled$NumOfProducts <- scale(df_sampled_scaled$NumOfProducts)

# 'CreditScore' column by the boxplots from the begining of this notebook, seems to be left skewed. So for the same reasons as the 'Balance' column, we will scale the data with the Robust scaler
df_sampled_scaled$CreditScore <- scale(df_sampled$CreditScore, median(df_sampled$CreditScore), scale = IQR(df_sampled$CreditScore))

head(df_sampled_scaled)
```

    ##   CreditScore         Age     Tenure    Balance NumOfProducts HasCrCard
    ## 1  -0.3138686  0.07142857 -1.3634842 -0.1485719    -0.7602787         0
    ## 2   0.3503650 -0.07142857 -1.3634842 -0.7976966     0.7250273         0
    ## 3   1.4525547  0.21428571 -1.0203029  0.1744339    -0.7602787         1
    ## 4   1.2481752  0.71428571  0.6956034 -0.7976966     0.7250273         1
    ## 5  -1.0948905  0.28571429 -0.3339404  0.3025446     0.7250273         0
    ## 6   0.2408759 -0.92857143 -1.0203029  0.2448632    -0.7602787         1
    ##   IsActiveMember EstimatedSalary Exited France Germany Female
    ## 1              1       0.2135675      0      0       0      1
    ## 2              0      -0.1113676      0      1       0      1
    ## 3              1      -0.3673186      0      0       0      1
    ## 4              1      -1.5656251      0      1       0      0
    ## 5              1      -0.4392573      0      1       0      0
    ## 6              1      -0.4950702      0      1       0      0

## 3. Classification models

## 3.1.1 Logistic Regression

``` r
logreg_fit <- glm(Exited ~., data = df)

# 5-fold cross validation to estimate test error (to check if model performs well for the data)
set.seed(1)
cv_error_logreg <- cv.glm(df, logreg_fit, K = 5 ) # k=5 since we have a small number of data (10000)
```

``` r
# test error of model (average mean-squared error of the 5 folds)
cv_error_logreg$delta[1] # == mean((logreg_probs - df$Exited)^2), delta is in (0,1)
```

    ## [1] 0.1381505

``` r
logreg_probs <- predict(logreg_fit, type = "response")
logreg_probs[1:10]
```

    ##           1           2           3           4           5           6 
    ##  0.16915251  0.19705027  0.32924767  0.26001293  0.20048932  0.27133548 
    ##           7           8           9          10 
    ##  0.13280979  0.30116529  0.15602331 -0.03780792

``` r
logreg_pred <- rep(0, 10000)
logreg_pred[logreg_probs > .5] = 1

table(logreg_pred, df$Exited)
```

    ##            
    ## logreg_pred    0    1
    ##           0 7816 1779
    ##           1  147  258

``` r
mean(logreg_pred == df$Exited)
```

    ## [1] 0.8074

Here, the output delta of the function cv.glm represents the mean of
squares of the real values (either 0 or 1) and the predicted
probabilities for being equal to 1, hence the smaller is the better is.

``` r
# check if scaling affects the performance of the model
logreg_fit_initial <- glm(Exited ~., data = df_scaled_1)
set.seed(1)
cv_error_logreg_initial <- cv.glm(df, logreg_fit_initial, K = 5 )
cv_error_logreg_initial$delta[1]
```

    ## [1] 0.1381505

``` r
logreg_probs_initial <- predict(logreg_fit_initial, type = "response")

logreg_pred_initial <- rep(0, 10000)
logreg_pred_initial[logreg_probs_initial > .5] = 1

table(logreg_pred_initial, df$Exited)
```

    ##                    
    ## logreg_pred_initial    0    1
    ##                   0 7816 1779
    ##                   1  147  258

``` r
mean(logreg_pred_initial == df$Exited)
```

    ## [1] 0.8074

``` r
# check if normal scaling affects the performance of the model
logreg_fit_initial <- glm(Exited ~., data = df_scaled)
set.seed(1)
cv_error_logreg_initial <- cv.glm(df, logreg_fit_initial, K = 5 )
cv_error_logreg_initial$delta
```

    ## [1] 0.1381505 0.1381054

``` r
logreg_probs_initial <- predict(logreg_fit_initial, type = "response")

logreg_pred_initial <- rep(0, 10000)
logreg_pred_initial[logreg_probs_initial > .5] = 1

table(logreg_pred_initial, df$Exited)
```

    ##                    
    ## logreg_pred_initial    0    1
    ##                   0 7816 1779
    ##                   1  147  258

``` r
mean(logreg_pred_initial == df$Exited)
```

    ## [1] 0.8074

Logistic Regression seems to perform well considering the accuracy,
which is just over 0.8. However we classify points as ‘0’ whereas they
are actually ‘1’ (about 0.2), meaning that we predicted customers that
they would not exit but they eventually did so. This may happens since
we have imbalance in the target column, since we have 7963 customers who
stay with the bank and 2037 churned (we have about 80% information for
customers who stayed).

``` r
# investigate the significance of each of the columns through using 5-fold CV
set.seed(1)
train_data <- trainControl(method="cv", number=5) 
model_logreg <- train(as.factor(Exited)~., data = df, family = binomial(), trControl = train_data, method = "glm")
summary(model_logreg)
```

    ## 
    ## Call:
    ## NULL
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.3097  -0.6589  -0.4560  -0.2697   2.9940  
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)     -3.886e+00  2.497e-01 -15.559  < 2e-16 ***
    ## CreditScore     -6.683e-04  2.803e-04  -2.384   0.0171 *  
    ## Age              7.271e-02  2.576e-03  28.230  < 2e-16 ***
    ## Tenure          -1.595e-02  9.355e-03  -1.705   0.0882 .  
    ## Balance          2.637e-06  5.142e-07   5.128 2.92e-07 ***
    ## NumOfProducts   -1.015e-01  4.713e-02  -2.154   0.0312 *  
    ## HasCrCard       -4.468e-02  5.934e-02  -0.753   0.4515    
    ## IsActiveMember  -1.075e+00  5.769e-02 -18.643  < 2e-16 ***
    ## EstimatedSalary  4.807e-07  4.737e-07   1.015   0.3102    
    ## France          -3.522e-02  7.064e-02  -0.499   0.6181    
    ## Germany          7.395e-01  7.881e-02   9.383  < 2e-16 ***
    ## Female           5.285e-01  5.449e-02   9.699  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 10109.8  on 9999  degrees of freedom
    ## Residual deviance:  8561.4  on 9988  degrees of freedom
    ## AIC: 8585.4
    ## 
    ## Number of Fisher Scoring iterations: 5

By using 5-fold CV at first glance (assuming that we have all predictors
in the model) we can say that CreditScore, Age, Balance, NumOfProducts,
IsActiveMember, Germany, and Female are the most important features for
classifying the output of variable Exited. Before we proceed just with
these predictors we can check how their p-values are affected by
dropping just the one with largest p-value each time.

``` r
# same as before, just without France, due the largest p-value
set.seed(1)
model_logreg1 <- train(as.factor(Exited)~. -France, data = df, family = binomial(), trControl = train_data, method = "glm")
summary(model_logreg1)
```

    ## 
    ## Call:
    ## NULL
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.3099  -0.6584  -0.4559  -0.2691   2.9901  
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)     -3.911e+00  2.445e-01 -15.994  < 2e-16 ***
    ## CreditScore     -6.666e-04  2.803e-04  -2.378   0.0174 *  
    ## Age              7.272e-02  2.575e-03  28.238  < 2e-16 ***
    ## Tenure          -1.598e-02  9.354e-03  -1.708   0.0876 .  
    ## Balance          2.637e-06  5.142e-07   5.129 2.91e-07 ***
    ## NumOfProducts   -1.013e-01  4.713e-02  -2.149   0.0316 *  
    ## HasCrCard       -4.493e-02  5.934e-02  -0.757   0.4489    
    ## IsActiveMember  -1.075e+00  5.768e-02 -18.640  < 2e-16 ***
    ## EstimatedSalary  4.813e-07  4.736e-07   1.016   0.3095    
    ## Germany          7.629e-01  6.336e-02  12.041  < 2e-16 ***
    ## Female           5.283e-01  5.449e-02   9.697  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 10109.8  on 9999  degrees of freedom
    ## Residual deviance:  8561.6  on 9989  degrees of freedom
    ## AIC: 8583.6
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
# same as before, just without EstimatedSalary, due the 2nd larger p-value
set.seed(1)
model_logreg2 <- train(as.factor(Exited)~. -EstimatedSalary, data = df, family = binomial(), trControl = train_data, method = "glm")
summary(model_logreg2)
```

    ## 
    ## Call:
    ## NULL
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.3216  -0.6596  -0.4561  -0.2686   2.9869  
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)    -3.837e+00  2.450e-01 -15.660  < 2e-16 ***
    ## CreditScore    -6.707e-04  2.803e-04  -2.393   0.0167 *  
    ## Age             7.268e-02  2.575e-03  28.225  < 2e-16 ***
    ## Tenure         -1.578e-02  9.353e-03  -1.687   0.0915 .  
    ## Balance         2.645e-06  5.141e-07   5.144 2.69e-07 ***
    ## NumOfProducts  -1.007e-01  4.713e-02  -2.136   0.0327 *  
    ## HasCrCard      -4.475e-02  5.934e-02  -0.754   0.4508    
    ## IsActiveMember -1.076e+00  5.768e-02 -18.656  < 2e-16 ***
    ## France         -3.541e-02  7.063e-02  -0.501   0.6161    
    ## Germany         7.396e-01  7.880e-02   9.385  < 2e-16 ***
    ## Female          5.290e-01  5.448e-02   9.709  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 10109.8  on 9999  degrees of freedom
    ## Residual deviance:  8562.4  on 9989  degrees of freedom
    ## AIC: 8584.4
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
# explore how model with just all the above significant predictors performs
reduced_model_logreg_fit <- glm(Exited ~ CreditScore+ Age+ Balance+ NumOfProducts+ IsActiveMember+ Germany+ Female, data = df)
set.seed(1)
cv.glm(df, reduced_model_logreg_fit, K = 5 )$delta[1]
```

    ## [1] 0.1380705

``` r
reduced_model_logreg_probs <- predict(reduced_model_logreg_fit, type = "response")
reduced_model_logreg_pred <- rep(0, 10000)
reduced_model_logreg_pred[reduced_model_logreg_probs > .5] = 1

table(reduced_model_logreg_pred, df$Exited)
```

    ##                          
    ## reduced_model_logreg_pred    0    1
    ##                         0 7817 1779
    ##                         1  146  258

``` r
mean(reduced_model_logreg_pred == df$Exited)
```

    ## [1] 0.8075

The reduced model does not make any significance difference, though it
is a bit better than the full one. We can investigate more, by using
only the 5 most important variables.

``` r
# explore how model with just the 5 most significant predictors performs 
reduced_model_logreg_fit5 <- glm(Exited ~  Age+ Balance+  IsActiveMember+ Germany+ Female, data = df)
set.seed(1)
cv.glm(df, reduced_model_logreg_fit5, K = 5 )$delta[1]
```

    ## [1] 0.1381429

``` r
reduced_model_logreg_probs5 <- predict(reduced_model_logreg_fit5, type = "response")
reduced_model_logreg_pred5 <- rep(0, 10000)
reduced_model_logreg_pred5[reduced_model_logreg_probs5 > .5] = 1

table(reduced_model_logreg_pred5, df$Exited)
```

    ##                           
    ## reduced_model_logreg_pred5    0    1
    ##                          0 7809 1785
    ##                          1  154  252

``` r
mean(reduced_model_logreg_pred5 == df$Exited)
```

    ## [1] 0.8061

As we expected, the last model performs worse than the one which
contains all the significant variables even they are less than the
others.

Although the accuracy is slightly lower than the full logistic model we
have less error (delta) in this case. Thus, this model provides more
(negligible though) certainty for the output in comparison to the first
one.

As for the specific method, since the accuracies are very closed to each
other we could select the last one, due to its simplicity.

``` r
plot(hatvalues(reduced_model_logreg_fit))
```

![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-41-1.png)<!-- -->

``` r
hats <- as.data.frame(hatvalues(reduced_model_logreg_fit))
hats[order(-hats['hatvalues(reduced_model_logreg_fit)']), ][1:20]
```

    ## Warning in xtfrm.data.frame(x): cannot xtfrm data frames

    ##  [1] 0.003888099 0.003804020 0.003528071 0.003521661 0.003385990 0.003354987
    ##  [7] 0.003317083 0.003314143 0.003296831 0.003264594 0.003213716 0.003210439
    ## [13] 0.003203007 0.003193366 0.003130812 0.003089660 0.003070146 0.003067597
    ## [19] 0.003045712 0.003006425

From the above plot and first 20 greater leverage values we can see that
there are no values with high leverage, hence we do not have to concern
about any influence from the outliers, which were detected during the
pre-processing.

lasso logistic for selecting the best model for logistic regression

``` r
x <- model.matrix(Exited ~ ., data=df)[, -9]
y <- df$Exited
lasso.logistic.mod <- glmnet(x, y, alpha = 1, family='binomial')
plot(lasso.logistic.mod)
```

![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-43-1.png)<!-- -->

``` r
plot(lasso.logistic.mod, xvar="lambda")
```

![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-43-2.png)<!-- -->

``` r
coef(lasso.logistic.mod)[,10]
```

    ##    (Intercept)    (Intercept)    CreditScore            Age         Tenure 
    ##    -2.76928183     0.00000000     0.00000000     0.03664806     0.00000000 
    ##        Balance  NumOfProducts      HasCrCard IsActiveMember         France 
    ##     0.00000000     0.00000000     0.00000000    -0.24787949     0.00000000 
    ##        Germany         Female 
    ##     0.23370407     0.00000000

Lasso regression suggests to keep only Age, IsActiveMember, and Germany.

``` r
set.seed(1) 
model_logreg <- glm(Exited~ Age+ IsActiveMember+ Germany, data = df)

cv.glm(df, model_logreg, K=5)$delta[1]
```

    ## [1] 0.1398845

``` r
logreg_probs_model <- predict(model_logreg, type = "response")

logreg_pred_model <- rep(0, 10000)
logreg_pred_model[logreg_probs_model > .5] = 1

table(logreg_pred_model, df$Exited)
```

    ##                  
    ## logreg_pred_model    0    1
    ##                 0 7833 1827
    ##                 1  130  210

``` r
mean(logreg_pred_model == df$Exited)
```

    ## [1] 0.8043

The lasso method suggests a model which provides higher error, as well
as lower accuracy, hence we could not select it.

## 3.1.2 Logistic Regression // Oversampled data

``` r
logreg_fit <- glm(Exited ~., data = df_sampled_scaled)

# 5-fold cross validation to estimate test error (to check if model performs well for the data)
set.seed(1)
cv_error_logreg <- cv.glm(df_sampled_scaled, logreg_fit, K = 5 )
```

``` r
# test error of model (average mean-squared error of the 5 folds)
cv_error_logreg$delta[1] # == mean((logreg_probs - df$Exited)^2), delta is in (0,1)
```

    ## [1] 0.1964009

``` r
logreg_probs <- predict(logreg_fit, type = "response")
logreg_probs[1:10]
```

    ##          1          2          3          4          5          6          7 
    ## 0.46473048 0.52983666 0.46305494 0.32774685 0.39222495 0.09571761 0.34683771 
    ##          8          9         10 
    ## 0.18432915 0.42438034 0.31801760

``` r
logreg_pred <- rep(0, nrow(df_sampled_scaled))
logreg_pred[logreg_probs > .5] = 1

table(logreg_pred, df_sampled_scaled$Exited)
```

    ##            
    ## logreg_pred    0    1
    ##           0 5730 2422
    ##           1 2233 5584

``` r
mean(logreg_pred == df_sampled_scaled$Exited)
```

    ## [1] 0.7084977

``` r
# approximate numbers

cat('Sensitivity using the oversampled data:', 5524/(5524+2464))
```

    ## Sensitivity using the oversampled data: 0.6915373

``` r
cat('\nSensitivity using the original data:', 258/(258+1779))
```

    ## 
    ## Sensitivity using the original data: 0.1266568

The oversampling does not provide better accuracy in logistic model.
However, we have better sensitivity here; we are able with this model to
predict correctly more customers who will exit the bank. Specifically,
we have 0.7 sensitivity whereas by using only initial data we had about
0.13

## 3.2. Naive Bayes

## 3.2.1 Binomial Naives Bayes

``` r
# create 5-fold cross validation randomly
fold1 <- c(1:8000)
fold2 <- c(1:8000)
fold3 <- c(1:8000)
fold4 <- c(1:8000)
fold5 <- c(1:8000)
train <- data.frame(fold1, fold2, fold3, fold4, fold5)
library(dplyr)
for (i in 1:5){
  set.seed(i)
  train[,i] <- df$Exited %>% createDataPartition(p = 0.8, list = FALSE)
}
```

``` r
nb_fit1 <- naiveBayes(Exited~ ., data = df, subset = train[,1], type = 'class')
nb_fit2 <- naiveBayes(Exited~ ., data = df, subset = train[,2], type = 'class')
nb_fit3 <- naiveBayes(Exited~ ., data = df, subset = train[,3], type = 'class')
nb_fit4 <- naiveBayes(Exited~ ., data = df, subset = train[,4], type = 'class')
nb_fit5 <- naiveBayes(Exited~ ., data = df, subset = train[,5], type = 'class')
```

``` r
table(predict(nb_fit1, df[-train[,1],]), df$Exited[-train[,1]])
```

    ##    
    ##        0    1
    ##   0 1472  277
    ##   1  105  146

``` r
table(predict(nb_fit2, df[-train[,2],]), df$Exited[-train[,2]])
```

    ##    
    ##        0    1
    ##   0 1514  254
    ##   1  103  129

``` r
table(predict(nb_fit3, df[-train[,3],]), df$Exited[-train[,3]])
```

    ##    
    ##        0    1
    ##   0 1520  247
    ##   1  100  133

``` r
table(predict(nb_fit4, df[-train[,4],]), df$Exited[-train[,4]])
```

    ##    
    ##        0    1
    ##   0 1505  254
    ##   1  102  139

``` r
table(predict(nb_fit5, df[-train[,5],]), df$Exited[-train[,5]])
```

    ##    
    ##        0    1
    ##   0 1526  248
    ##   1   84  142

``` r
accuracy <- c()
for (i in 1:5){
  nb_pred <- predict(naiveBayes(Exited~ ., data = df, subset = train[,i],type = 'class'), df[-train[,i],] )
  accuracy[i] = mean(nb_pred == df$Exited[-train[,i]])
}
accuracy
```

    ## [1] 0.8090 0.8215 0.8265 0.8220 0.8340

``` r
mean(accuracy)
```

    ## [1] 0.8226

We can see that the Naive Bayes model performs better than both reduced
and full Logistic regression model. We can investigate if we have better
accuracy in the case of less predictors.

``` r
# Age+ Balance+  IsActiveMember+ Germany+ Female
accuracy <- c()
for (i in 1:5){
  nb_pred <- predict(naiveBayes(Exited~ Age+ Balance+  IsActiveMember+ Germany+ Female, data = df, subset = train[,i], type = 'class'), df[-train[,i],] )
  accuracy[i] = mean(nb_pred == df$Exited[-train[,i]])
}
accuracy
```

    ## [1] 0.7975 0.8150 0.8170 0.8140 0.8210

``` r
mean(accuracy)
```

    ## [1] 0.8129

The reduced Naive Bayes model does not perform better that the full one.

## 3.2.2 Naive Bayes with kernel density estimator

``` r
y <- as.character(df$Exited)
accuracy <- c()
for (i in 1:5){
  nb_pred <- predict(naive_bayes(y~ ., data = df, subset = train[,i], usekernel=T), df[-train[,i],] )
  accuracy[i] = mean(nb_pred == y[-train[,i]])
}
accuracy
```

    ## [1] 0.9610 0.9680 0.9720 0.9705 0.9635

``` r
mean(accuracy)
```

    ## [1] 0.967

``` r
nb_fit1 <- naive_bayes(y~ ., data = df, subset = train[,1],usekernel=T)
nb_fit2 <- naive_bayes(y~ ., data = df, subset = train[,2],usekernel=T)
nb_fit3 <- naive_bayes(y~ ., data = df, subset = train[,3],usekernel=T)
nb_fit4 <- naive_bayes(y~ ., data = df, subset = train[,4],usekernel=T)
nb_fit5 <- naive_bayes(y~ ., data = df, subset = train[,5],usekernel=T)

# tables for 5 cases
table(predict(nb_fit1, df[-train[,1],]), y[-train[,1]])
```

    ##    
    ##        0    1
    ##   0 1577   78
    ##   1    0  345

``` r
table(predict(nb_fit2, df[-train[,2],]), y[-train[,2]])
```

    ##    
    ##        0    1
    ##   0 1617   64
    ##   1    0  319

``` r
table(predict(nb_fit3, df[-train[,3],]), y[-train[,3]])
```

    ##    
    ##        0    1
    ##   0 1620   56
    ##   1    0  324

``` r
table(predict(nb_fit4, df[-train[,4],]), y[-train[,4]])
```

    ##    
    ##        0    1
    ##   0 1607   59
    ##   1    0  334

``` r
table(predict(nb_fit5, df[-train[,5],]), y[-train[,5]])
```

    ##    
    ##        0    1
    ##   0 1610   73
    ##   1    0  317

``` r
# use all data
nb_fit1 <- naive_bayes(y~ ., data = df,usekernel=T)
table(predict(nb_fit1, df), y)
```

    ##    y
    ##        0    1
    ##   0 7963  322
    ##   1    0 1715

We can see that the accuracy of the Naive Bayes algorithm with all
predictors is extremely nice for our data just under 0.97, as well as
the sensitivity which is about 0.85.

We can investigate to improve more the sensitivity by using the
oversampled data.

## 3.2.3 Naive Bayes with Oversampled Data

``` r
# using validation approach
set.seed(1)
train <- df_sampled_scaled$Exited %>% createDataPartition(p = 0.8, list = FALSE)
```

``` r
nb_fit1 <- naiveBayes(Exited~ ., data = df_sampled_scaled, subset = train,type = 'class')
table(predict(nb_fit1, df_sampled_scaled[-train,]), df_sampled_scaled$Exited[-train])
```

    ##    
    ##        0    1
    ##   0 1186  491
    ##   1  431 1085

``` r
nb_pred <- predict(naiveBayes(Exited~ ., data = df_sampled_scaled, subset = train,type = 'class'), df_sampled_scaled[-train,] )
accuracy <- mean(nb_pred == df_sampled_scaled$Exited[-train])
cat('Accuracy using the oversample data:',accuracy)
```

    ## Accuracy using the oversample data: 0.7112433

``` r
# approximate numbers

cat('Sensitivity using the oversample data:',1089/(1089+501))
```

    ## Sensitivity using the oversample data: 0.6849057

``` r
# Naive Bayes with kernel density estimator
set.seed(1)
y <- as.character(df_sampled_scaled$Exited)
nb_fit_kernel <- naive_bayes(y~ ., data = df_sampled_scaled, subset = train, usekernel=T)
table(predict(nb_fit_kernel, df_sampled_scaled[-train,]), y[-train])
```

    ##    
    ##        0    1
    ##   0 1617    0
    ##   1    0 1576

Naive Bayes by using a kernel density estimator for the densities of
fk(xj) on the oversampled data provides perfect accuracy and
sensitivity; only very few customers of the test set predicts
incorrectly.

``` r
# training confusion matrix
nb_fit_kernel <- naive_bayes(y~ ., data = df_sampled_scaled, usekernel=T)
table(predict(nb_fit_kernel, df_sampled_scaled), y)
```

    ##    y
    ##        0    1
    ##   0 7963    6
    ##   1    0 8000

## 3.3. QDA

``` r
# Create separate data frames for churned and not churned customers
df_churned <- df[df$Exited == 1, ]
df_not_churned <- df[df$Exited == 0, ]

# Convert the data frame from wide format to long format using gather
df_long <- gather(df, key = "variable", value = "value", -Exited)
df_long$Exited <- factor(df_long$Exited)
# Plot histograms of each variable separated by churn and not churned customers using facet_wrap
ggplot(df_long, aes(value, fill = Exited)) + 
  geom_histogram(bins = 30, alpha = 0.5, position = "identity") +
  facet_wrap(~ variable, scales = "free") +
  labs(x = "", y = "Count", fill = "Exited")
```

![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-62-1.png)<!-- -->

## 3.3.1. QDA simple fit with all the data

``` r
qda.fit <- qda(Exited ~., data = df_scaled_1)
qda.fit
```

    ## Call:
    ## qda(Exited ~ ., data = df_scaled_1)
    ## 
    ## Prior probabilities of groups:
    ##      0      1 
    ## 0.7963 0.2037 
    ## 
    ## Group means:
    ##    CreditScore       Age       Tenure     Balance NumOfProducts HasCrCard
    ## 0 -0.001095552 0.0340324  0.007080802 -0.19157342    0.02418487 0.7071455
    ## 1 -0.049615692 0.6531664 -0.027680132 -0.04771074   -0.09454302 0.6990673
    ##   IsActiveMember EstimatedSalary    France   Germany    Female
    ## 0      0.5545649    -0.006117981 0.5279417 0.2128595 0.4274771
    ## 1      0.3608247     0.023916290 0.3976436 0.3996073 0.5591556

``` r
# Make predictions on the training data
set.seed(1234)
train_pred <- predict(qda.fit, df_scaled_1)

# Calculate accuracy
train_accuracy <- mean(train_pred$class == df_scaled_1$Exited)
train_accuracy 
```

    ## [1] 0.836

The accuracy is pretty good, although it is on the training data. So, we
need to seperate our data into train and test, in order to figure out
how our model can perform with unseen data, with cross validation.

## 3.3.2. QDA Cross Validation, with threshold 0.5

``` r
set.seed(1234)
X <- df_scaled_1[ , -which(names(df_scaled_1) == "Exited")] # Predictor variables
y <- as.factor(df_scaled_1$Exited)# Response variable

model <- train(x=X, y=y, method="qda")# Define qda

k <- 5 # Define the number of splits for k-fold cross-validation
folds <- createFolds(y, k=k)

conf_matrices <- list() # store confusion matrices
predicted_all <- c() # store predicted y's of test set, of all folds 
actual_all <- c() # store their actual y's of test set, of all folds

# Use cross-validation to evaluate qda's performance and calculate confusion matrices, with the help of the created lists
for(i in 1:k){
  test_idx <- folds[[i]] 
  train_idx <- setdiff(1:length(y), test_idx)
  model <- train(x=X[train_idx, ], y=y[train_idx], method="qda")
  predicted <- predict(model, newdata=X[test_idx, ])
  conf_matrix <- table(predicted, y[test_idx])
  conf_matrices[[i]] <- conf_matrix
  
  # Store predicted and actual values for this fold
  predicted_all <- c(predicted_all, predicted)
  actual_all <- c(actual_all, y[test_idx])
  
  # Print the confusion matrix for this fold
  cat("Confusion matrix for fold ", i, ":\n")
  print(conf_matrix)
  
  # Print the number of observations in test set for this fold
  cat("Number of observations in test set for fold ", i, ":", length(test_idx), "\n")
}
```

    ## Confusion matrix for fold  1 :
    ##          
    ## predicted    0    1
    ##         0 1508  242
    ##         1   85  165
    ## Number of observations in test set for fold  1 : 2000 
    ## Confusion matrix for fold  2 :
    ##          
    ## predicted    0    1
    ##         0 1514  255
    ##         1   78  153
    ## Number of observations in test set for fold  2 : 2000 
    ## Confusion matrix for fold  3 :
    ##          
    ## predicted    0    1
    ##         0 1514  248
    ##         1   78  159
    ## Number of observations in test set for fold  3 : 1999 
    ## Confusion matrix for fold  4 :
    ##          
    ## predicted    0    1
    ##         0 1500  252
    ##         1   93  155
    ## Number of observations in test set for fold  4 : 2000 
    ## Confusion matrix for fold  5 :
    ##          
    ## predicted    0    1
    ##         0 1521  264
    ##         1   72  144
    ## Number of observations in test set for fold  5 : 2001

``` r
accuracy <- mean(predicted_all == actual_all) # Calculate the overall accuracy

cat("Cross-validation accuracy:", round(accuracy, 3), "\n")
```

    ## Cross-validation accuracy: 0.833

``` r
# RECALL and PRECISION for all folds:
TP <- 153
FN <- 255
FP <- 78
RECALL <- TP / (TP + FN)
PRECISION <- TP / (TP + FP)

TP1 <- 165
FN1 <- 242
FP1 <- 85
RECALL1 <- TP1 / (TP1 + FN1)
PRECISION1 <- TP1 / (TP1 + FP1)

TP2 <- 159
FN2 <- 248
FP2 <- 78
RECALL2 <- TP2 / (TP2 + FN2)
PRECISION2 <- TP2 / (TP2 + FP2)

TP3 <- 155
FN3 <- 252
FP3 <- 93
RECALL3 <- TP3 / (TP3 + FN3)
PRECISION3 <- TP3 / (TP3 + FP3)

TP4 <- 155
FN4 <- 252
FP4 <- 93
RECALL4 <- TP4 / (TP4 + FN4)
PRECISION4 <- TP4 / (TP4 + FP4)

cat("Average Recall:", (RECALL + RECALL1 + RECALL2 + RECALL3 + RECALL4)/5)
```

    ## Average Recall: 0.3865479

``` r
cat("\nAverage Precision:", (PRECISION + PRECISION1 + PRECISION2 + PRECISION3 + PRECISION4)/5)
```

    ## 
    ## Average Precision: 0.6486447

``` r
# We can clearly see that recall rate is low
```

We can clearly see that the average recall rate is pretty low, even
though, we must be more consetrated in maximizing the recall rate, and
not precision.

## 3.3.3. QDA - ROC curve

``` r
set.seed(1234)
# Define response variable
y <- as.factor(df_scaled_1[, 9])

# Perform 5-fold cross-validation
cv <- createFolds(y, k = 5)

# Initialize vector to store predicted probabilities
probs <- rep(0, nrow(df_scaled_1))

# Loop through each fold of the cross-validation
for (i in 1:5) {
  # Get indices for training and testing data
  train_indices <- unlist(cv[-i])
  test_indices <- cv[[i]]
  
  # Fit QDA model to training data
  qda_model <- qda(df_scaled_1[train_indices, -9], y[train_indices])
  
  # Predict class probabilities for testing data
  probs[test_indices] <- predict(qda_model, df_scaled_1[test_indices, -9])$posterior[, 2]
}

# Generate ROC curve and calculate AUC
roc <- roc(y, probs)
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

``` r
auc <- auc(roc)

# Plot ROC curve with thresholds
plot(roc, main = paste("ROC Curve (AUC =", round(auc, 2), ")"), print.thres = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), print.auc = TRUE)
```

![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-67-1.png)<!-- -->

In k-fold cross-validation, the data is divided into k folds, and each
fold is used as a validation set while the rest of the data is used for
training. This process is repeated k times, with each fold used exactly
once as a validation set.

At the end of k-fold cross-validation, you have k different models (one
for each fold), and you can calculate the performance metric (such as
ROC-AUC) for each of them. The overall performance metric is then
typically calculated as the average of the performance metrics across
all folds. As we can see the model performs better than a random model.

Each point out of ten, represent a threshold. As the threshold
decreases, the Specificity decreases and the Sensitivity increases. For
example, for the threshold 0.4, the True Positive rate, which is the
Recall rate is at 0.474, while the False positive rate is at 0.916. When
the threshold is set to 0.2, the recall rate increases form 0.474 to
0.720 and the specificity decreases from 0.916 to 0.749. It is not bad
at all! When it comes to specificity, the model, even after lowering the
threshold, still predicted correctly as negative a lot of cases, without
marking too many cases as false positive! That are some good news, we
could lower the threshold, get better recall, and and get not that too
bad of a precision rate!

Let’s set threshold lower than 0.5, as we are interested to predict
correctly the clients who will churn. By lowering the threshold, we will
achieve a higher Recall rate, as it will accept as a positive instance
easier than before.

## 3.3.4. QDA - QDA Cross Validation, with threshold 0.2

``` r
set.seed(1234)
X <- df_scaled_1[ , -which(names(df_scaled_1) == "Exited")] # Predictor variables
y <- as.factor(df_scaled_1$Exited)# Response variable

model <- train(x=X, y=y, method="qda")# Define qda

k <- 5 # Define the number of splits for k-fold cross-validation
folds <- createFolds(y, k=k)

conf_matrices <- list() # store confusion matrices
predicted_all <- c() # store predicted y's of test set, of all folds 
actual_all <- c() # store their actual y's of test set, of all folds

# Use cross-validation to evaluate qda's performance and calculate confusion matrices, with the help of the created lists
for(i in 1:k){
  test_idx <- folds[[i]] 
  train_idx <- setdiff(1:length(y), test_idx)
  model <- train(x=X[train_idx, ], y=y[train_idx], method="qda")
  predicted_probs <- predict(model, newdata = X[test_idx, ], type = "prob")[, 2]
  predicted_labels <- ifelse(predicted_probs >= 0.2, "1", "0") # Set threshold to 0.2
  conf_matrix <- table(predicted = predicted_labels, actual = y[test_idx])
  conf_matrices[[i]] <- conf_matrix
  
  # Store predicted and actual values for this fold
  predicted_all <- c(predicted_all, predicted)
  actual_all <- c(actual_all, y[test_idx])
  
  # Print the confusion matrix for this fold
  cat("Confusion matrix for fold ", i, ":\n")
  print(conf_matrix)
  
  # Print the number of observations in test set for this fold
  cat("Number of observations in test set for fold ", i, ":", length(test_idx), "\n")
}
```

    ## Confusion matrix for fold  1 :
    ##          actual
    ## predicted    0    1
    ##         0 1165  105
    ##         1  428  302
    ## Number of observations in test set for fold  1 : 2000 
    ## Confusion matrix for fold  2 :
    ##          actual
    ## predicted    0    1
    ##         0 1184  125
    ##         1  408  283
    ## Number of observations in test set for fold  2 : 2000 
    ## Confusion matrix for fold  3 :
    ##          actual
    ## predicted    0    1
    ##         0 1217  125
    ##         1  375  282
    ## Number of observations in test set for fold  3 : 1999 
    ## Confusion matrix for fold  4 :
    ##          actual
    ## predicted    0    1
    ##         0 1191  105
    ##         1  402  302
    ## Number of observations in test set for fold  4 : 2000 
    ## Confusion matrix for fold  5 :
    ##          actual
    ## predicted    0    1
    ##         0 1196  121
    ##         1  397  287
    ## Number of observations in test set for fold  5 : 2001

``` r
accuracy <- mean(predicted_all == actual_all) # Calculate the overall accuracy
```

    ## Warning in predicted_all == actual_all: longer object length is not a multiple
    ## of shorter object length

``` r
cat("Cross-validation accuracy:", round(accuracy, 3), "\n")
```

    ## Cross-validation accuracy: 0.731

``` r
# RECALL AND PRECISION for the first fold
TP <- 302
FN <- 105
FP <- 428
RECALL <- TP / (TP + FN)
PRECISION <- TP / (TP + FP)

TP1 <- 283
FN1 <- 125
FP1 <- 408
RECALL1 <- TP1 / (TP1 + FN1)
PRECISION1 <- TP1 / (TP1 + FP1)

TP2 <- 282
FN2 <- 125
FP2 <- 375
RECALL2 <- TP2 / (TP2 + FN2)
PRECISION2 <- TP2 / (TP2 + FP2)

TP3 <- 302
FN3 <- 105
FP3 <- 402
RECALL3 <- TP3 / (TP3 + FN3)
PRECISION3 <- TP3 / (TP3 + FP3)

TP4 <- 287
FN4 <- 121
FP4 <- 397
RECALL4 <- TP4 / (TP4 + FN4)
PRECISION4 <- TP4 / (TP4 + FP4)


cat("Average Recall:", (RECALL + RECALL1 + RECALL2 + RECALL3 + RECALL4)/5)
```

    ## Average Recall: 0.7147926

``` r
cat("\nAverage Precision:", (PRECISION + PRECISION1 + PRECISION2 + PRECISION3 + PRECISION4)/5)
```

    ## 
    ## Average Precision: 0.4202083

``` r
# we can see that now, the recall rate got better by just lowering the threshold!
```

Lowering the threshold will generally increase the recall rate, which
means that the model will correctly identify more positive cases. This
is because the lower threshold will classify more instances as positive,
including some that were previously classified as negative, thus
reducing the number of false negatives. However, this also means that
the model is likely to have a higher false positive rate, which means
that it may incorrectly classify some negative cases as positive.

## 3.3.5. QDA - QDA Cross Validation, with threshold 0.2 - Oversampled data

``` r
set.seed(1234)
X <- df_sampled_scaled[ , -which(names(df_sampled_scaled) == "Exited")] # Predictor variables
y <- as.factor(df_sampled_scaled$Exited)# Response variable

model <- train(x=X, y=y, method="qda")# Define qda

k <- 5 # Define the number of splits for k-fold cross-validation
folds <- createFolds(y, k=k)

conf_matrices <- list() # store confusion matrices
predicted_all <- c() # store predicted y's of test set, of all folds 
actual_all <- c() # store their actual y's of test set, of all folds

# Use cross-validation to evaluate qda's performance and calculate confusion matrices, with the help of the created lists
for(i in 1:k){
  test_idx <- folds[[i]] 
  train_idx <- setdiff(1:length(y), test_idx)
  model <- train(x=X[train_idx, ], y=y[train_idx], method="qda")
  predicted_probs <- predict(model, newdata = X[test_idx, ], type = "prob")[, 2]
  predicted_labels <- ifelse(predicted_probs >= 0.2, "1", "0") # Set threshold to 0.2
  conf_matrix <- table(predicted = predicted_labels, actual = y[test_idx])
  conf_matrices[[i]] <- conf_matrix
  
  # Store predicted and actual values for this fold
  predicted_all <- c(predicted_all, predicted)
  actual_all <- c(actual_all, y[test_idx])
  
  # Print the confusion matrix for this fold
  cat("Confusion matrix for fold ", i, ":\n")
  print(conf_matrix)
  
  # Print the number of observations in test set for this fold
  cat("Number of observations in test set for fold ", i, ":", length(test_idx), "\n")
}
```

    ## Confusion matrix for fold  1 :
    ##          actual
    ## predicted    0    1
    ##         0  608  100
    ##         1  985 1501
    ## Number of observations in test set for fold  1 : 3194 
    ## Confusion matrix for fold  2 :
    ##          actual
    ## predicted    0    1
    ##         0  597   85
    ##         1  995 1516
    ## Number of observations in test set for fold  2 : 3193 
    ## Confusion matrix for fold  3 :
    ##          actual
    ## predicted    0    1
    ##         0  617  107
    ##         1  975 1494
    ## Number of observations in test set for fold  3 : 3193 
    ## Confusion matrix for fold  4 :
    ##          actual
    ## predicted    0    1
    ##         0  605   76
    ##         1  988 1525
    ## Number of observations in test set for fold  4 : 3194 
    ## Confusion matrix for fold  5 :
    ##          actual
    ## predicted    0    1
    ##         0  625   88
    ##         1  968 1514
    ## Number of observations in test set for fold  5 : 3195

``` r
accuracy <- mean(predicted_all == actual_all) # Calculate the overall accuracy
```

    ## Warning in predicted_all == actual_all: longer object length is not a multiple
    ## of shorter object length

``` r
cat("Cross-validation accuracy:", round(accuracy, 3), "\n")
```

    ## Cross-validation accuracy: 0.498

``` r
TP <-1517
FP <- 979
FN <- 83
RECALL <- TP/(TP + FN)
PRECISION <- TP/(TP + FP)

TP1 <-1516
FP1 <- 985
FN1 <- 84
RECALL1 <- TP1/(TP1 + FN1)
PRECISION1 <- TP1/(TP1 + FP1)

TP2 <-1491
FP2 <- 957
FN2 <- 109
RECALL2 <- TP2/(TP2 + FN2)
PRECISION2 <- TP2/(TP2 + FP2)

TP3 <-1506
FP3 <- 977
FN3 <- 93
RECALL3 <- TP3/(TP3 + FN3)
PRECISION3 <- TP3/(TP3 + FP3)

TP4 <-1497
FP4 <- 976
FN4 <- 103
RECALL4 <- TP4/(TP4 + FN4)
PRECISION4 <- TP4/(TP4 + FP4)

cat("Average Recall:", (RECALL + RECALL1 + RECALL2 + RECALL3 + RECALL4)/5)
```

    ## Average Recall: 0.9409927

``` r
cat("\nAverage Precision:", (PRECISION + PRECISION1 + PRECISION2 + PRECISION3 + PRECISION4)/5)
```

    ## 
    ## Average Precision: 0.6069721

After performing the quadradic model with all the variables, as well as
on a subset of variables (we thought were the best ones), it is correct
to use the technique PCA, where it reduces the problem of
multicollinearity (the coefficients that the qda model will not be
reliable due to the inflation of the standard errors), by combining
explanatory variables to a smaller set of uncorrelated variables. This
method will also give us the necessary information of each variable, so
we use all the resources we have!

## 3.3.6. PCA

``` r
set.seed(1234)

predictors <- as.matrix(df_scaled_1[, -9])
pca <- prcomp(predictors, scaled = FALSE)
```

    ## Warning: In prcomp.default(predictors, scaled = FALSE) :
    ##  extra argument 'scaled' will be disregarded

``` r
loadings <- pca$rotation
loadings
```

    ##                          PC1          PC2           PC3          PC4
    ## CreditScore      0.013583031  0.009468961 -0.0006896723  0.002450336
    ## Age             -0.110468606 -0.020766776  0.0050720870 -0.988648047
    ## Tenure           0.366226619 -0.537613719 -0.7582945416 -0.035094726
    ## Balance         -0.161889078 -0.098888418 -0.0029481636 -0.002089736
    ## NumOfProducts    0.846464745  0.478619158  0.0750279664 -0.109369386
    ## HasCrCard        0.005379630 -0.001796490 -0.0136057018  0.007808069
    ## IsActiveMember  -0.008264381  0.017993927  0.0101770590 -0.070814787
    ## EstimatedSalary  0.331310291 -0.686021238  0.6472081876 -0.018736163
    ## France           0.016879899  0.014128135 -0.0004028703  0.042638247
    ## Germany         -0.023566681 -0.019363161  0.0032525285 -0.038975724
    ## Female           0.008691683  0.008286566  0.0121421606 -0.024647388
    ##                           PC5           PC6          PC7           PC8
    ## CreditScore     -0.9984490470 -0.0393127633  0.022178550 -0.0272703008
    ## Age              0.0009441103 -0.0622479701  0.033750335 -0.0694890226
    ## Tenure          -0.0003904534 -0.0008484349 -0.005371244  0.0205008417
    ## Balance         -0.0252495134  0.4443175366 -0.037894871 -0.0105599367
    ## NumOfProducts    0.0108205506  0.1123605848 -0.012799651 -0.0186795329
    ## HasCrCard        0.0062499142  0.0001923911  0.016777173 -0.0693473617
    ## IsActiveMember  -0.0330344621 -0.0440649628 -0.674833676  0.7291990229
    ## EstimatedSalary -0.0020663224 -0.0146338079 -0.008001520  0.0009966498
    ## France           0.0270994774 -0.6671853171  0.036481290 -0.0185626288
    ## Germany         -0.0215877285  0.5802316966 -0.003255697  0.0085871237
    ## Female          -0.0029691925  0.0239797272  0.734610680  0.6757124462
    ##                          PC9         PC10          PC11
    ## CreditScore      0.004210218  0.003764851  2.431068e-03
    ## Age              0.002493533 -0.005724534 -5.678178e-03
    ## Tenure          -0.011789781 -0.004277185 -3.131298e-05
    ## Balance         -0.057459629 -0.820336838 -2.968883e-01
    ## NumOfProducts   -0.013177678 -0.146431962 -4.426699e-02
    ## HasCrCard        0.995087517 -0.062298811 -2.262331e-02
    ## IsActiveMember   0.062817816 -0.011532191  1.567931e-02
    ## EstimatedSalary  0.006629132  0.007169919 -2.854026e-04
    ## France          -0.025294784 -0.547774140  4.994546e-01
    ## Germany          0.021251062  0.026729203  8.118835e-01
    ## Female           0.032881204 -0.026521963 -2.218712e-02

``` r
cor(pca$x)
```

    ##                PC1           PC2           PC3           PC4           PC5
    ## PC1   1.000000e+00  2.654606e-15 -3.174453e-16  5.362687e-16 -2.632298e-16
    ## PC2   2.654606e-15  1.000000e+00  1.642043e-15  1.481121e-16  2.895287e-16
    ## PC3  -3.174453e-16  1.642043e-15  1.000000e+00 -5.955583e-17  1.471904e-16
    ## PC4   5.362687e-16  1.481121e-16 -5.955583e-17  1.000000e+00 -1.763429e-16
    ## PC5  -2.632298e-16  2.895287e-16  1.471904e-16 -1.763429e-16  1.000000e+00
    ## PC6   2.452278e-17 -3.614253e-17  2.015063e-16  6.527516e-16  8.110144e-17
    ## PC7  -4.557194e-16  1.807207e-16  2.325280e-16  2.452611e-16 -2.287338e-16
    ## PC8  -9.750767e-17 -1.696643e-16 -4.277353e-17  1.809640e-17  3.730990e-17
    ## PC9   6.955602e-17  3.566037e-17  4.840703e-17 -2.432419e-16  1.910405e-17
    ## PC10 -7.599234e-16 -3.924946e-16  1.472771e-16  1.136736e-16  1.479684e-16
    ## PC11 -4.496936e-16  3.953445e-16 -3.322950e-16 -8.205988e-16  3.058063e-17
    ##                PC6           PC7           PC8           PC9          PC10
    ## PC1   2.452278e-17 -4.557194e-16 -9.750767e-17  6.955602e-17 -7.599234e-16
    ## PC2  -3.614253e-17  1.807207e-16 -1.696643e-16  3.566037e-17 -3.924946e-16
    ## PC3   2.015063e-16  2.325280e-16 -4.277353e-17  4.840703e-17  1.472771e-16
    ## PC4   6.527516e-16  2.452611e-16  1.809640e-17 -2.432419e-16  1.136736e-16
    ## PC5   8.110144e-17 -2.287338e-16  3.730990e-17  1.910405e-17  1.479684e-16
    ## PC6   1.000000e+00 -6.220680e-18  3.835731e-17  2.414034e-17 -2.470214e-15
    ## PC7  -6.220680e-18  1.000000e+00  1.709443e-15  3.978019e-17  3.046059e-16
    ## PC8   3.835731e-17  1.709443e-15  1.000000e+00  2.197190e-16  6.777637e-16
    ## PC9   2.414034e-17  3.978019e-17  2.197190e-16  1.000000e+00  1.105843e-16
    ## PC10 -2.470214e-15  3.046059e-16  6.777637e-16  1.105843e-16  1.000000e+00
    ## PC11  1.442420e-15 -2.611642e-16  3.060988e-16 -2.020831e-16  3.258713e-15
    ##               PC11
    ## PC1  -4.496936e-16
    ## PC2   3.953445e-16
    ## PC3  -3.322950e-16
    ## PC4  -8.205988e-16
    ## PC5   3.058063e-17
    ## PC6   1.442420e-15
    ## PC7  -2.611642e-16
    ## PC8   3.060988e-16
    ## PC9  -2.020831e-16
    ## PC10  3.258713e-15
    ## PC11  1.000000e+00

``` r
summary(pca)
```

    ## Importance of components:
    ##                           PC1    PC2    PC3    PC4     PC5     PC6     PC7
    ## Standard deviation     1.0216 0.9986 0.9963 0.8746 0.72157 0.62871 0.50349
    ## Proportion of Variance 0.1842 0.1760 0.1752 0.1350 0.09188 0.06976 0.04474
    ## Cumulative Proportion  0.1842 0.3602 0.5353 0.6703 0.76219 0.83194 0.87668
    ##                            PC8     PC9    PC10   PC11
    ## Standard deviation     0.49029 0.45564 0.41413 0.2816
    ## Proportion of Variance 0.04242 0.03664 0.03027 0.0140
    ## Cumulative Proportion  0.91910 0.95574 0.98600 1.0000

``` r
#qda_model <- qda(pca$x, df_scaled_1[, 9])
```

From the first output we can say that, when we take a specific
observasion, and multiply for each feature the corresponding value that
the first PCA column gives, and at the end add them all, we will get the
principal componet score for that specific observation.

Each row corresponds to a variable, and each column corresponds to a PC.
(when priting the loadings)

We can see now that the problem of multicollinearity is resolved. The
values in each cell represent the loading of that variable on that PC,
which can be interpreted as the correlation between the variable and the
PC.

For example, the first variable “CreditScore” has a loading of 0.013 on
PC1, indicating that it is weakly positively correlated with the first
principal component. However, it has a loading of -0.998 on PC5,
indicating that it is strongly negatively correlated with the fifth
principal component.

Similarly, the second variable “Age” has a loading of -0.110 on PC1,
indicating that it is weakly negatively correlated with the first
principal component, but it has a loading of -0.989 on PC4, indicating
that it is strongly negatively correlated with the fourth principal
component.

Lastly, when we print the correlation of the pc’s we can see that
multicollinearity is resolved.

``` r
# Create the plot
plot(pca, type = "l")
```

![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-73-1.png)<!-- -->

``` r
biplot(pca)
```

![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-73-2.png)<!-- -->

As we can see, the most important variables, that explain most of the
variance, are EstimatedSalary, Tenure, and NumOfProducts. Variables with
higher values on PC1 are positively correlated with higher tenure and
estimated salary, but not as strongly correlated with NumOfProducts.
Similarly, variables with higher values on PC2 are negatively correlated
with estimated salary and tenure, but positively correlated with
NumOfProducts.

``` r
df_scaled_1_pca <- cbind(df_scaled_1, pca$x[, 1:6])

df_scaled_1_pca$Exited <- as.factor(df_scaled_1_pca$Exited)

df_scaled_1_pca <- na.omit(df_scaled_1_pca)

library(ggplot2)

ggplot(df_scaled_1_pca, aes(x = PC1, y = PC2, color = factor(Exited))) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "PCA Plot",
       x = "PC1", y = "PC2", color = "Exited")
```

![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-74-1.png)<!-- -->

``` r
# while clustering the two groups, it can be clearly seen that clients who churned have a high value of pc1 and pc2
```

Overall, it seems that just the first 6 principal components would
explain a total of 83.2% of the variability. (The PC1 explains 18.42% of
the variance and PC2 explains 17.60% of the variance and so on). We
select as best 6 PC’s as common rule of thumb is to retain enough
components to explain at least 70-80% of the total variability in the
predictor variables

## 3.3.6.1 QDA - PCA with threshold 0.5

``` r
# Split data into train and test sets
set.seed(123)
df_pca <- df_scaled_1_pca[, c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "Exited")]
train_idx <- sample(nrow(df_pca), round(0.8 * nrow(df_pca)))
train_data <- df_pca[train_idx, ]
test_data <- df_pca[-train_idx, ]

# Define training control
ctrl <- trainControl(method = "cv", number = 5)

# Fit QDA model with cross-validation
qda_fit <- train(Exited ~ ., data = train_data, method = "qda", trControl = ctrl)

# Make predictions on test data
pred <- predict(qda_fit, newdata = test_data)

conf_mat <- confusionMatrix(pred, test_data$Exited)
conf_mat
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    0    1
    ##          0 1534  324
    ##          1   63   79
    ##                                           
    ##                Accuracy : 0.8065          
    ##                  95% CI : (0.7885, 0.8236)
    ##     No Information Rate : 0.7985          
    ##     P-Value [Acc > NIR] : 0.1942          
    ##                                           
    ##                   Kappa : 0.2066          
    ##                                           
    ##  Mcnemar's Test P-Value : <2e-16          
    ##                                           
    ##             Sensitivity : 0.9606          
    ##             Specificity : 0.1960          
    ##          Pos Pred Value : 0.8256          
    ##          Neg Pred Value : 0.5563          
    ##              Prevalence : 0.7985          
    ##          Detection Rate : 0.7670          
    ##    Detection Prevalence : 0.9290          
    ##       Balanced Accuracy : 0.5783          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

``` r
TP <- conf_mat$table[2, 2]
FN <- conf_mat$table[1, 2]
FP <- conf_mat$table[2, 1]
RECALL <- TP / (TP + FN)
PRECISION <- TP / (TP + FP)
cat("RECALL: ", round(RECALL, 2), "\nPRECISION: ", round(PRECISION, 2))
```

    ## RECALL:  0.2 
    ## PRECISION:  0.56

## 3.3.6.2 QDA - PCA with threshold 0.2

``` r
# Split data into train and test sets
set.seed(123)
df_pca <- df_scaled_1_pca[, c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "Exited")]
train_idx <- sample(nrow(df_pca), round(0.8 * nrow(df_pca)))
train_data <- df_pca[train_idx, ]
test_data <- df_pca[-train_idx, ]

# Define training control
ctrl <- trainControl(method = "cv", number = 5)

# Fit QDA model with cross-validation
qda_fit <- train(Exited ~ ., data = train_data, method = "qda", trControl = ctrl)

# Make predictions on test data with a threshold of 0.2
pred_probs <- predict(qda_fit, newdata = test_data, type = "prob")
pred_labels <- ifelse(pred_probs[, "1"] >= 0.2, "1", "0")
pred_labels <- factor(pred_labels, levels = c("0", "1"))
test_labels <- factor(test_data$Exited, levels = c("0", "1"))
conf_mat <- confusionMatrix(pred_labels, test_labels)

# Calculate recall and precision
conf_mat
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    0    1
    ##          0 1186  117
    ##          1  411  286
    ##                                           
    ##                Accuracy : 0.736           
    ##                  95% CI : (0.7161, 0.7552)
    ##     No Information Rate : 0.7985          
    ##     P-Value [Acc > NIR] : 1               
    ##                                           
    ##                   Kappa : 0.3554          
    ##                                           
    ##  Mcnemar's Test P-Value : <2e-16          
    ##                                           
    ##             Sensitivity : 0.7426          
    ##             Specificity : 0.7097          
    ##          Pos Pred Value : 0.9102          
    ##          Neg Pred Value : 0.4103          
    ##              Prevalence : 0.7985          
    ##          Detection Rate : 0.5930          
    ##    Detection Prevalence : 0.6515          
    ##       Balanced Accuracy : 0.7262          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

``` r
TP <- conf_mat$table[2, 2]
FN <- conf_mat$table[1, 2]
FP <- conf_mat$table[2, 1]
RECALL <- TP / (TP + FN)
PRECISION <- TP / (TP + FP)
cat("RECALL: ", round(RECALL, 2), "\nPRECISION: ", round(PRECISION, 2))
```

    ## RECALL:  0.71 
    ## PRECISION:  0.41

When the threshold is set to 0.2, we get better recall results!

## 3.4. KNN

## 3.4.1 KNN, Split into Train and Test, Evaluate Performance

Split the data into train.X train.y test.X and test.y

``` r
X <- df_scaled_1[, -9]
y <- df_scaled_1[, 9]

set.seed(42)# Set the random seed for reproducibility

train_idx <- createDataPartition(y, p = 0.8, list = FALSE)
train.X <- X[train_idx, ]
train.y <- y[train_idx]
test.X <- X[-train_idx, ]
test.y <- y[-train_idx]
train_subset <- cbind(train.X, train.y)
test_subset <- cbind(test.X, test.y)
```

``` r
set.seed(1234)
knn.pred <- knn(train.X, test.X, train.y, k = 3)
table(knn.pred, test.y)
```

    ##         test.y
    ## knn.pred    0    1
    ##        0 1481  248
    ##        1  101  170

## 3.4.2 Cross Validation on Searching the Best k that Maximizes the Recall Rate

As our goal is to maximize the recall rate, we perform a cros validation
of 5, for each k that ranges from 1 to 20, and calculate the average
recall rate for each. Then the output refers to the k that maximizes the
Recall rate

``` r
set.seed(1234)

X <- df_scaled_1[, -which(names(df_scaled_1) == "Exited")] # Predictor variables
y <- as.factor(df_scaled_1$Exited) # Response variable

# Define the range of k values to test
k_range <- 1:20

# Define the number of splits for k-fold cross-validation
k_fold <- 5

# Create a list to store cross-validation results
cv_results <- list()

# Perform k-fold cross-validation for each k value in the range
for (k in k_range) {
  # Define the k-fold cross-validation procedure
  folds <- createFolds(y, k = k_fold)

  # Create a list to store recall scores for each fold
  recall_scores <- c()

  # Iterate over the folds and calculate the recall score for each one
  for (i in 1:k_fold) {
    # Split the data into training and test sets for this fold
    test_idx <- folds[[i]]
    train_idx <- setdiff(1:length(y), test_idx)
    X_train <- X[train_idx, ]
    y_train <- y[train_idx]
    X_test <- X[test_idx, ]
    y_test <- y[test_idx]

    # Train a KNN model with the current k value using the training set
    model <- train(X_train, y_train, method = "knn", trControl = trainControl(method = "none"), tuneGrid = data.frame(k = k))

    # Make predictions on the test set and calculate recall score
    pred <- predict(model, X_test)
    cm <- confusionMatrix(pred, y_test, positive = "1")
    recall_score <- cm$byClass[["Recall"]]
    recall_scores <- c(recall_scores, recall_score)
  }

  # Store the average recall score for this k value
  cv_results[[k]] <- mean(recall_scores)
}

# Find the k value with the highest average recall score
optimal_k <- k_range[which.max(unlist(cv_results))]

cat("Optimal k value:", optimal_k, "\n")
```

    ## Optimal k value: 1

``` r
cv_results
```

    ## [[1]]
    ## [1] 0.4791625
    ## 
    ## [[2]]
    ## [1] 0.4697921
    ## 
    ## [[3]]
    ## [1] 0.4393831
    ## 
    ## [[4]]
    ## [1] 0.438381
    ## 
    ## [[5]]
    ## [1] 0.4315014
    ## 
    ## [[6]]
    ## [1] 0.4280833
    ## 
    ## [[7]]
    ## [1] 0.422207
    ## 
    ## [[8]]
    ## [1] 0.4148348
    ## 
    ## [[9]]
    ## [1] 0.4074565
    ## 
    ## [[10]]
    ## [1] 0.4025497
    ## 
    ## [[11]]
    ## [1] 0.4084417
    ## 
    ## [[12]]
    ## [1] 0.4064858
    ## 
    ## [[13]]
    ## [1] 0.4035398
    ## 
    ## [[14]]
    ## [1] 0.4005757
    ## 
    ## [[15]]
    ## [1] 0.3927278
    ## 
    ## [[16]]
    ## [1] 0.3966638
    ## 
    ## [[17]]
    ## [1] 0.3981211
    ## 
    ## [[18]]
    ## [1] 0.3878065
    ## 
    ## [[19]]
    ## [1] 0.3804608
    ## 
    ## [[20]]
    ## [1] 0.385382

The reason the cross validation gives as the best k to be equal to 1, is
that because the minority of class is the positive instances, clients
who churned. This is because with a lower value of k, the algorithm is
more likely to select neighbors that belong to the positive class, which
can improve the recall for the positive class. Although, we will choose
to select the k to be equal to 2, so that we do not overfit the model.

We figured that it would be a waste of time to try and fit a KNN model,
at first on the whole set, after divide in train and test, and etc. So,
we decided to perform KNN, with cross validation equal to 5, and test
the accuracy of the 5 models on different subsets, so called validation
sets. Before moving into the procedure as explained above, it is a must
to scale the data first. Variables that are on a large scale will have a
much larger effect on the distance between the observations, and hence
on the KNN classifier, than variables that are on a small scale. It is
good to mention that we use K equal to 2(each new observation, the
algorithm will identify the 2 nearest neighbors (in terms of distance)
from the training set and assign the class that is most common among
those 2 neighbor to the new observation.)

## 3.4.3. KNN - Cross Validation with k = 2

``` r
set.seed(1234)
X <- df_scaled_1[ , -which(names(df_scaled_1) == "Exited")] # Predictor variables
y <- as.factor(df_scaled_1$Exited)# Response variable

k <- 5 # Define the number of splits for k-fold cross-validation
folds <- createFolds(y, k=k)

conf_matrices <- list() # store confusion matrices
predicted_all <- c() # store predicted y's of test set, of all folds 
actual_all <- c() # store their actual y's of test set, of all folds

# Use cross-validation to evaluate k-NN's performance and calculate confusion matrices, with the help of the created lists
for(i in 1:k){
  test_idx <- folds[[i]] 
  train_idx <- setdiff(1:length(y), test_idx)
  
  # Fit k-NN model using the training set, with k=2
  model <- train(x=X[train_idx, ], y=y[train_idx], method="knn", trControl=trainControl(method="cv", number=5), tuneGrid = data.frame(k = 2))
  
  # Make predictions on the test set
  predicted <- predict(model, newdata=X[test_idx, ])
  
  # Calculate confusion matrix for this fold and store it in the list
  conf_matrix <- table(predicted, y[test_idx])
  conf_matrices[[i]] <- conf_matrix
  
  # Store predicted and actual values for this fold
  predicted_all <- c(predicted_all, predicted)
  actual_all <- c(actual_all, y[test_idx])
  
  # Print the confusion matrix for this fold
  cat("Confusion matrix for fold ", i, ":\n")
  print(conf_matrix)
  
  # Print the number of observations in test set for this fold
  cat("Number of observations in test set for fold ", i, ":", length(test_idx), "\n")
}
```

    ## Confusion matrix for fold  1 :
    ##          
    ## predicted    0    1
    ##         0 1370  223
    ##         1  222  185
    ## Number of observations in test set for fold  1 : 2000 
    ## Confusion matrix for fold  2 :
    ##          
    ## predicted    0    1
    ##         0 1399  207
    ##         1  194  200
    ## Number of observations in test set for fold  2 : 2000 
    ## Confusion matrix for fold  3 :
    ##          
    ## predicted    0    1
    ##         0 1398  201
    ##         1  194  206
    ## Number of observations in test set for fold  3 : 1999 
    ## Confusion matrix for fold  4 :
    ##          
    ## predicted    0    1
    ##         0 1395  223
    ##         1  198  185
    ## Number of observations in test set for fold  4 : 2001 
    ## Confusion matrix for fold  5 :
    ##          
    ## predicted    0    1
    ##         0 1401  229
    ##         1  192  178
    ## Number of observations in test set for fold  5 : 2000

``` r
accuracy <- mean(predicted_all == actual_all) # Calculate the overall accuracy

cat("Cross-validation accuracy:", round(accuracy, 3), "\n")
```

    ## Cross-validation accuracy: 0.792

``` r
TP <- 185
FP <- 222
FN <- 223
RECALL <- TP/(TP+FN)
PRECISION <- TP/(TP+FP)

TP1 <- 200
FP1 <- 194
FN1 <- 207
RECALL1 <- TP1/(TP1+FN1)
PRECISION1 <- TP1/(TP1+FP1)

TP2 <- 206
FP2 <- 194
FN2 <- 201
RECALL2 <- TP2/(TP2+FN2)
PRECISION2 <- TP2/(TP2+FP2)

TP3 <- 185
FP3 <- 198
FN3 <- 223
RECALL3 <- TP3/(TP3+FN3)
PRECISION3 <- TP3/(TP3+FP3)

TP4 <- 178
FP4 <- 192
FN4 <- 229
RECALL4 <- TP4/(TP4+FN4)
PRECISION4 <- TP4/(TP4+FP4)

cat("Average Recall:", (RECALL + RECALL1 + RECALL2 + RECALL3 + RECALL4)/5)
```

    ## Average Recall: 0.4683504

``` r
cat("\nAverage Precision:", (PRECISION + PRECISION1 + PRECISION2 + PRECISION3 + PRECISION4)/5)
```

    ## 
    ## Average Precision: 0.4882539

## 3.4.4. KNN - Cross Validation with k = 2, PCA

``` r
set.seed(123)
df_pca <- df_scaled_1_pca[, c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "Exited")]
train_idx <- sample(nrow(df_pca), round(0.8 * nrow(df_pca)))
train_data <- df_pca[train_idx, ]
test_data <- df_pca[-train_idx, ]

# Define training control
ctrl <- trainControl(method = "cv", number = 5)

# Fit knn model with 1 neighbor and cross-validation
knn_fit <- train(Exited ~ ., data = train_data, method = "knn", trControl = ctrl, tuneGrid = data.frame(k = 2))

# Make predictions on test data
pred <- predict(knn_fit, newdata = test_data)

conf_mat <- confusionMatrix(pred, test_data$Exited)
conf_mat
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    0    1
    ##          0 1371  207
    ##          1  226  196
    ##                                           
    ##                Accuracy : 0.7835          
    ##                  95% CI : (0.7648, 0.8014)
    ##     No Information Rate : 0.7985          
    ##     P-Value [Acc > NIR] : 0.9545          
    ##                                           
    ##                   Kappa : 0.3389          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.3870          
    ##                                           
    ##             Sensitivity : 0.8585          
    ##             Specificity : 0.4864          
    ##          Pos Pred Value : 0.8688          
    ##          Neg Pred Value : 0.4645          
    ##              Prevalence : 0.7985          
    ##          Detection Rate : 0.6855          
    ##    Detection Prevalence : 0.7890          
    ##       Balanced Accuracy : 0.6724          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

``` r
TP <- conf_mat$table[2, 2]
FN <- conf_mat$table[1, 2]
FP <- conf_mat$table[2, 1]
RECALL <- TP / (TP + FN)
PRECISION <- TP / (TP + FP)
cat("RECALL: ", round(RECALL, 2), "\nPRECISION: ", round(PRECISION, 2))
```

    ## RECALL:  0.49 
    ## PRECISION:  0.46

## 3.4.4. KNN - Cross Validation with k = 2, Oversampled Data

``` r
set.seed(1234)
X <- df_sampled_scaled[ , -which(names(df_sampled_scaled) == "Exited")] # Predictor variables
y <- as.factor(df_sampled_scaled$Exited)# Response variable

k <- 5 # Define the number of splits for k-fold cross-validation
folds <- createFolds(y, k=k)

conf_matrices <- list() # store confusion matrices
predicted_all <- c() # store predicted y's of test set, of all folds 
actual_all <- c() # store their actual y's of test set, of all folds

# Use cross-validation to evaluate k-NN's performance and calculate confusion matrices, with the help of the created lists
for(i in 1:k){
  test_idx <- folds[[i]] 
  train_idx <- setdiff(1:length(y), test_idx)
  
  # Fit k-NN model using the training set, with k=2
  model <- train(x=X[train_idx, ], y=y[train_idx], method="knn", trControl=trainControl(method="cv", number=5), tuneGrid = data.frame(k = 2))
  
  # Make predictions on the test set
  predicted <- predict(model, newdata=X[test_idx, ])
  
  # Calculate confusion matrix for this fold and store it in the list
  conf_matrix <- table(predicted, y[test_idx])
  conf_matrices[[i]] <- conf_matrix
  
  # Store predicted and actual values for this fold
  predicted_all <- c(predicted_all, predicted)
  actual_all <- c(actual_all, y[test_idx])
  
  # Print the confusion matrix for this fold
  cat("Confusion matrix for fold ", i, ":\n")
  print(conf_matrix)
  
  # Print the number of observations in test set for this fold
  cat("Number of observations in test set for fold ", i, ":", length(test_idx), "\n")
}
```

    ## Confusion matrix for fold  1 :
    ##          
    ## predicted    0    1
    ##         0 1224   85
    ##         1  368 1517
    ## Number of observations in test set for fold  1 : 3194 
    ## Confusion matrix for fold  2 :
    ##          
    ## predicted    0    1
    ##         0 1262   73
    ##         1  331 1528
    ## Number of observations in test set for fold  2 : 3194 
    ## Confusion matrix for fold  3 :
    ##          
    ## predicted    0    1
    ##         0 1249   83
    ##         1  343 1518
    ## Number of observations in test set for fold  3 : 3193 
    ## Confusion matrix for fold  4 :
    ##          
    ## predicted    0    1
    ##         0 1240   92
    ##         1  353 1509
    ## Number of observations in test set for fold  4 : 3194 
    ## Confusion matrix for fold  5 :
    ##          
    ## predicted    0    1
    ##         0 1243   66
    ##         1  350 1535
    ## Number of observations in test set for fold  5 : 3194

``` r
accuracy <- mean(predicted_all == actual_all) # Calculate the overall accuracy

cat("Cross-validation accuracy:", round(accuracy, 3), "\n")
```

    ## Cross-validation accuracy: 0.866

``` r
TP <- 1536
FP <- 370
FN <- 64
RECALL <- TP/(TP+FN)
PRECISION <- TP/(TP+FP)

TP1 <- 1524
FP1 <- 333
FN1 <- 75
RECALL1 <- TP1/(TP1+FN1)
PRECISION1 <- TP1/(TP1+FP1)

TP2 <- 1521
FP2 <- 347
FN2 <- 79
RECALL2 <- TP2/(TP2+FN2)
PRECISION2 <- TP2/(TP2+FP2)

TP3 <- 1524
FP3 <- 377
FN3 <- 76
RECALL3 <- TP3/(TP3+FN3)
PRECISION3 <- TP3/(TP3+FP3)

TP4 <- 1522
FP4 <- 349
FN4 <- 78
RECALL4 <- TP4/(TP4+FN4)
PRECISION4 <- TP4/(TP4+FP4)

cat("Average Recall:", (RECALL + RECALL1 + RECALL2 + RECALL3 + RECALL4)/5)
```

    ## Average Recall: 0.9534941

``` r
cat("\nAverage Precision:", (PRECISION + PRECISION1 + PRECISION2 + PRECISION3 + PRECISION4)/5)
```

    ## 
    ## Average Precision: 0.8111893

## 3.5. LDA

\#Regular Dataset

``` r
#Runing on the regular datasetinstall.packages('caret')
library('caret')
trainIndex <- createDataPartition(df$Exited, p = 0.8, list = FALSE)

# Split the data into training and test sets (80% training, 20% test)
train_data_subset <- df[trainIndex, ]
test_data_subset <- df[-trainIndex, ]
```

``` r
#Simple LDA
library("MASS")
linear <- lda(Exited~., train_data_subset)
linear
```

    ## Call:
    ## lda(Exited ~ ., data = train_data_subset)
    ## 
    ## Prior probabilities of groups:
    ##       0       1 
    ## 0.79425 0.20575 
    ## 
    ## Group means:
    ##   CreditScore      Age   Tenure  Balance NumOfProducts HasCrCard IsActiveMember
    ## 0    651.9032 37.43720 5.021089 72749.51      1.543752 0.7042808      0.5564998
    ## 1    646.2248 45.05286 4.905225 91289.13      1.489064 0.6968408      0.3572296
    ##   EstimatedSalary    France   Germany    Female
    ## 0        99800.92 0.5280138 0.2110482 0.4345294
    ## 1       102160.24 0.3955043 0.4003645 0.5522479
    ## 
    ## Coefficients of linear discriminants:
    ##                           LD1
    ## CreditScore     -6.524101e-04
    ## Age              7.755973e-02
    ## Tenure          -1.421372e-02
    ## Balance          2.317021e-06
    ## NumOfProducts   -5.456985e-02
    ## HasCrCard       -2.428636e-02
    ## IsActiveMember  -9.961363e-01
    ## EstimatedSalary  7.753644e-07
    ## France          -2.690423e-02
    ## Germany          8.424505e-01
    ## Female           4.606446e-01

``` r
p2 <- predict(linear, test_data_subset)$class
tab1 <- table(Predicted = p2, Actual = test_data_subset$Exited)
tab1
```

    ##          Actual
    ## Predicted    0    1
    ##         0 1548  305
    ##         1   61   86

``` r
Accuracy <- sum(diag(tab1))/sum(tab1)
Accuracy
```

    ## [1] 0.817

``` r
par(mar = c(4, 4, 2, 2))
plot(linear)
```

![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-88-1.png)<!-- -->

Oversampled

``` r
trainIndex <- createDataPartition(oversampled_churn_data$Exited, p = 0.8, list = FALSE)

# Split the data into training and test sets (80% training, 20% test)
train_data_subset <- df[trainIndex, ]
test_data_subset <- df[-trainIndex, ]
```

``` r
#Simple LDA
linear <- lda(Exited~., train_data_subset)
linear
```

    ## Call:
    ## lda(Exited ~ ., data = train_data_subset)
    ## 
    ## Prior probabilities of groups:
    ##         0         1 
    ## 0.7981227 0.2018773 
    ## 
    ## Group means:
    ##   CreditScore      Age   Tenure  Balance NumOfProducts HasCrCard IsActiveMember
    ## 0    652.2156 37.39595 5.041556 72554.91      1.540066 0.7076995      0.5544927
    ## 1    645.5096 44.79913 4.923745 90290.13      1.483571 0.7129572      0.3595784
    ##   EstimatedSalary    France   Germany    Female
    ## 0        99643.29 0.5315979 0.2135801 0.4324918
    ## 1       101151.14 0.4060756 0.3955363 0.5616863
    ## 
    ## Coefficients of linear discriminants:
    ##                           LD1
    ## CreditScore     -6.360054e-04
    ## Age              7.784052e-02
    ## Tenure          -1.382849e-02
    ## Balance          2.219508e-06
    ## NumOfProducts   -6.757598e-02
    ## HasCrCard        2.769733e-02
    ## IsActiveMember  -1.002990e+00
    ## EstimatedSalary  4.961667e-07
    ## France           5.644357e-03
    ## Germany          8.404975e-01
    ## Female           5.177171e-01

**If** you have only LD1, it means that there is only one significant
discriminant function that separates the classes well. This could be due
to various reasons such as low sample size, low variability in the
predictor variables, or a high degree of overlap between the classes. We
invesitage further

MOdelling with 5 Folds CV/Scaling/Centering

Oversampled Data

``` r
set.seed(1)
#Set Level using code below (incase it returns error "type is regression")
oversampled_churn_data$Exited <- factor(oversampled_churn_data$Exited, levels = c("0", "1"))
levels(oversampled_churn_data$Exited) <- make.names(levels(oversampled_churn_data$Exited))

# Define the independent variables
predictors <- c("CreditScore",      "Age",      "Tenure",   "Balance",  "NumOfProducts",  "HasCrCard",      "IsActiveMember", "EstimatedSalary","France",   "Germany",  "Female")

# Define the target variable
target <- "Exited"

# Define the number of folds for cross-validation
k <- 5

#Making Test Data
test_index <- createDataPartition(oversampled_churn_data$Exited, p = 0.2, list = FALSE)
test_data <- oversampled_churn_data[test_index, ]
train_data <- oversampled_churn_data[-test_index, ]

# Create the training control object
train_control <- trainControl(method = "cv", number = k)

# Train the LDA model with cross-validation
lda_model <- train(as.formula(paste(target, "~", paste(predictors, collapse = "+"))),
                   data = train_data,
                   preProcess = c("center", "scale"),
                   method = "lda",
                   trControl = train_control)

# Print the model results
print(lda_model)
```

    ## Linear Discriminant Analysis 
    ## 
    ## 12774 samples
    ##    11 predictor
    ##     2 classes: 'X0', 'X1' 
    ## 
    ## Pre-processing: centered (11), scaled (11) 
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 10219, 10219, 10219, 10220, 10219 
    ## Resampling results:
    ## 
    ##   Accuracy   Kappa    
    ##   0.7099567  0.4199357

``` r
# Make predictions on the test set
predictions <- predict(lda_model, newdata = test_data, type="raw")


# Calculate the accuracy
accuracy <- confusionMatrix(predictions, test_data$Exited)
print(accuracy)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   X0   X1
    ##         X0 1160  482
    ##         X1  433 1120
    ##                                           
    ##                Accuracy : 0.7136          
    ##                  95% CI : (0.6976, 0.7292)
    ##     No Information Rate : 0.5014          
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.4273          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.1126          
    ##                                           
    ##             Sensitivity : 0.7282          
    ##             Specificity : 0.6991          
    ##          Pos Pred Value : 0.7065          
    ##          Neg Pred Value : 0.7212          
    ##              Prevalence : 0.4986          
    ##          Detection Rate : 0.3631          
    ##    Detection Prevalence : 0.5139          
    ##       Balanced Accuracy : 0.7137          
    ##                                           
    ##        'Positive' Class : X0              
    ## 

``` r
#print(paste("Accuracy:", accuracy))
```

``` r
#Plot ROC
predictions <- predict(lda_model, newdata = test_data, type="prob")
library(pROC)
roc_obj <- roc(test_data$Exited, predictions$X1)
```

    ## Setting levels: control = X0, case = X1

    ## Setting direction: controls < cases

``` r
# Get the optimal operating point
opt_point <- coords(roc_obj, "best", ret=c("threshold", "specificity", "sensitivity"))


# Plot the ROC curve with the optimal point
plot(roc_obj, col = "darkblue", main = "ROC Curve", print.thres = c(0.1, 0.2, 0.3, 0.4, 0.5,0.6,0.7,0.8,0.9), print.auc = TRUE)
points(opt_point$specificity, opt_point$sensitivity, pch = 19, col = "red")
text(opt_point$specificity , opt_point$sensitivity ,
     paste0("Threshold = ", round(opt_point$threshold, 2),
            "\nSensitivity = ", round(opt_point$sensitivity, 2),
            "\nSpecificity = ", round(opt_point$specificity, 2)),
     pos = 2)
```

![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-92-1.png)<!-- -->
Regular Df

``` r
#Set Level

df$Exited <- factor(df$Exited, levels = c("0", "1"))
levels(df$Exited) <- make.names(levels(df$Exited))

# Define the independent variables
predictors <- c("CreditScore",      "Age",      "Tenure",   "Balance",  "NumOfProducts",  "HasCrCard",      "IsActiveMember", "EstimatedSalary","France",   "Germany",  "Female")

# Define the target variable
target <- "Exited"

# Define the number of folds for cross-validation
k <- 5

#Making Test Data
test_index <- createDataPartition(df$Exited, p = 0.3, list = FALSE)
test_data <- df[test_index, ]
train_data <- df[-test_index,]


# Create the training control object
train_control <- trainControl(method = "cv", number = k)

# Train the LDA model with cross-validation
lda_model <- train(as.formula(paste(target, "~", paste(predictors, collapse = "+"))),
                   data = train_data,
                   preProcess = c("center", "scale"),
                   method = "lda",
                   trControl = train_control)

# Print the model results
print(lda_model)
```

    ## Linear Discriminant Analysis 
    ## 
    ## 6999 samples
    ##   11 predictor
    ##    2 classes: 'X0', 'X1' 
    ## 
    ## Pre-processing: centered (11), scaled (11) 
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 5599, 5600, 5599, 5599, 5599 
    ## Resampling results:
    ## 
    ##   Accuracy   Kappa    
    ##   0.8079716  0.2347337

``` r
# Make predictions on the test set
predictions <- predict(lda_model, newdata = test_data, type = "raw")


# Calculate the accuracy
accuracy <- confusionMatrix(predictions, test_data$Exited)
print(accuracy)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   X0   X1
    ##         X0 2285  463
    ##         X1  104  149
    ##                                           
    ##                Accuracy : 0.8111          
    ##                  95% CI : (0.7966, 0.8249)
    ##     No Information Rate : 0.7961          
    ##     P-Value [Acc > NIR] : 0.02116         
    ##                                           
    ##                   Kappa : 0.2557          
    ##                                           
    ##  Mcnemar's Test P-Value : < 2e-16         
    ##                                           
    ##             Sensitivity : 0.9565          
    ##             Specificity : 0.2435          
    ##          Pos Pred Value : 0.8315          
    ##          Neg Pred Value : 0.5889          
    ##              Prevalence : 0.7961          
    ##          Detection Rate : 0.7614          
    ##    Detection Prevalence : 0.9157          
    ##       Balanced Accuracy : 0.6000          
    ##                                           
    ##        'Positive' Class : X0              
    ## 

``` r
#print(paste("Accuracy:", accuracy))
```

``` r
predictions <- predict(lda_model, newdata = test_data, type = "prob")

# Calculate ROC curve and AUC
# Calculate the ROC curve
library(pROC)
roc_obj <- roc(test_data$Exited, predictions$X1)
```

    ## Setting levels: control = X0, case = X1

    ## Setting direction: controls < cases

``` r
# Get the optimal operating point
opt_point <- coords(roc_obj, "best", ret=c("threshold", "specificity", "sensitivity"))


# Plot the ROC curve with the optimal point
plot(roc_obj, col = "darkblue", main = "ROC Curve", print.thres = c(0.1, 0.2, 0.3, 0.4, 0.5,0.6,0.7,0.8,0.9), print.auc = TRUE)
points(opt_point$specificity, opt_point$sensitivity, pch = 19, col = "red")
text(opt_point$specificity , opt_point$sensitivity ,
     paste0("Threshold = ", round(opt_point$threshold, 2),
            "\nSensitivity = ", round(opt_point$sensitivity, 2),
            "\nSpecificity = ", round(opt_point$specificity, 2)),
     pos = 2)
```

![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-94-1.png)<!-- -->
SO we can clearly see comparing the two AUC curves, oversampled data is
giving better performance, as the dataset is more balanced LDA seems to
do slightly better job at making classifications while also keeping in
mind the increase of specificity.

Final Remarks: We have higher specificty but overall balanced accuray
increases,

Balanced accuracy = (Sensitivity + Specificity) / 2

where:

Sensitivity: The “true positive rate” – the percentage of positive cases
the model is able to detect. Specificity: The “true negative rate” – the
percentage of negative cases the model is able to detect.

Accuracy is the proportion of correct predictions among all predictions,
regardless of the class distribution. It is a popular metric for
balanced datasets where the number of positive and negative samples is
roughly the same.

However, in imbalanced datasets, where one class is much more prevalent
than the other, accuracy can be misleading. A model that always predicts
the majority class can achieve a high accuracy score but does not
perform well in identifying the minority class. In this case, balanced
accuracy, also known as the average of sensitivity and specificity, is a
better metric.

Balanced accuracy takes into account both the true positive rate
(sensitivity) and the true negative rate (specificity) and is less
affected by imbalanced datasets. It is a better metric for evaluating
models in situations where the cost of false negatives (missing positive
cases) and false positives (wrongly identifying negative cases) is not
equal.

Optimal point on the ROC curve indicates the higest sensitivty while
maintining the highest specificity.

## LDA with Principal Component

Oversampled Data

``` r
#Perform PCA
# Select all columns except 'Exited'
pca_input <- oversampled_churn_data[, -which(names(oversampled_churn_data) == "Exited")]

# Perform PCA
pca_output <- prcomp(pca_input, scale = TRUE)

# Add 'Exited' variable back to the PCA output data
df_pca <- data.frame(pca_output$x, Exited = oversampled_churn_data$Exited)
```

``` r
# Compute the proportion of variance explained by each principal component
pca_var <- pca_output$sdev^2 / sum(pca_output$sdev^2)

# Create a dataframe with the proportion of variance and cumulative proportion for each principal component
pca_var_df <- data.frame(Proportion = pca_var, CumulativeProportion = cumsum(pca_var))

# Create an elbow plot
library(ggplot2)
ggplot(data = pca_var_df, aes(x = 1:length(pca_var), y = Proportion)) +
  geom_point(size = 3, color = "blue") +
  geom_line(color = "blue") +
  xlab("Principal Component") +
  ylab("Proportion of Variance") +
  ggtitle("Elbow Plot for PCA") +
  theme_bw()
```

![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-96-1.png)<!-- -->

We choose 9 Principal Components

``` r
set.seed(1)
# Define the independent variables
predictors <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9")#, "PC10")

# Define the target variable
target <- "Exited"

# Define the number of folds for cross-validation
k <- 5

#Making Test Data
test_index <- createDataPartition(df_pca$Exited, p = 0.2, list = FALSE)
test_data <- df_pca[test_index, ]
train_data <- df_pca[-test_index, ]

# Create the training control object
train_control <- trainControl(method = "cv", number = k)

# Train the LDA model with cross-validation
lda_model <- train(as.formula(paste(target, "~", paste(predictors, collapse = "+"))),
                   data = train_data,
                   method = "lda",
                   trControl = train_control)

# Print the model results
print(lda_model)
```

    ## Linear Discriminant Analysis 
    ## 
    ## 12774 samples
    ##     9 predictor
    ##     2 classes: 'X0', 'X1' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 10219, 10219, 10219, 10220, 10219 
    ## Resampling results:
    ## 
    ##   Accuracy  Kappa    
    ##   0.70753   0.4150909

``` r
# Make predictions on the test set
predictions <- predict(lda_model, newdata = test_data, type="raw")


# Calculate the accuracy
accuracy <- confusionMatrix(predictions, test_data$Exited)
print(accuracy)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   X0   X1
    ##         X0 1153  478
    ##         X1  440 1124
    ##                                           
    ##                Accuracy : 0.7127          
    ##                  95% CI : (0.6966, 0.7283)
    ##     No Information Rate : 0.5014          
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.4254          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.222           
    ##                                           
    ##             Sensitivity : 0.7238          
    ##             Specificity : 0.7016          
    ##          Pos Pred Value : 0.7069          
    ##          Neg Pred Value : 0.7187          
    ##              Prevalence : 0.4986          
    ##          Detection Rate : 0.3609          
    ##    Detection Prevalence : 0.5105          
    ##       Balanced Accuracy : 0.7127          
    ##                                           
    ##        'Positive' Class : X0              
    ## 

``` r
#print(paste("Accuracy:", accuracy))
```

``` r
# Make predictions on the test set
predictions <- predict(lda_model, newdata = test_data, type="prob")
# Calculate ROC curve and AUC
# Calculate the ROC curve
library(pROC)
roc_obj <- roc(test_data$Exited, predictions$X1)
```

    ## Setting levels: control = X0, case = X1

    ## Setting direction: controls < cases

``` r
# Get the optimal operating point
opt_point <- coords(roc_obj, "best", ret=c("threshold", "specificity", "sensitivity"))


# Plot the ROC curve with the optimal point
plot(roc_obj, col = "darkblue", main = "ROC Curve", print.thres = c(0.1, 0.2, 0.3, 0.4, 0.5,0.6,0.7,0.8,0.9), print.auc = TRUE)
points(opt_point$specificity, opt_point$sensitivity, pch = 19, col = "red")
text(opt_point$specificity , opt_point$sensitivity ,
     paste0("Threshold = ", round(opt_point$threshold, 2),
            "\nSensitivity = ", round(opt_point$sensitivity, 2),
            "\nSpecificity = ", round(opt_point$specificity, 2)),
     pos = 2)
```

![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-98-1.png)<!-- -->

Regular Df

``` r
#Perform PCA


# Select all columns except 'Exited'
pca_input <- df[, -which(names(df) == "Exited")]

# Perform PCA
pca_output <- prcomp(pca_input, scale = TRUE)

# Add 'Exited' variable back to the PCA output data
df_pca <- data.frame(pca_output$x, Exited = df$Exited)
```

``` r
# Compute the proportion of variance explained by each principal component
pca_var <- pca_output$sdev^2 / sum(pca_output$sdev^2)

# Create a dataframe with the proportion of variance and cumulative proportion for each principal component
pca_var_df <- data.frame(Proportion = pca_var, CumulativeProportion = cumsum(pca_var))

# Create an elbow plot
library(ggplot2)
ggplot(data = pca_var_df, aes(x = 1:length(pca_var), y = Proportion)) +
  geom_point(size = 3, color = "blue") +
  geom_line(color = "blue") +
  xlab("Principal Component") +
  ylab("Proportion of Variance") +
  ggtitle("Elbow Plot for PCA") +
  theme_bw()
```

![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-100-1.png)<!-- -->

``` r
#df_pca$Exited <- factor(df_pca$Exited, levels = c("0", "1"))
#levels(df_pca$Exited) <- make.names(levels(df_pca$Exited))
# Define the independent variables
predictors <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9")#, "PC10")

# Define the target variable
target <- "Exited"

# Define the number of folds for cross-validation
k <- 5

#Making Test Data
test_index <- createDataPartition(df_pca$Exited, p = 0.2, list = FALSE)
test_data <- df_pca[test_index, ]
train_data <- df_pca[-test_index, ]

# Create the training control object
train_control <- trainControl(method = "cv", number = k)

# Train the LDA model with cross-validation
lda_model <- train(as.formula(paste(target, "~", paste(predictors, collapse = "+"))),
                   data = train_data,
                   method = "lda",
                   trControl = train_control)

# Print the model results
print(lda_model)
```

    ## Linear Discriminant Analysis 
    ## 
    ## 7999 samples
    ##    9 predictor
    ##    2 classes: 'X0', 'X1' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 6399, 6400, 6399, 6399, 6399 
    ## Resampling results:
    ## 
    ##   Accuracy   Kappa    
    ##   0.8086016  0.2415417

``` r
# Make predictions on the test set
predictions <- predict(lda_model, newdata = test_data, type="raw")


# Calculate the accuracy
accuracy <- confusionMatrix(predictions, test_data$Exited)
print(accuracy)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   X0   X1
    ##         X0 1521  318
    ##         X1   72   90
    ##                                          
    ##                Accuracy : 0.8051         
    ##                  95% CI : (0.787, 0.8222)
    ##     No Information Rate : 0.7961         
    ##     P-Value [Acc > NIR] : 0.1658         
    ##                                          
    ##                   Kappa : 0.2261         
    ##                                          
    ##  Mcnemar's Test P-Value : <2e-16         
    ##                                          
    ##             Sensitivity : 0.9548         
    ##             Specificity : 0.2206         
    ##          Pos Pred Value : 0.8271         
    ##          Neg Pred Value : 0.5556         
    ##              Prevalence : 0.7961         
    ##          Detection Rate : 0.7601         
    ##    Detection Prevalence : 0.9190         
    ##       Balanced Accuracy : 0.5877         
    ##                                          
    ##        'Positive' Class : X0             
    ## 

``` r
#print(paste("Accuracy:", accuracy))
```

``` r
# Make predictions on the test set
predictions <- predict(lda_model, newdata = test_data, type="prob")
# Calculate ROC curve and AUC
# Calculate the ROC curve
library(pROC)
roc_obj <- roc(test_data$Exited, predictions$X1)
```

    ## Setting levels: control = X0, case = X1

    ## Setting direction: controls < cases

``` r
# Get the optimal operating point
opt_point <- coords(roc_obj, "best", ret=c("threshold", "specificity", "sensitivity"))


# Plot the ROC curve with the optimal point
plot(roc_obj, col = "darkblue", main = "ROC Curve", print.thres = c(0.1, 0.2, 0.3, 0.4, 0.5,0.6,0.7,0.8,0.9), print.auc = TRUE)
points(opt_point$specificity, opt_point$sensitivity, pch = 19, col = "red")
text(opt_point$specificity , opt_point$sensitivity ,
     paste0("Threshold = ", round(opt_point$threshold, 2),
            "\nSensitivity = ", round(opt_point$sensitivity, 2),
            "\nSpecificity = ", round(opt_point$specificity, 2)),
     pos = 2)
```

![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-102-1.png)<!-- -->

# RANDOM FOREST

``` r
library("randomForest")
```

    ## Warning: package 'randomForest' was built under R version 4.2.3

    ## randomForest 4.7-1.1

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

Simple Random Forest

``` r
# Split data into training and testing sets
set.seed(123)
trainIndex <- sample(nrow(oversampled_churn_data), 0.8 * nrow(oversampled_churn_data))
train_data <- oversampled_churn_data[trainIndex, ]
test_data <- oversampled_churn_data[-trainIndex, ]

# Train the random forest model
rf_model <- randomForest(Exited ~ ., data = train_data, keep.forest = TRUE)


# Make predictions on the test set using the random forest model
test_preds <- predict(rf_model, newdata = test_data, type = "response")

# Compute the confusion matrix
conf_matrix <- confusionMatrix(test_preds, test_data$Exited)

# Print the confusion matrix and various statistics
cat("Confusion Matrix:\n")
```

    ## Confusion Matrix:

``` r
print(conf_matrix$table)
```

    ##           Reference
    ## Prediction   X0   X1
    ##         X0 1440   42
    ##         X1  145 1567

``` r
cat("\nAccuracy:", conf_matrix$overall["Accuracy"], "\n")
```

    ## 
    ## Accuracy: 0.9414527

``` r
cat("Sensitivity:", conf_matrix$byClass["Sensitivity"], "\n")
```

    ## Sensitivity: 0.9085174

``` r
cat("Specificity:", conf_matrix$byClass["Specificity"], "\n")
```

    ## Specificity: 0.9738968

``` r
cat("Precision:", conf_matrix$byClass["Pos Pred Value"], "\n")
```

    ## Precision: 0.9716599

``` r
cat("Recall:", conf_matrix$byClass["Sensitivity"], "\n")
```

    ## Recall: 0.9085174

``` r
cat("F1 Score:", conf_matrix$byClass["F1"], "\n")
```

    ## F1 Score: 0.9390284

From our simple model we can infer, With a sensitivity of 90.03%, the
model successfully identified 90.03% of churned consumers. The model
accurately recognized the non-churned consumers with a specificity of
97.84%. A accuracy of 94.05% means that 94.05% of all customers
projected to churn by the model actually churned. A high accuracy and F1
score, as well as a high sensitivity and specificity, indicate that the
model is effective at detecting consumers who are likely to churn

We proceed to try Tuning Hyperparameter Tuning

``` r
#May take some time for the tuning
library(caret)
#If displays problem in factors use code below
#oversampled_churn_data$Exited <- factor(oversampled_churn_data$Exited, levels = c("0", "1"))
#levels(oversampled_churn_data$Exited) <- make.names(levels(oversampled_churn_data$Exited))


# Split data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(oversampled_churn_data$Exited, p = .8, list = FALSE)
train_data <- oversampled_churn_data[trainIndex, ]
test_data <- oversampled_churn_data[-trainIndex, ]

# Create a trainControl object for cross-validation
train_control <- trainControl(method = "cv", number = 5, classProbs = TRUE)

# Create a grid of hyperparameters to tune over
mtry_values <- seq(1, ncol(train_data) - 1, by = 1)
param_grid <- expand.grid(mtry = mtry_values)

# Perform hyperparameter tuning using 5-fold cross-validation and ROC as the metric
rf_tune <- train(Exited ~ ., data = train_data, method = "rf",
                   trControl = train_control, tuneGrid = param_grid,
                   metric = "AUC")
```

    ## Warning in train.default(x, y, weights = w, ...): The metric "AUC" was not in
    ## the result set. Accuracy will be used instead.

``` r
# Print the optimal hyperparameter and corresponding ROC value
cat("Optimal mtry:", rf_tune$bestTune$mtry, "\n")
```

    ## Optimal mtry: 4

``` r
cat("ROC:", rf_tune$results[which.max(rf_tune$results$ROC), "ROC"], "\n")
```

    ## ROC:

``` r
# Train a final random forest model on the full dataset using the optimal hyperparameter
final_rf_model <- train(Exited ~ ., data = oversampled_churn_data, method = "rf",
                          trControl = train_control, tuneLength = 0,
                          preProcess = c("center", "scale"), metric = "ROC",
                          tuneGrid = data.frame(mtry = rf_tune$bestTune$mtry))
```

    ## Warning in train.default(x, y, weights = w, ...): The metric "ROC" was not in
    ## the result set. Accuracy will be used instead.

``` r
# Make predictions on the test set using the final model
test_preds <- predict(final_rf_model, newdata = test_data, type = "raw")

# Calculate the accuracy on the test set
accuracy <- mean(test_preds == test_data$Exited)
cat("Test set accuracy:", accuracy, "\n")
```

    ## Test set accuracy: 1

``` r
rf_tune
```

    ## Random Forest 
    ## 
    ## 12776 samples
    ##    11 predictor
    ##     2 classes: 'X0', 'X1' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 10221, 10221, 10221, 10220, 10221 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  Accuracy   Kappa    
    ##    1    0.7734033  0.5468938
    ##    2    0.8864276  0.7728293
    ##    3    0.9295557  0.8590844
    ##    4    0.9358958  0.8717684
    ##    5    0.9357396  0.8714559
    ##    6    0.9329215  0.8658180
    ##    7    0.9326087  0.8651913
    ##    8    0.9318258  0.8636260
    ##    9    0.9301820  0.8603363
    ##   10    0.9297905  0.8595536
    ##   11    0.9271293  0.8542280
    ## 
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final value used for the model was mtry = 4.

``` r
final_rf_model
```

    ## Random Forest 
    ## 
    ## 15969 samples
    ##    11 predictor
    ##     2 classes: 'X0', 'X1' 
    ## 
    ## Pre-processing: centered (11), scaled (11) 
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 12776, 12774, 12775, 12776, 12775 
    ## Resampling results:
    ## 
    ##   Accuracy  Kappa    
    ##   0.949277  0.8985351
    ## 
    ## Tuning parameter 'mtry' was held constant at a value of 4

Train a new model based on the hyperparameters learned above

``` r
library(caret)

# Split data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(oversampled_churn_data$Exited, p = .8, list = FALSE)
train_data <- oversampled_churn_data[trainIndex, ]
test_data <- oversampled_churn_data[-trainIndex, ]

# Train a Random Forest model using the optimal hyperparameter
rf_model <- train(Exited ~ ., data = train_data, method = "rf",
                  trControl = train_control, tuneGrid =data.frame(mtry = rf_tune$bestTune$mtry),
                  metric = "Accuracy", preProcess = c("center", "scale"),
                  tuneLength = 0)

# Make predictions on the test set using the final model
test_preds <- predict(rf_model, newdata = test_data, type = "raw")

# Compute the confusion matrix
conf_matrix <- confusionMatrix(test_preds, test_data$Exited)

# Print the confusion matrix and various statistics
cat("Confusion Matrix:\n")
```

    ## Confusion Matrix:

``` r
print(conf_matrix$table)
```

    ##           Reference
    ## Prediction   X0   X1
    ##         X0 1470   33
    ##         X1  122 1568

``` r
cat("\nAccuracy:", conf_matrix$overall["Accuracy"], "\n")
```

    ## 
    ## Accuracy: 0.9514563

``` r
cat("Sensitivity:", conf_matrix$byClass["Sensitivity"], "\n")
```

    ## Sensitivity: 0.9233668

``` r
cat("Specificity:", conf_matrix$byClass["Specificity"], "\n")
```

    ## Specificity: 0.9793879

``` r
cat("Precision:", conf_matrix$byClass["Pos Pred Value"], "\n")
```

    ## Precision: 0.9780439

``` r
cat("Recall:", conf_matrix$byClass["Sensitivity"], "\n")
```

    ## Recall: 0.9233668

``` r
cat("F1 Score:", conf_matrix$byClass["F1"], "\n")
```

    ## F1 Score: 0.9499192

``` r
# Make predictions on the test set using the final model
test_preds <- predict(rf_model, newdata = test_data, type = "prob")
# Calculate ROC curve and AUC
# Calculate the ROC curve
library(pROC)
roc_obj <- roc(test_data$Exited, test_preds$X1)
```

    ## Setting levels: control = X0, case = X1

    ## Setting direction: controls < cases

``` r
# Get the optimal operating point
opt_point <- coords(roc_obj, "best", ret=c("threshold", "specificity", "sensitivity"))


# Plot the ROC curve with the optimal point
plot(roc_obj, col = "darkblue", main = "ROC Curve", print.thres = c(0.1, 0.2, 0.3, 0.4, 0.5,0.6,0.7,0.8,0.9), print.auc = TRUE)
points(opt_point$specificity, opt_point$sensitivity, pch = 19, col = "red")
text(opt_point$specificity , opt_point$sensitivity ,
     paste0("Threshold = ", round(opt_point$threshold, 2),
            "\nSensitivity = ", round(opt_point$sensitivity, 2),
            "\nSpecificity = ", round(opt_point$specificity, 2)),
     pos = 2)
```

![](Churn_Modelling-CODE_files/figure-gfm/unnamed-chunk-109-1.png)<!-- -->
