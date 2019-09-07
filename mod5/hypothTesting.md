Module 5 - Hollywood’s Most Profitable Stories
================
Fariha Khan
August 25 2019

Introduction
============

This analysis explores Hollywood's Most Profitable Stories dataset to conduct statistical hypothesis testing.

Data was loaded downloaded from the sample data available on <https://public.tableau.com/en-us/s/resources>.

Use the following packages for hypothesis testing:

``` r
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ggpubr))
suppressPackageStartupMessages(library(knitr))
```

Read in data

``` r
df <- read.csv("~/Documents/MS_DataScience/MIS500/Week5/data/HollywoodsMostProfitableStories.csv")
glimpse(df)
```

    ## Observations: 74
    ## Variables: 8
    ## $ Film              <fct> Waiting For Forever, My Week with Marilyn, A...
    ## $ Genre             <fct> Romance, Drama, Drama, Drama, Comedy, Comedy...
    ## $ Lead.Studio       <fct> Independent, The Weinstein Company, Independ...
    ## $ Audience..score.. <int> 53, 84, 89, 66, 80, 70, 61, 52, 57, 47, 74, ...
    ## $ Profitability     <dbl> 0.005, 0.826, 0.449, 2.140, 4.472, 0.253, 1....
    ## $ Rotten.Tomatoes.. <int> 6, 83, 79, 34, 84, 78, 85, 68, 71, 15, 52, 1...
    ## $ Worldwide.Gross   <dbl> 0.025, 8.258, 8.973, 10.700, 14.310, 15.174,...
    ## $ Year              <int> 2011, 2011, 2011, 2009, 2011, 2008, 2008, 20...

Clean up dataset

-   Clean column

-   Remove NA

``` r
names(df)
```

    ## [1] "Film"              "Genre"             "Lead.Studio"      
    ## [4] "Audience..score.." "Profitability"     "Rotten.Tomatoes.."
    ## [7] "Worldwide.Gross"   "Year"

``` r
df <- df %>% 
      rename(Audience.score = Audience..score.., 
             Rotten.Tomatoes = Rotten.Tomatoes..) %>%
      mutate(Film = as.character(Film)) %>% 
      na.omit()
glimpse(df)
```

    ## Observations: 70
    ## Variables: 8
    ## $ Film            <chr> "Waiting For Forever", "My Week with Marilyn",...
    ## $ Genre           <fct> Romance, Drama, Drama, Drama, Comedy, Comedy, ...
    ## $ Lead.Studio     <fct> Independent, The Weinstein Company, Independen...
    ## $ Audience.score  <int> 53, 84, 89, 66, 80, 70, 61, 52, 57, 47, 74, 58...
    ## $ Profitability   <dbl> 0.005, 0.826, 0.449, 2.140, 4.472, 0.253, 1.38...
    ## $ Rotten.Tomatoes <int> 6, 83, 79, 34, 84, 78, 85, 68, 71, 15, 52, 45,...
    ## $ Worldwide.Gross <dbl> 0.025, 8.258, 8.973, 10.700, 14.310, 15.174, 1...
    ## $ Year            <int> 2011, 2011, 2011, 2009, 2011, 2008, 2008, 2010...

Preliminary Testing
===================

Check distribution of variables

``` r
xdens <- ggdensity(df, x = "Audience.score",add = "mean", 
                   title = "Audience Score Density") +
      stat_overlay_normal_density(color = "red", linetype = "dashed")

ydens <- ggdensity(df, x = "Worldwide.Gross", add = "mean", 
                   title = "Worldwide Gross Density") +
      stat_overlay_normal_density(color = "blue", linetype = "dashed")

ggarrange(xdens, ydens, labels = "AUTO")
```

![](hypothTesting_files/figure-markdown_github/densityplots-1.png)

Do Shapiro-Wilk normality test and draw quantile-quantile plots

``` r
xshap <- shapiro.test(df$Audience.score) %>% 
      unlist() %>% t() %>% as.data.frame()
xshap <- xshap[2]
xshap <- ggtexttable(xshap, cols = colnames(xshap))
xq <- ggqqplot(df$Audience.score, color = "red", size = 1, shape = 1,
               ylab = "Audience Score (%)")

yshap <- shapiro.test(df$Worldwide.Gross) %>% 
      unlist() %>% t() %>% as.data.frame()
yshap <- yshap[2]
yshap <- ggtexttable(yshap, cols = colnames(yshap))
yq <- ggqqplot(df$Worldwide.Gross, color = "blue", size = 1, 
               shape = 1, ylab = "Worldwide Gross")
ggarrange(xshap, yshap, xq, yq, 
          ncol = 2, nrow = 2, heights = c(1, 3), labels = c("A", "B"))
```

![](hypothTesting_files/figure-markdown_github/normalityTest-1.png)

Correlation tests
=================

``` r
ggscatter(df, x = "Audience.score", y = "Worldwide.Gross", 
          add = "reg.line", conf.int = TRUE, cor.coef = TRUE, 
          title = "Audience Score vs. Worldwide Gross",
          cor.method = "spearman")
```

![](hypothTesting_files/figure-markdown_github/scatterplots-1.png)

``` r
ggscatter(df, x = "Audience.score", y = "Worldwide.Gross", 
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "kendall",
          title = "Audience Score vs. Worldwide Gross")
```

![](hypothTesting_files/figure-markdown_github/scatterplots-2.png)

``` r
x <- cor.test(x = df$Audience.score, y = df$Worldwide.Gross,
          method = "spearman", alternative = 'greater')
```

    ## Warning in cor.test.default(x = df$Audience.score, y = df
    ## $Worldwide.Gross, : Cannot compute exact p-value with ties

``` r
cor.test(x = df$Audience.score, y = df$Worldwide.Gross,
          method = "kendall")
```

    ## 
    ##  Kendall's rank correlation tau
    ## 
    ## data:  df$Audience.score and df$Worldwide.Gross
    ## z = 2.6483, p-value = 0.008089
    ## alternative hypothesis: true tau is not equal to 0
    ## sample estimates:
    ##       tau 
    ## 0.2181915
