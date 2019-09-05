### Fariha Khan
### Module 5: Hypotheses Testing
### Data: Hollywood’s Most Profitable Movies
### 


# Load data ------------------------------------------------------------------------------
library(tidyverse)
### Load in data
df <- read_csv("~/Downloads/HollywoodsMostProfitableStories.csv")

### Explore and prep dataset
### Remove spaces from column names
### Remove rows with NA
glimpse(df)
names(df) <- gsub(" ", "_", names(df))
df <- na.omit(df)

### Look at genre overview
df %>%
      group_by(Genre) %>%
      summarize(n = n())

### Get overview of audience score and film gross
summary(df$`Audience__score_%`)
summary(df$Worldwide_Gross)

df %>% 
      arrange(`Audience__score_%`) %>% 
      select(`Audience__score_%`)
# Correlation coefficient ----------------------------------------------------------------
### Extract data into individual vectors in ascending order
score <-  df %>% 
      arrange(`Audience__score_%`) %>% 
      select(`Audience__score_%`) %>% 
      unlist()
gross <- df %>% 
      arrange(Worldwide_Gross) %>% 
      select(Worldwide_Gross) %>% 
      unlist()

### Get correlation coefficient 
cor(df$`Audience__score_%`, df$Worldwide_Gross,
    method = 'pearson')


# Spearman correlation -------------------------------------------------------------------

cor.test(df$`Audience__score_%`, df$Worldwide_Gross, 
         method = 'pearson')

# Spearman -------------------------------------------------------------------------------

cor(df$`Audience__score_%`, df$Worldwide_Gross,
    method = 'spearman')



cor.test(df$`Audience__score_%`, df$Worldwide_Gross, 
         method = 'spearman')
