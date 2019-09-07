### Fariha Khan
### Module 5: Hypotheses Testing
### Data: Hollywood’s Most Profitable Movies
### 


# Load data ------------------------------------------------------------------------------
library(tidyverse)
### Load in data
df2 <- read_csv("~/Documents/MS_DataScience/MIS500/Week5/data/HollywoodsMostProfitableStories.csv")

### Explore and prep dataset
### Remove spaces from column names
### Remove rows with NA
glimpse(df2)
names(df2) <- gsub(" ", "_", names(df2))
df2 <- na.omit(df2)

### Look at genre overview
df2 %>%
      group_by(Genre) %>%
      summarize(n = n())

### Get overview of audience score and film gross
summary(df2$`Audience__score_%`)
summary(df2$Worldwide_Gross)

df2 %>% 
      arrange(`Audience__score_%`) %>% 
      select(`Audience__score_%`)
# Correlation coefficient ----------------------------------------------------------------
### Extract data into individual vectors in ascending order
score <-  df2 %>% 
      arrange(`Audience__score_%`) %>% 
      select(`Audience__score_%`) %>% 
      unlist()
gross <- df2 %>% 
      arrange(Worldwide_Gross) %>% 
      select(Worldwide_Gross) %>% 
      unlist()

### Get correlation coefficient 
shapiro.test(df2$`Audience__score_%`)
shapiro.test(df2$Worldwide_Gross)



cor(df2$`Audience__score_%`, df2$Worldwide_Gross,
    method = 'pearson')


# Spearman correlation -------------------------------------------------------------------

cor.test(df2$`Audience__score_%`, df2$Worldwide_Gross, 
         method = 'pearson')

# Spearman -------------------------------------------------------------------------------

cor(df2$`Audience__score_%`, df2$Worldwide_Gross,
    method = 'spearman')



cor.test(df2$`Audience__score_%`, df2$Worldwide_Gross, 
         method = 'spearman')
# 
# ggqqplot(df2$Worldwide.Gross, add = "qqline")

# ggdensity(df2, x = "Audience.score",
#           add = "mean", title = "Audience Score Density") +
#       stat_overlay_normal_density(color = "red", linetype = "dashed")

