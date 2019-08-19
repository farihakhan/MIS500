# #### Fariha Khan
# ### Critical thinking assignment 4 
# ### August 18, 2019

# Assignment instructions ----------------------------------------------------------------

# R Hypothesis Tests

# Copy in sample code --------------------------------------------------------------------


# Load package into active libray 
# The dplyr package will be used to conduct statistical testing
# The restulting outcomes ill be used to generate a hypothesis 
library(dplyr)


# Store student score data
tScore_before <- c(40, 62, 74, 22, 64, 65, 49, 49, 49)
tScore_after <- c(68, 61, 64, 76, 90, 75, 66, 60, 63)

# Create a data frame
score_data <- data.frame(
      group = rep(c("Score Before", "Score After"), each = 9),
      scores = c(tScore_before,  tScore_after))



# Print all data
print(score_data)


# Summary by groups ----------------------------------------------------------------------


group_by(score_data, group) %>%
      
      summarise(
            
            count = n(),
            
            mean = mean(scores, na.rm = TRUE),
            
            sd = sd(scores, na.rm = TRUE)
            
      )

# Get full summary of data in two different groups
# Get overview of values in two groups
score_data %>% 
      group_by(group) %>% 
      summarise(count = n(),
                min = min(scores, na.rm = TRUE),
                mean = mean(scores, na.rm = TRUE),
                max = max(scores, na.rm = TRUE),
                sd = sd(scores,na.rm = TRUE))


summary(score_data$group)


# T-test -------------------------------------------------------------


# Compute Unpaired Two Sample t-test
# The three alternate hypotheses:
# μd > Do
# μd < Do
# μd ≠ Do

ttest_twosample <- t.test(tScore_before, tScore_after, var.equal = TRUE)

ttest_twosample



# Compute independent t-test

ttest_ind <- t.test(scores ~ group, data = score_data, var.equal = TRUE)

ttest_ind




#test whether the average score before score is less than the average after score, type this:

t.test(scores ~ group, data = score_data,
       
       var.equal = TRUE, alternative = "less")
