library(readxl)
library(tidyverse)
library(janitor)


df <- read_excel("Hollywood.xlsx", sheet = 2)
colnames(df)


## Question 1 – Descriptive Statistics
summary_stats <- df %>%
  summarise(
    min_opening = min(`Opening Gross`, na.rm = TRUE),
    mean_opening = mean(`Opening Gross`, na.rm = TRUE),
    max_opening = max(`Opening Gross`, na.rm = TRUE),
    
    min_total_us = min(`Total U.S. Gross`, na.rm = TRUE),
    mean_total_us = mean(`Total U.S. Gross`, na.rm = TRUE),
    max_total_us = max(`Total U.S. Gross`, na.rm = TRUE),
    
    min_total_non_us = min(`Total Non-U.S. Gross`, na.rm = TRUE),
    mean_total_non_us = mean(`Total Non-U.S. Gross`, na.rm = TRUE),
    max_total_non_us = max(`Total Non-U.S. Gross`, na.rm = TRUE),
    
    min_theatres = min(`Opening Theatres`, na.rm = TRUE),
    mean_theatres = mean(`Opening Theatres`, na.rm = TRUE),
    max_theatres = max(`Opening Theatres`, na.rm = TRUE)
  )

summary_stats
as.data.frame(summary_stats)


## Question 1 – Counts
num_comedy <- df %>%
  filter(Genre == "Comedy") %>%
  nrow()

num_comedy

num_r_rated <- df %>%
  filter(MPAA == "R") %>%
  nrow()

num_r_rated


## Question 2 – ROI Calculation
df <- df %>%
  mutate(
    roi_us = (`Total U.S. Gross` - Budget) / Budget
  )

summary(df$roi_us)


## Question 2 – Confidence Interval
t.test(df$roi_us)

## Hipótesis: ¿ROI Promedio > 12%?
## H0: μ = 0.12
## H1: μ > 0.12


## Question 2 – Hypothesis Test
t.test(df$roi_us, mu = 0.12, alternative = "greater")


## Question 3 – Create Comedy Dummy
df <- df %>%
  mutate(
    comedy_dummy = ifelse(Genre == "Comedy", "Comedy", "Non-Comedy")
  )


## Question 3 – Total U.S. Gross (Comedy vs Non-Comedy)
t_test_gross <- t.test(`Total U.S. Gross` ~ comedy_dummy, data = df)

t_test_gross


## Question 3 – ROI (Comedy vs Non-Comedy)
t_test_roi <- t.test(roi_us ~ comedy_dummy, data = df)

t_test_roi


## Question 3 - Plot
ggplot(df, aes(x = `Opening Gross`, y = `Total U.S. Gross`)) +
  geom_point() +
  geom_smooth(method = "lm")


## Question 4 – Create R-rated Dummy
df <- df %>%
  mutate(
    r_dummy = ifelse(MPAA == "R", "R-rated", "Non R-rated")
  )


## Question 4 – Total U.S. Gross (R-rated vs Non R-rated)
t_test_gross_r <- t.test(`Total U.S. Gross` ~ r_dummy, data = df)

t_test_gross_r


## Question 4 – ROI (R-rated vs Non R-rated)
t_test_roi_r <- t.test(roi_us ~ r_dummy, data = df)

t_test_roi_r 


## Question 5 – Full Model
model_full <- lm(`Total U.S. Gross` ~ Budget + Genre + MPAA + `Known Story` + Sequel, data = df)

summary(model_full)


## p < 0.10 → se queda
## p ≥ 0.10 → se elimina


## Question 5 – Reduced Model
model_reduced <- lm(`Total U.S. Gross` ~ Budget + Genre + Sequel, data = df)

summary(model_reduced)


## Question 5.c – Interpretation of Sequel coefficient
coef(model_reduced)

