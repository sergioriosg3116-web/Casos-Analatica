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

## Question 6 – Full Model
model6_full <- lm(`Opening Gross` ~ Budget + Genre + MPAA + `Known Story` + Sequel + `Opening Theatres` + Summer + Holiday + Christmas, data = df)

summary(model6_full)


##p < 0.10 → se queda


## Question 6 – Reduced Model
model6_reduced <- lm(`Opening Gross` ~ Budget + MPAA + Sequel + `Opening Theatres` + Summer, data = df)

summary(model6_reduced)


## Question 6.d – Effect of 100 Additional Opening Theatres
beta_theatres <- coef(model6_reduced)["`Opening Theatres`"]
se_theatres <- summary(model6_reduced)$coefficients["`Opening Theatres`", "Std. Error"]
df_resid <- df.residual(model6_reduced)
t_crit <- qt(0.975, df_resid)

point_estimate_100 <- 100 * beta_theatres
ci_lower_100 <- 100 * (beta_theatres - t_crit * se_theatres)
ci_upper_100 <- 100 * (beta_theatres + t_crit * se_theatres)

point_estimate_100
ci_lower_100
ci_upper_100

## Question 7 – Simple Regression
model7 <- lm(`Total U.S. Gross` ~ `Opening Gross`, data = df)

summary(model7)


## Question 7 – Multiple Regression (extended model)
model7_extended <- lm(`Total U.S. Gross` ~ `Opening Gross` + Budget + Genre + MPAA + Sequel, data = df)
summary(model7_extended)


## Question 7.c – Formal test of the 25% rule
beta_hat <- coef(model7)["`Opening Gross`"]
se_beta <- summary(model7)$coefficients["`Opening Gross`", "Std. Error"]
df_resid_7 <- df.residual(model7)

t_stat_7c <- (beta_hat - 4) / se_beta
p_value_7c <- 2 * pt(-abs(t_stat_7c), df = df_resid_7)

t_stat_7c
p_value_7c

## 95% confidence interval for slope
confint(model7, level = 0.95)

## Question 7 - Plot
ggplot(df, aes(x = Genre, y = `Total U.S. Gross`)) +
  geom_boxplot()

## Question 8 – Full Model with Critics
model8_full <- lm(`Total U.S. Gross` ~ `Opening Gross` + `Critics´ Opinion` + Budget + Genre + MPAA + Sequel, data = df)

summary(model8_full)

## Question 8 - Model Reduced
model8_reduced <- lm(`Total U.S. Gross` ~ 
                       `Opening Gross` + 
                       `Critics´ Opinion` + 
                       Budget + 
                       Genre,
                     data = df)

summary(model8_reduced)


## Question 8.c – Prediction and 95% Prediction Interval
new_movie <- data.frame(
  `Opening Gross` = 20000000,
  `Critics´ Opinion` = 79,
  Budget = 90000000,
  Genre = "Drama",
  check.names = FALSE
)

predict(model8_reduced, newdata = new_movie, interval = "prediction", level = 0.95)

## Question 9 – Interaction Model
model9 <- lm(`Total U.S. Gross` ~ `Opening Gross` + `Critics´ Opinion` + comedy_dummy + 
               `Critics´ Opinion`:comedy_dummy + Budget + MPAA + Sequel, data = df)

summary(model9)

