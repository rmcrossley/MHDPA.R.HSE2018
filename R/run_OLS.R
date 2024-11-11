# Import packages ---------------------------------------------------------
library(ggplot2)
library(ggcorrplot)

# Source other scripts ----------------------------------------------------
upload <- new.env(); source("./R/upload.R", local = upload)

# Write functions ---------------------------------------------------------
run_OLS <- function(){
  # Load in data
  df <- upload$hse18red

  # Clean data to remove NAs
  df_clean <- df %>% filter(!is.na(BMI), BMIOK == 1)
  df_clean1 <- df_clean[, !names(df_clean) %in% "ILL12m"]
  df_clean2 <- df_clean1[, !names(df_clean1) %in% "ag16g10"]
  df_clean3 <- df_clean2[, !names(df_clean2) %in% "MENHTAKg2"]
  df_clean4 <- df_clean3[, !names(df_clean3) %in% "SCSatis"]
  df_clean5 <- df_clean4[, !names(df_clean4) %in% "LifeSatG"]
  df_clean6 <- df_clean5[, !names(df_clean5) %in% "AntiDepTakg2"]
  df_clean7 <- df_clean6[, !names(df_clean6) %in% "Anxiet17g3"]
  df_clean8 <- df_clean7[, !names(df_clean7) %in% "GHQ"]
  df_clean9 <- df_clean8[, !names(df_clean8) %in% "BMIvg5"]

  #p <- ggplot(df_clean9, aes(x = GHQ12Scr, y = BMI))+
  #  geom_point()
  #print(p)

  # Estimate the model and same the results in object "ols"
  ols <- lm(BMI ~. , data = df_clean9)

  print(ols)

  print(summary(ols))

  #z <- ggplot(df_clean9, aes(x = GHQ12Scr, y = BMI)) +
  #  geom_point() +
  #  geom_smooth(method = "lm", se = FALSE) +
  #  scale_x_continuous(limits = c(0, 12), expand = c(0, 0)) +
  #  theme_bw()
  #print(z)

  # Get the model residuals
  model_residuals = ols$residuals

  # Plot the result
  hist(model_residuals)

  # Plot the residuals
  qqnorm(model_residuals)
  # Plot the Q-Q line
  qqline(model_residuals)

  # Compute correlation at 2 decimal places
  corr_matrix = round(cor(df_clean9), 2)

  # Compute and show the  result
  ggcorrplot(corr_matrix, type = "lower",
             lab = TRUE)

  # higher the value of coefficients the higher their importance is
  coeff <- tidy(ols) %>%
    arrange(desc(abs(estimate))) %>%
    filter(abs(estimate) > 0.5)
  print(coeff)

  # Plot importance
  ggplot(coeff, aes(x = term, y = estimate, fill = term)) + geom_col() + coord_flip()
}
