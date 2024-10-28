
# Load required packages --------------------------------------------------
library(tidyverse)
library(vcd)
library(ggplot2)

# Source other scripts ----------------------------------------------------
upload <- new.env(); source("./R/upload.R", local = upload)


# Main function to run analysis -------------------------------------------

#' Run analysis
#'
#' Main function used to run analysis. This function is called from
#' the main.R script which should be used to run the project.
#'
#' @export
#'
run_grouped_analysis <- function() {
  print("Found the run_analysis function!")
  #log action
  logger$info("Running grouped analysis...")

  # Load in data
  df <- upload$hse18lab

  # Loop through factors for plots --------------------------------------------------------------------------------------
  # BMI first
  # List of factors to iterate through
  factors <- c("ag16g10", "Sex", "origin2",
               "LifeSatG", "IllAff7", "ILL12m", "MENHTAKg2",
               "AntiDepTakg2", "AntiDepM2", "topqual3", "RELIGSC", "HHINC3",
               "eqv5", "GHQ", "Anxiet17g3", "MVPATert")

  # Initialize a data frame to store results
  results <- data.frame(Factor = character(), Chi_Square_p_value = numeric(),
                        Cramers_V = numeric(), stringsAsFactors = FALSE)

  # Loop through each factor and perform the analysis
  for (factor in factors) {
    print(paste("Analysing", factor, "vs BMI"))

    # Clean out NAs for both Sex and the current factor
    df_clean <- df[complete.cases(df$BMIvg5, df[[factor]]), ]

    # Create a contingency table
    tbl <- table(df_clean$BMIvg5, df_clean[[factor]])

    # Perform chi-square test
    chi_square_result <- chisq.test(tbl)

    # Calculate Cramér's V
    cramers_v <- assocstats(tbl)$cramer

    # Save the results
    results <- rbind(results, data.frame(Factor = factor,
                                         Chi_Square_p_value = chi_square_result$p.value,
                                         Cramers_V = cramers_v))

    # Visualize with a mosaic plot
    mosaicplot(tbl, main = paste("Mosaic Plot of", factor, "and BMI"),
               ylab = factor, xlab = "BMI", color = TRUE)
  }

  # Print or save the results
  print(results)

  # Optionally, save results to a CSV or document
  write.csv(results, "BMI_chi_square_results.csv", row.names = FALSE)

  # Compare to GHQ score bands
  # List of factors to iterate through
  factors <- c("ag16g10", "Sex", "origin2", "topqual3", "RELIGSC", "HHINC3", "eqv5", "MVPATert", "Anxiet17g3")

  # Initialize a data frame to store results
  results <- data.frame(Factor = character(), Chi_Square_p_value = numeric(),
                        Cramers_V = numeric(), stringsAsFactors = FALSE)

  # Loop through each factor and perform the analysis
  for (factor in factors) {
    print(paste("Analysing", factor, "vs GHQ score"))

    # Clean out NAs for both Sex and the current factor
    df_clean <- df[complete.cases(df[[factor]], df$GHQ), ]

    # Create a contingency table
    tbl <- table(df_clean[[factor]], df_clean$GHQ)

    # Perform chi-square test
    chi_square_result <- chisq.test(tbl)

    # Calculate Cramér's V
    cramers_v <- assocstats(tbl)$cramer

    # Save the results
    results <- rbind(results, data.frame(Factor = factor,
                                         Chi_Square_p_value = chi_square_result$p.value,
                                         Cramers_V = cramers_v))

    # Visualize with a mosaic plot
    mosaicplot(tbl, main = paste("Mosaic Plot of", factor, "and GHQ"),
               xlab = factor, ylab = "GHQ", color = TRUE)
  }

  # Print or save the results
  print(results)

  # Optionally, save results to a CSV or document
  write.csv(results, "GHQ_chi_square_results.csv", row.names = FALSE)

}

run_plots <- function(){
  #log action
  logger$info("Running plots...")

  # Load in data
  df <- upload$hse18lab

  df_filtered <-  df %>%
    filter(BMIOK == 1, !is.na(GHQ12Scr))

  df_clean <- df[complete.cases(df$BMI, df$GHQ12Scr), ]

  #Plotting
  ggplot(df_clean, aes(y = BMI, x = GHQ12Scr)) +
    geom_point() +
    labs(title = "Scatter plot of BMI vs GHQ", y = "BMI", x = "GHQ score")

  ggplot(df, aes(x = as.factor(GHQ12Scr), y = BMI)) +
    geom_boxplot() +
    labs(title = "Box plot of BMI by GHQ Score", x = "GHQ Score", y = "BMI")

  ggplot(df, aes(y = GHQ12Scr, x = BMIvg5)) +
    geom_boxplot() +
    labs(title = "Box plot of BMI by GHQ Score", x = "BMI", y = "GHQ Score") +
    scale_y_continuous(breaks = seq(0, 12, by = 1))


}

run_age_filter <- function(){
  df <- upload$hse18lab

  df_filtered <-  df %>%
    filter(BMIOK == 1, !is.na(GHQ12Scr), !is.na(Sex)) %>%
    mutate(Sex = as.factor(Sex))

  df_men <- df_filtered %>% filter(Sex == "Male")
  df_women <- df_filtered %>% filter(Sex == "Female")

  p1 <- ggplot(df_women, aes(y = BMI, x = as.factor(GHQ12Scr))) +
    geom_boxplot(fill = "deeppink") +
    labs(title = "Box plot of BMI vs GHQ (Women)", y = "BMI", x = "GHQ")
  print(p1)

  p2 <- ggplot(df_men, aes(y = BMI, x = as.factor(GHQ12Scr))) +
    geom_boxplot(fill = "blue") +
    labs(title = "Box plot of BMI vs GHQ (Men)", y = "BMI", x = "GHQ")
  print(p2)

  q1 <- ggplot(df_men, aes(y = GHQ12Scr, x = BMIvg5)) +
    geom_boxplot(fill = "blue") +
    labs(title = "Box plot of BMI by GHQ Score (Men)", x = "BMI", y = "GHQ Score") +
    scale_y_continuous(breaks = seq(0, 12, by = 1))
  print(q1)

  q2 <- ggplot(df_women, aes(y = GHQ12Scr, x = BMIvg5)) +
    geom_boxplot(fill = "deeppink") +
    labs(title = "Box plot of BMI by GHQ Score (Women)", x = "BMI", y = "GHQ Score") +
    scale_y_continuous(breaks = seq(0, 12, by = 1))
  print(q2)
}
