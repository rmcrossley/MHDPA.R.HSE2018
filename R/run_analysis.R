
# Load required packages --------------------------------------------------
library(tidyverse)
library(vcd)

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
run_analysis <- function() {
  print("Found the run_analysis function!")
  #log action
  logger$info("Running analysis...")

  # Load in data
  df <- upload$hse_2019_in_lab

  # Loop through factors for plots --------------------------------------------------------------------------------------
  # BMI first
  # List of factors to iterate through
  factors <- c("ag16g10", "Sex", "ThCoAny", "origin2",
               "LifeSatG", "wemwbs", "IllAff7", "ILL12m", "MENHTAKg2",
               "AntiDepTakg2", "SCOFF2", "qimd19", "AntiDepM2", "topqual3", "RELIGSC", "HHINC3", "eqv5", "totalwug_19")

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

  # BMI first
  # List of factors to iterate through
  factors <- c("ag16g10", "Sex", "qimd19", "origin2", "topqual3", "RELIGSC", "HHINC3", "eqv5", "totalwug_19")

  # Initialize a data frame to store results
  results <- data.frame(Factor = character(), Chi_Square_p_value = numeric(),
                        Cramers_V = numeric(), stringsAsFactors = FALSE)

  # Loop through each factor and perform the analysis
  for (factor in factors) {
    print(paste("Analysing", factor, "vs Life Satisfaction"))

    # Clean out NAs for both Sex and the current factor
    df_clean <- df[complete.cases(df[[factor]], df$LifeSatG), ]

    # Create a contingency table
    tbl <- table(df_clean[[factor]], df_clean$LifeSatG)

    # Perform chi-square test
    chi_square_result <- chisq.test(tbl)

    # Calculate Cramér's V
    cramers_v <- assocstats(tbl)$cramer

    # Save the results
    results <- rbind(results, data.frame(Factor = factor,
                                         Chi_Square_p_value = chi_square_result$p.value,
                                         Cramers_V = cramers_v))

    # Visualize with a mosaic plot
    mosaicplot(tbl, main = paste("Mosaic Plot of", factor, "and Life Satisfaction"),
               xlab = factor, ylab = "Life Satisfaction", color = TRUE)
  }

  # Print or save the results
  print(results)

  # Optionally, save results to a CSV or document
  write.csv(results, "LifeSatG_chi_square_results.csv", row.names = FALSE)

}
