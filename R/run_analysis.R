
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
    mosaicplot(tbl, main = paste("Mosaic Plot of", factor, "and Life Satisfaction"),
               xlab = factor, ylab = "GHQ", color = TRUE)
  }

  # Print or save the results
  print(results)

  # Optionally, save results to a CSV or document
  write.csv(results, "GHQ_chi_square_results.csv", row.names = FALSE)

}
