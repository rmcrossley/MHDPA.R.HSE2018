
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
               "eqv5", "GHQ", "Anxiet17g3", "MVPATert", "qimd")

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
  factors <- c("ag16g10", "Sex", "origin2", "topqual3", "RELIGSC", "HHINC3", "eqv5", "MVPATert", "Anxiet17g3", "qimd")

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

run_sex_filter <- function(){
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

run_depravity_filter <- function(){
  df <- upload$hse18lab

  df_filtered <-  df %>%
    filter(BMIOK == 1, !is.na(GHQ12Scr), !is.na(qimd)) %>%
    mutate(qimd = as.factor(qimd))

  df_least <- df_filtered %>% filter(qimd == "0.48->8.37 (Least Deprived)")
  df_most <- df_filtered %>% filter(qimd == "33.88->92.60 (Most Deprived)")

  q1 <- ggplot(df_least, aes(y = GHQ12Scr, x = BMIvg5)) +
    geom_boxplot(fill = "darkolivegreen4") +
    labs(title = "Box plot of BMI vs GHQ (Least Deprived)", y = "GHQ Score", x = "BMI Status") +
    scale_y_continuous(breaks = seq(0, 12, by = 1))
  print(q1)

  q2 <- ggplot(df_most, aes(y = GHQ12Scr, x = BMIvg5)) +
    geom_boxplot(fill = "darkorange3") +
    labs(title = "Box plot of BMI vs GHQ (Most Deprived)", y = "GHQ Score", x = "BMI Status") +
    scale_y_continuous(breaks = seq(0, 12, by = 1))
  print(q2)
}

#filtered on age check (i.e. 40-74)
run_HCage_filter <- function(){
  df <- upload$hse18lab

  df_filtered <-  df %>%
    filter(BMIOK == 1, !is.na(GHQ12Scr), !is.na(age16g5), age16g5 > 6, age16g5 < 14)

  q1 <- ggplot(df_filtered, aes(y = GHQ12Scr, x = BMIvg5)) +
    geom_boxplot(fill = "brown2") +
    labs(title = "Box plot of BMI vs GHQ (Ages 40-74)", y = "GHQ Score", x = "BMI Status") +
    scale_y_continuous(breaks = seq(0, 12, by = 1))
  print(q1)

  tbl <- table(df_filtered$BMIvg5, df_filtered$GHQ12Scr)
  # Visualize with a mosaic plot
  mosaicplot(tbl, main = paste("Mosaic Plot of GHQ Score and BMI Status for those aged 40-74"),
             ylab = "GHQ Score", xlab = "BMI", color = TRUE)
}

run_age_filter <- function(){
  df <- upload$hse18lab

  df_filtered <-  df %>%
    filter(BMIOK == 1, !is.na(GHQ12Scr), !is.na(ag16g10)) %>%
    mutate(ag16g10 = as.factor(ag16g10))

  df_1624 <- df_filtered %>% filter(ag16g10 == "16-24")
  df_6574 <- df_filtered %>% filter(ag16g10 == "65-74")

  q1 <- ggplot(df_1624, aes(y = GHQ12Scr, x = BMIvg5)) +
    geom_boxplot(fill = "chocolate1") +
    labs(title = "Box plot of BMI vs GHQ (16-24)", y = "GHQ Score", x = "BMI Status") +
    scale_y_continuous(breaks = seq(0, 12, by = 1))
  print(q1)

  q2 <- ggplot(df_6574, aes(y = GHQ12Scr, x = BMIvg5)) +
    geom_boxplot(fill = "chocolate3") +
    labs(title = "Box plot of BMI vs GHQ (65-74)", y = "GHQ Score", x = "BMI Status") +
    scale_y_continuous(breaks = seq(0, 12, by = 1))
  print(q2)
}

run_employment_filter <- function(){
  df <- upload$hse18lab

  df_filtered <-  df %>%
    filter(BMIOK == 1, !is.na(GHQ12Scr), !is.na(nssec8))

  df_un <- df_filtered %>% filter(nssec8 == 8)
  df_emp <- df_filtered %>% filter(nssec8 != 8 & nssec8 != 99)

  q1 <- ggplot(df_un, aes(y = GHQ12Scr, x = BMIvg5)) +
    geom_boxplot(fill = "chocolate4") +
    labs(title = "Box plot of BMI vs GHQ (Never worked or long term unemployed )", y = "GHQ Score", x = "BMI Status") +
    scale_y_continuous(breaks = seq(0, 12, by = 1))
  print(q1)

  q2 <- ggplot(df_emp, aes(y = GHQ12Scr, x = BMIvg5)) +
    geom_boxplot(fill = "cornflowerblue") +
    labs(title = "Box plot of BMI vs GHQ (Employed)", y = "GHQ Score", x = "BMI Status") +
    scale_y_continuous(breaks = seq(0, 12, by = 1))
  print(q2)
}

run_limlast_filter <- function(){
  df <- upload$hse18lab

  df_filtered <-  df %>%
    filter(BMIOK == 1, !is.na(GHQ12Scr), !is.na(limlast))

  df_n <- df_filtered %>% filter(limlast == 3)
  df_y <- df_filtered %>% filter(limlast != 3)
  df_y_lim <- df_filtered %>% filter(limlast == 1)
  df_y_non <- df_filtered %>% filter(limlast == 2)

  q1 <- ggplot(df_n, aes(y = GHQ12Scr, x = BMIvg5)) +
    geom_boxplot(fill = "antiquewhite4") +
    labs(title = "Box plot of BMI vs GHQ (No longlasting illness)", y = "GHQ Score", x = "BMI Status") +
    scale_y_continuous(breaks = seq(0, 12, by = 1))
  print(q1)

  q2 <- ggplot(df_y, aes(y = GHQ12Scr, x = BMIvg5)) +
    geom_boxplot(fill = "aquamarine3") +
    labs(title = "Box plot of BMI vs GHQ (Longlasting illness)", y = "GHQ Score", x = "BMI Status") +
    scale_y_continuous(breaks = seq(0, 12, by = 1))
  print(q2)

  q3 <- ggplot(df_y_lim, aes(y = GHQ12Scr, x = BMIvg5)) +
    geom_boxplot(fill = "aquamarine1") +
    labs(title = "Box plot of BMI vs GHQ (Limiting longlasting illness)", y = "GHQ Score", x = "BMI Status") +
    scale_y_continuous(breaks = seq(0, 12, by = 1))
  print(q3)

  q4 <- ggplot(df_y_non, aes(y = GHQ12Scr, x = BMIvg5)) +
    geom_boxplot(fill = "aquamarine2") +
    labs(title = "Box plot of BMI vs GHQ (Non limiting longlasting illness)", y = "GHQ Score", x = "BMI Status") +
    scale_y_continuous(breaks = seq(0, 12, by = 1))
  print(q4)
}

run_moderators <- function(){
  message("Starting the function 'run_moderators'...")

  #Load in data
  df <- upload$hse18lab
  message("df loaded")

  #Filter data by valid BMI only
  df_filtered <-  df %>%
    filter(BMIOK == 1)
  message("Dataframe successfully filtered by valid BMI only")

  # Define your variable names and file path
  moderators <- c("nssec8", "ag16g10", "limlast", "Sex", "origin2",
                  "LifeSatG", "IllAff7", "ILL12m", "MENHTAKg2",
                  "AntiDepTakg2", "AntiDepM2", "topqual3", "RELIGSC", "HHINC3",
                  "eqv5", "GHQ", "Anxiet17g3", "MVPATert")
  message("Defined moderators of interest")

  # File to save outputs to
  output_file <- "moderation_results.txt"
  message("Created output file")

  # Open the file for writing
  sink(output_file)
  message("Opened output file")

  # Loop through each moderator, fit model, and save output
  for (moderator in moderators) {

    # Dynamically build the formula
    formula <- as.formula(paste("GHQ12Scr ~ BMI *", moderator))
    message("Formula built with moderator:", moderator)

    # Fit the model
    model <- lm(formula, data = df_filtered)
    message("Model fitting complete")

    # Print a header for clarity in output file
    cat("\n\n--- Results for Moderator:", moderator, "---\n\n")

    # Capture the summary output
    model_summary <- capture.output(summary(model))

    # Save the summary to a file
    cat(model_summary)
    message("Completed for moderator:", moderator)
  }

  sink()
  file.show(output_file)
}

visualise_moderators <- function(){
  #Load in data
  df <- upload$hse18lab

  #Filter data by valid BMI only
  df_filtered <-  df %>%
    filter(BMIOK == 1)

  moderators <- c("nssec8", "ag16g10", "limlast", "Sex", "origin2",
                  "LifeSatG", "IllAff7", "ILL12m", "MENHTAKg2",
                  "AntiDepTakg2", "AntiDepM2", "topqual3", "RELIGSC", "HHINC3",
                  "eqv5", "GHQ", "Anxiet17g3", "MVPATert")

  for (moderator in moderators){
  c <- moderator
  p <- ggplot(df_filtered, aes(x = BMI, y = GHQ12Scr, color = factor(c))) +
    geom_point() +
    geom_smooth(method = "lm", aes(group = c), se = FALSE) +
    scale_y_continuous(breaks = seq(0, 12, by = 1))
  print(p)
  }
}

#stuck working this one out - as far as printed works
run_mediators <- function(){

  #Load in data
  df <- upload$hse18lab

  #Filter data by valid BMI only
  df_filtered <-  df %>%
    filter(BMIOK == 1)

  mediators <- c("nssec8", "ag16g10", "limlast", "Sex", "origin2",
                  "LifeSatG", "IllAff7", "ILL12m", "MENHTAKg2",
                  "AntiDepTakg2", "AntiDepM2", "topqual3", "RELIGSC", "HHINC3",
                  "eqv5", "GHQ", "Anxiet17g3", "MVPATert")

  # File to save outputs to
  file.create("mediation_results.txt")
  output_file <- "mediation_results.txt"
  message("Created output file")

  while (sink.number() > 0) {
    sink()
  }

  # Open the file for writing
  sink(output_file)
  message("Opened output file")

  for (mediator in mediators){

    df_clean <- df_filtered %>%
      filter(!is.na(BMI) & !is.na(mediator))

    message(summary(df_clean$BMI))
    message(summary(df_clean$GHQ12Scr))
    # formula <- as.formula(paste("df_clean$", mediator))
    message(summary(df[[mediator]]))
    message("here")

    # Step 1: Model the effect of BMI on mediator
    mediator_model <- lm(mediator ~ BMI, data = df_clean)

    # Step 2: Model the effect of BMI and mediator on GHQ
    outcome_model <- lm(GHQ12Scr ~ BMI + mediator, data = df_clean)

    cat("\n\n--- Results for Mediator:", mediator, "---\n\n")

    # Step 3: Test for mediation
    mediate_effect <- mediate(mediator_model, outcome_model, treat = "BMI", mediator = "mediator", boot = TRUE)

    # Capture the summary output
    model_summary <- capture.output(summary(mediate_effect))

    # Save the summary to a file
    cat(model_summary)
    message("Completed for mediator:", mediator)
  }

  sink()

  file.show("mediation_results.txt")

}
