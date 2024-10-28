
# Load required packages --------------------------------------------------
library(tidyverse)
library(haven)

hse_2018_in <- read_sav("C:/Users/Becky.Crossley/Department of Health and Social Care/GOV-Healthy Weight, Food & Nutrition Analysis - Documents/Data/HSfE/Raw/2018/UKDA-8649-spss/spss/spss25/hse_2018_eul_22052020.sav")

# Get the column names
column_names <- colnames(hse_2018_in)

# Save the column names to a text file
write(column_names, file = "column_names_hse_2018.txt")

# Reduce to data we want --------------------------------------------
# Select the specific columns
hse18red <- hse_2018_in %>%
  select(nssec8, qimd, BMI, limlast, BMIOK, GHQ, age16g5, GHQ12Scr, Anxiet17g3, MVPATert, Sex, ag16g10, BMIvg5, SCSatis, origin2, LifeSatG, IllAff7, ILL12m, MENHTAKg2, AntiDepTakg2, AntiDepM2, topqual3, RELIGSC, HHINC3, eqv5)

#Relabel df for relabelling factors
hse18lab <- hse18red

# Clean data to label correctly
# Modify factor levels for Depravity
hse18lab$qimd <- factor(hse18lab$qimd, levels = c("1", "2", "3", "4", "5"),
                          labels = c("0.48->8.37 (Least Deprived)", "8.37->13.92", "13.92->21.43", "21.43->33.88", "33.88->92.60 (Most Deprived)"))
# Modify factor levels for BMI
hse18lab$BMIvg5 <- factor(hse18lab$BMIvg5, levels = c("1", "2", "3", "4", "5"),
                                labels = c("Underweight", "Normal", "Overweight", "Obese", "Morbidly Obese"))
#Modify factor levels for Sex
hse18lab$Sex <- factor(hse18lab$Sex, levels = c("1", "2"),
                       labels = c("Female", "Male"))
# Modify factor levels for Age groups
hse18lab$ag16g10 <- factor(hse18lab$ag16g10, levels = c("1", "2", "3", "4", "5", "6", "7"),
                           labels = c("16-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+"))
# Modify factor levels for life satisfaction
hse18lab$LifeSatG <- factor(hse18lab$LifeSatG, levels = c("1", "2", "3", "4"),
                            labels = c("Low (0-4)", "Medium (5-6)", "High (7-8)", "Very high (9-10)"))
# Modify factor levels for whether mental health drug was taken in last week
hse18lab$MENHTAKg2 <- factor(hse18lab$MENHTAKg2, levels = c("0", "1"),
                             labels = c("0", "1+"))
# Modify factor levels for whether prescribed drug for depression was taken in last week
hse18lab$AntiDepTakg2 <- factor(hse18lab$AntiDepTakg2, levels = c("0", "1"),
                                labels = c("0", "1+"))
# Modify factor levels for whether prescribed drug for depression
hse18lab$AntiDepM2 <- factor(hse18lab$AntiDepM2, levels = c("0", "1"),
                             labels = c("Not taking", "Taking"))
# Modify factor levels for highest level of qualification
hse18lab$topqual3 <- factor(hse18lab$topqual3, levels = c("1", "2", "3", "4", "5", "6", "7"),
                            labels = c("NVQ4/NVQ5/Degree or equiv", "Higher ed below degree", "NVQ3/GCE A Level equiv", "NVQ2/GCE O Level equiv", "NVQ1/CSE other grade equiv", "Foreign/other", "No qualification"))
# Modify factor levels for quintiles for total household income
hse18lab$eqv5 <- factor(hse18lab$eqv5, levels = c("1", "2", "3", "4", "5"),
                        labels = c( "Lowest Quintile (<=£14,956)", "Second lowest Quintile (>£14,956 <= £23,443)", "Middle Quintile (>£23,443 <=£35,540)", "Second highest Quintile (>£35,540 <=£56,000)", "Highest Quintile (>£56,000)"))
# Modify factor levels for general health on day: anxiety/depression
hse18lab$Anxiet17g3 <- factor(hse18lab$Anxiet17g3, levels = c("1", "2", "3"),
                       labels = c("Not anxious or depressed", "Slightly or moderately anxious or depressed", "Severely or extremely anxious or depressed"))
# Modify factor levels for activity per week
hse18lab$MVPATert <- factor(hse18lab$MVPATert, levels = c("1", "2", "3"),
                       labels = c("Low", "Medium", "High"))
# Modify factor levels for GHQ-12 score bands
hse18lab$GHQ <- factor(hse18lab$GHQ, levels = c("0", "1"),
                       labels = c("Score 0-3", "Score 4+"))
