
# Load required packages --------------------------------------------------
library(tidyverse)
library(haven)

hse_2019_in <- read_sav("C:/Users/Becky.Crossley/Department of Health and Social Care/GOV-Healthy Weight, Food & Nutrition Analysis - Documents/Data/HSfE/Raw/2019/UKDA-8860-spss/spss/spss25/hse_2019_eul_20211006.sav")

# Get the column names
#column_names <- colnames(hse_2019_in)

# Save the column names to a text file
#write(column_names, file = "column_names_hse_2019.txt")

# print(head(hse_2019_in))

# Reduce to data we want --------------------------------------------
# Select the specific columns
hse_2019_in_reduced <- hse_2019_in %>%
  select(Sex, ag16g10, HSEYR, BMIvg5, ThCoAny, SCSatis, origin2, LifeSatG, wemwbs, IllAff7, ILL12m, MENHTAKg2, AntiDepTakg2, SCOFF2, qimd19, AntiDepM2, topqual3, RELIGSC, HHINC3, eqv5, totalwug_19)

#print(head(hse_2019_in_reduced))
#print(summary(hse_2019_in_reduced))

hse_2019_in_lab <- hse_2019_in_reduced

# Clean data to label correctly
# Modify factor levels for BMI
hse_2019_in_lab$BMIvg5 <- factor(hse_2019_in_lab$BMIvg5, levels = c("1", "2", "3", "4", "5"),
                                 labels = c("Underweight", "Normal", "Overweight", "Obese", "Morbidly Obese"))
# Modify factor levels for Sex
hse_2019_in_lab$Sex <- factor(hse_2019_in_lab$Sex, levels = c("1", "2"), labels = c("Female", "Male"))
# Modify factor levels for Age groups
hse_2019_in_lab$ag16g10 <- factor(hse_2019_in_lab$ag16g10, levels = c("1", "2", "3", "4", "5", "6", "7"), labels = c("16-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+"))
# Modify factor levels for Therapy
hse_2019_in_lab$ThCoAny <- factor(hse_2019_in_lab$ThCoAny, levels = c("0", "1"), labels = c("Not mentioned", "Mentioned"))
# Modify factor levels for life satisfaction
hse_2019_in_lab$LifeSatG <- factor(hse_2019_in_lab$LifeSatG, levels = c("1", "2", "3", "4"), labels = c("Low (0-4)", "Medium (5-6)", "High (7-8)", "Very high (9-10)"))
# Modify factor levels for whether mental health drug was taken in last week
hse_2019_in_lab$MENHTAKg2 <- factor(hse_2019_in_lab$MENHTAKg2, levels = c("0", "1"), labels = c("0", "1+"))
# Modify factor levels for whether prescribed drug for depression was taken in last week
hse_2019_in_lab$AntiDepTakg2 <- factor(hse_2019_in_lab$AntiDepTakg2, levels = c("0", "1"), labels = c("0", "1+"))
# Modify factor levels for whether prescribed drug for depression
hse_2019_in_lab$AntiDepM2 <- factor(hse_2019_in_lab$AntiDepM2, levels = c("0", "1"), labels = c("Not taking", "Taking"))
# Modify factor levels for ED screener, SCOFF
hse_2019_in_lab$SCOFF2 <- factor(hse_2019_in_lab$SCOFF2, levels = c("1", "2"), labels = c("Score 0-1", "Score 2+"))
# Modify factor levels for depravity
hse_2019_in_lab$qimd19 <- factor(hse_2019_in_lab$qimd19, levels = c("1", "5"), labels = c("Least Deprived", "Most Deprived"))
# Modify factor levels for highest level of qualification
hse_2019_in_lab$topqual3 <- factor(hse_2019_in_lab$topqual3, levels = c("1", "2", "3", "4", "5", "6", "7"), labels = c("NVQ4/NVQ5/Degree or equiv", "Higher ed below degree", "NVQ3/GCE A Level equiv", "NVQ2/GCE O Level equiv", "NVQ1/CSE other grade equiv", "Foreign/other", "No qualification"))
# Modify factor levels for quintiles for total household income
hse_2019_in_lab$eqv5 <- factor(hse_2019_in_lab$eqv5, levels = c("1", "2", "3", "4", "5"), labels = c( "Lowest Quintile (<=£14,956)", "Second lowest Quintile (>£14,956 <= £23,443)", "Middle Quintile (>£23,443 <=£35,540)", "Second highest Quintile (>£35,540 <=£56,000)", "Highest Quintile (>£56,000)"))
# Modify factor levels for alcohol units per week
hse_2019_in_lab$totalwug_19 <- factor(hse_2019_in_lab$totalwug_19, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"), labels = c("None drinker/not in last 12 months", "Non-zero, but under 1", "1-7", "Over 7-10", "Over 10-14", "Over 14-21", "Over 21-28", "Over 28-35", "Over 35-50", "Over 50"))
