# Source other scripts ----------------------------------------------------
upload <- new.env(); source("./R/upload.R", local = upload)

run_vif <- function(){
  df <- upload$hse18red

  # Clean data to remove NAs
  df_clean <- df %>% filter(!is.na(GHQ12Scr))
  df_clean1 <- df_clean[, !names(df_clean) %in% "ILL12m"]

  # Run a test plot of BMI status against GHQ12Scr
  df_cleanBMI <- df_clean1 %>% filter(!is.na(BMIvg5), BMIOK == 1)

  df_done <- df_cleanBMI[, !names(df_cleanBMI) %in% "BMIOK"]

  model <- lm(GHQ12Scr ~ ., data=df_done)
  print(summary(model))

  vif_values <- vif(model)
  print("VIF values:")
  print(vif_values)

  par(mar = c(5, 5, 5, 5))  # Adjust the margin size
  p <- barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue", cex.names = 0.6, las = 1) +
    abline(v = 5, lwd = 3, lty = 2)
  print(p)

  corrdata <- df_done[ , c("eqv5", "topqual3", "RELIGSC", "HHINC3", "AntiDepM2", "AntiDepTakg2", "MENHTAKg2", "BMIvg5", "LifeSatG", "IllAff7", "origin2", "SCSatis", "age16g5", "Anxiet17g3", "MVPATert", "limlast", "BMI", "GHQ", "ag16g10", "Sex", "qimd", "nssec8")]
  cormatrix <- cor(corrdata, use="pairwise.complete.obs")
  print(cormatrix)
  q <- image(cormatrix, main = "Correlation Matrix", col = colorRampPalette(c("blue", "white", "red"))(20))
  print(q)
}

#nssec8, qimd, BMI, limlast, BMIOK, GHQ, age16g5, GHQ12Scr, Anxiet17g3, MVPATert, Sex, ag16g10, BMIvg5, SCSatis, origin2, LifeSatG, IllAff7, ILL12m, MENHTAKg2, AntiDepTakg2, AntiDepM2, topqual3, RELIGSC, HHINC3, eqv5
