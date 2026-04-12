library(readxl)

# -------------------------------
# STEP 0: Load data
# -------------------------------
df <- read_excel("D:/Research/Data.xlsx")

# -------------------------------
# STEP 1: Create datasets
# -------------------------------
crop <- df[, c("CropSense_Revenue",
               "CropSense_Price",
               "CropSense_Digital_Ad",
               "CropSense_Traditional_Ad",
               "Ag_Commodity_Index",
               "Household_Income")]

fresh <- df[, c("FreshConnect_Revenue",
                "FreshConnect_Price",
                "FreshConnect_Digital_Ad",
                "Household_Income",
                "Population_Density")]

# -------------------------------
# STEP 2: Simple Regression + R² Table
# -------------------------------

# CropSense models
m1 <- lm(CropSense_Revenue ~ CropSense_Price, data = crop)
m2 <- lm(CropSense_Revenue ~ CropSense_Digital_Ad, data = crop)
m3 <- lm(CropSense_Revenue ~ CropSense_Traditional_Ad, data = crop)
m4 <- lm(CropSense_Revenue ~ Household_Income, data = crop)
m5 <- lm(CropSense_Revenue ~ Ag_Commodity_Index, data = crop)

# FreshConnect models
f1 <- lm(FreshConnect_Revenue ~ FreshConnect_Price, data = fresh)
f2 <- lm(FreshConnect_Revenue ~ FreshConnect_Digital_Ad, data = fresh)
f3 <- lm(FreshConnect_Revenue ~ Household_Income, data = fresh)
f4 <- lm(FreshConnect_Revenue ~ Population_Density, data = fresh)

# R² Comparison Table
r2_table <- data.frame(
  Variable = c("Price", "Digital Ads", "Traditional Ads", "Income", "Commodity Index / Population"),
  CropSense_R2 = c(summary(m1)$r.squared,
                   summary(m2)$r.squared,
                   summary(m3)$r.squared,
                   summary(m4)$r.squared,
                   summary(m5)$r.squared),
  FreshConnect_R2 = c(summary(f1)$r.squared,
                      summary(f2)$r.squared,
                      NA,
                      summary(f3)$r.squared,
                      summary(f4)$r.squared)
)

print("R² Comparison Table:")
print(r2_table)

# -------------------------------
# STEP 3: Multiple Regression
# -------------------------------

model_crop <- lm(CropSense_Revenue ~ 
                   CropSense_Price +
                   CropSense_Digital_Ad +
                   CropSense_Traditional_Ad +
                   Household_Income +
                   Ag_Commodity_Index,
                 data = crop)

model_fresh <- lm(FreshConnect_Revenue ~ 
                    FreshConnect_Price +
                    FreshConnect_Digital_Ad +
                    Household_Income +
                    Population_Density,
                  data = fresh)

summary(model_crop)
summary(model_fresh)

# -------------------------------
# STEP 4: Adjusted R²
# -------------------------------

cat("Adjusted R² CropSense:", summary(model_crop)$adj.r.squared, "\n")
cat("Adjusted R² FreshConnect:", summary(model_fresh)$adj.r.squared, "\n")

# -------------------------------
# STEP 5: Confidence Intervals
# -------------------------------

cat("\nConfidence Intervals - CropSense:\n")
print(confint(model_crop))

cat("\nConfidence Intervals - FreshConnect:\n")
print(confint(model_fresh))

# -------------------------------
# STEP 6: Price Elasticity
# -------------------------------

coef_price_crop <- coef(model_crop)["CropSense_Price"]
elasticity_crop <- coef_price_crop * 
  (mean(crop$CropSense_Price) / mean(crop$CropSense_Revenue))

coef_price_fresh <- coef(model_fresh)["FreshConnect_Price"]
elasticity_fresh <- coef_price_fresh * 
  (mean(fresh$FreshConnect_Price) / mean(fresh$FreshConnect_Revenue))

cat("\nPrice Elasticity CropSense:", elasticity_crop, "\n")
cat("Price Elasticity FreshConnect:", elasticity_fresh, "\n")

# -------------------------------
# STEP 7: Economic Logic Check
# -------------------------------

cat("\nEconomic Logic Check:\n")
cat("CropSense Price Coefficient:", coef(model_crop)["CropSense_Price"], "(Should be negative)\n")
cat("FreshConnect Price Coefficient:", coef(model_fresh)["FreshConnect_Price"], "(Should be negative)\n")

# -------------------------------
# STEP 8: Scenario Analysis
# -------------------------------

baseline <- mean(crop$CropSense_Revenue)

# Scenario A
predicted_A <- predict(model_crop, newdata = data.frame(
  CropSense_Price = mean(crop$CropSense_Price) * 0.9,
  CropSense_Digital_Ad = mean(crop$CropSense_Digital_Ad) * 1.25,
  CropSense_Traditional_Ad = mean(crop$CropSense_Traditional_Ad),
  Household_Income = mean(crop$Household_Income),
  Ag_Commodity_Index = mean(crop$Ag_Commodity_Index)
))

# Scenario B
predicted_B <- predict(model_crop, newdata = data.frame(
  CropSense_Price = mean(crop$CropSense_Price),
  CropSense_Digital_Ad = mean(crop$CropSense_Digital_Ad) * 0.85,
  CropSense_Traditional_Ad = mean(crop$CropSense_Traditional_Ad) * 0.85,
  Household_Income = mean(crop$Household_Income) * 0.98,
  Ag_Commodity_Index = mean(crop$Ag_Commodity_Index) * 1.05
))

# % Change Calculation
change_A <- ((predicted_A - baseline) / baseline) * 100
change_B <- ((predicted_B - baseline) / baseline) * 100

cat("\nBaseline Revenue:", baseline, "\n")
cat("Scenario A Revenue:", predicted_A, " | Change:", change_A, "%\n")
cat("Scenario B Revenue:", predicted_B, " | Change:", change_B, "%\n")

# -------------------------------
# STEP 9: Most Important Variable
# -------------------------------

cat("\nMost Important Variable:\n")
cat("CropSense:", r2_table$Variable[which.max(r2_table$CropSense_R2)], "\n")
cat("FreshConnect:", r2_table$Variable[which.max(na.omit(r2_table$FreshConnect_R2))], "\n")