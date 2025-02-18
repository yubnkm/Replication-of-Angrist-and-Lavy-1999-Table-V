#----------------------------------------------------------
# main.R
# Replicating Table 5 in Angrist and Lavy (1999)
#----------------------------------------------------------
renv::activate()
renv::install("AER")
renv::install("sandwich")
renv::install("lmtest")
renv::install("plm")

library(data.table)
library(AER)
library(sandwich)
library(lmtest)
library(plm)

# Load the cleaned data
df <- fread("final_data_clean.csv")

# Replicate Table 5

for (dvar in c("avgverb", "avgmath")) {
cat("\n============================================================\n")
cat("Outcome is ", dvar, " in FULL SAMPLE\n")
if(dvar == "avgverb") {
    cat("Reading Comprehension (avgverb): (1) - (4) \n")
} else {
    cat("Math (avgmath): (7) - (10) \n")
}
cat("============================================================\n")

form1 <- as.formula(paste(dvar, "~ classize + tipuach | func1 + tipuach"))
ivreg1 <- ivreg(form1, data=df)
vcov_cl1 <- vcovCL(ivreg1, cluster = ~ schlcode, type = "HC1") # school code clustered errors
print(coeftest(ivreg1, vcov = vcov_cl1))
cat(sprintf("Root MSE: %.3f\n", summary(ivreg1)$sigma))
cat(paste("N: ", ivreg1$nobs, "\n"))

form2 <- as.formula(paste(dvar, "~ classize + tipuach + c_size | func1 + tipuach + c_size"))
ivreg2 <- ivreg(form2, data=df)
vcov_cl2 <- vcovCL(ivreg2, cluster = ~ schlcode, type = "HC1")
print(coeftest(ivreg2, vcov = vcov_cl2))
cat(sprintf("Root MSE: %.3f\n", summary(ivreg2)$sigma))
cat(paste("N: ", ivreg2$nobs, "\n"))

form3 <- as.formula(paste(dvar, "~ classize + tipuach + c_size + c_size2 | func1 + tipuach + c_size + c_size2"))
ivreg3 <- ivreg(form3, data=df)
vcov_cl3 <- vcovCL(ivreg3, cluster = ~ schlcode, type = "HC1")
print(coeftest(ivreg3, vcov = vcov_cl3))
cat(sprintf("Root MSE: %.3f\n", summary(ivreg3)$sigma))
cat(paste("N: ", ivreg3$nobs, "\n"))

form4 <- as.formula(paste(dvar, "~ classize + trend | func1 + trend"))
ivreg4 <- ivreg(form4, data=df)
vcov_cl4 <- vcovCL(ivreg4, cluster = ~ schlcode, type = "HC1")
print(coeftest(ivreg4, vcov = vcov_cl4))
cat(sprintf("Root MSE: %.3f\n", summary(ivreg4)$sigma))
cat(paste("N: ", ivreg4$nobs, "\n"))

}

for (dvar in c("avgverb", "avgmath")) {
cat("\n============================================================\n")
cat("Outcome is ", dvar, " in DISCONTINUOUS SAMPLE\n")
if(dvar == "avgverb") {
    cat("Reading Comprehension (avgverb): (5), (6) \n")
} else {
    cat("Math (avgmath): (11), (12) \n")
}
cat("============================================================\n")
df_discont <- df[disc==1]

form1_d <- as.formula(paste(dvar, "~ classize + tipuach | func1 + tipuach"))
ivreg1_d <- ivreg(form1_d, data=df_discont)
vcov_cl1_d <- vcovCL(ivreg1_d, cluster = ~ schlcode, type = "HC1")
print(coeftest(ivreg1_d, vcov = vcov_cl1_d))
cat(sprintf("Root MSE: %.3f\n", summary(ivreg1_d)$sigma))
cat(paste("N: ", ivreg1_d$nobs, "\n"))

form2_d <- as.formula(paste(dvar, "~ classize + tipuach + c_size | func1 + tipuach + c_size"))
ivreg2_d <- ivreg(form2_d, data=df_discont)
vcov_cl2_d <- vcovCL(ivreg2_d, cluster = ~ schlcode, type = "HC1")
print(coeftest(ivreg2_d, vcov = vcov_cl2_d))
cat(sprintf("Root MSE: %.3f\n", summary(ivreg2_d)$sigma))
cat(paste("N: ", ivreg2_d$nobs, "\n"))

}
