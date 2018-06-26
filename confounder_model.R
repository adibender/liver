## libraries
library(mgcv)

## data
liver <- readRDS("./procdata/modData.Rds")

## specify model formula
# -> categorical variables are included dummy coded with first level as reference
# -> continous variables are included as smooth terms
# -> Centers are included in form of a gaussian random effect
form.confounder <- time ~
  recipient.sex + donor.sex +
  recipient.diag2 + donor.cod +
  Perfusion +
  transplant.year +
  s(recipient.age) +
  s(proc.to.transp) +
  s(LAB.MELD) +
  s(adm.to.ICU) +
  s(last.SGOT) +
  s(last.GGT) +
  s(last.SGPT) +
  s(donor.BMI) +
  s(recipient.BMI) +
  s(centerID, bs = "re")


## fit model
mod.confounder <- gam(
  formula = form.confounder,
  data    = liver,
  family  = "cox.ph",
  weights = status)

saveRDS(mod.confounder, "./models/confounderModel.Rds")
