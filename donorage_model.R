## libraries
library(mgcv)

## confounder model
liver <- readRDS("./procdata/modData.Rds")
mod.confounder <- readRDS("./models/confounderModel.Rds")

## add smooth esimate of donor.age effect to confounder model
mod.age <- update(mod.confounder, .~. + s(donor.age))

saveRDS(mod.age, "./models/donorageMod.Rds")
