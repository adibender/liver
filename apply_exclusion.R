# @Author: andreas.bender@stat.uni-muenchen.de
# @Date:   2016-12-08 20:45:08
# @Last Modified by:   andreas.bender@stat.uni-muenchen.de
# @Last Modified time: 2016-12-16 15:58:35

## libraries
library(checkmate)
library(magrittr)
library(tidyverse)

## read in data from first preprocessing step
liver <- readRDS("./procdata/liver.Rds")
rm.df <- readRDS("./procdata/rmDF.Rds")

## Exclusion creteria donor specific
# Only 16 years and older donors
liver %<>% filter(donor.age >= 16)
rm.df$donorAge16 <- nrow(liver)

# only donors with heart beating (HB)
liver %<>% filter(donor.type == "HB")
rm.df$donorTypeHB <- nrow(liver)


## Exclusion criteria recipient specific
# only 16 years and older recipients
liver %<>% filter(recipient.age >= 16)
rm.df$recipientAge16 <- nrow(liver)
# after this exclusion all non-transplanted livers are also excluded
stopifnot(sum(liver$final.status.of.liver == "Transplanted")==nrow(liver))

# no recipients with urgency NT or U
liver %<>% filter(!(Urgency.at.txp %in% c("NT", "U")))
rm.df$UrgencyNTorU <- nrow(liver)


## no livers that were split (as discussed with Dr. Pratschke on 09.12.2014)
liver %<>% filter(organ == "WLiv")
rm.df$organWLiv <- nrow(liver)

## implausible time to procurrement
liver %<>% filter(!(proc.to.transp <= 0))
rm.df$procToTransplantTimeLess0 <- nrow(liver)
liver %<>% filter(!(proc.to.transp > 1))
rm.df$procToTransplantTimelOver1 <- nrow(liver)

## consider only first transplantation
liver %<>% filter(type.of.listing=="First txp") %>%
	select(-type.of.listing)
## as we only consider first transplantations there should be no observations
# left with graft failure
liver %<>% filter(!grepl(".*graft failure*", recipient.diag))
rm.df$retransplant <- nrow(liver)




## exclude all observations where any information given after death
liver %<>%
  filter(death - lastseen >= 0 | is.na(death - lastseen)) %>%
  filter(death - fail     >= 0 | is.na(death - fail))     %>%
  filter(death - retrans  >= 0 | is.na(death - retrans))  %>%
  filter(death - reregis  >= 0 | is.na(death - reregis))
rm.df$negativeDiffDeath <- nrow(liver)
# same for reregistration after retrans and failure time after retrans
liver %<>%
  filter(retrans - reregis  >= 0 | is.na(retrans - reregis))  %>%
  filter(retrans - fail  >= 0 | is.na(retrans - fail))
rm.df$negativeDiffRetransFail <- nrow(liver)

## exclude patients with death = 0, assuming they died during operation
liver %<>% filter((death != 0 | is.na(death)))
rm.df$deathAt0 <- nrow(liver)

## exclude patients with retrans = 0 for same reason
liver %<>% filter((retrans != 0 | is.na(retrans)))
rm.df$retransAt0 <- nrow(liver)


## eclude year 2014 as events in 2014 may not have had enough time to be
# submitted and included in the central data base
liver %<>% filter(transplant.year != 2014)
rm.df$not2014 <- nrow(liver)


## exclusion of individual observations
# height below 25
liver %<>% filter(recipient.height >= 25, donor.height >= 25)
rm.df$heightBelow25 <- nrow(liver)

## BMI between 15 and 40
liver %<>% filter(donor.BMI >=15, donor.BMI <=40, recipient.BMI >=15,
	recipient.BMI <=40)
rm.df$BMIextreme <- nrow(liver)

## Remove extreme values for last* variables
liver <- filter(liver, last.GGT < 1000, last.SGOT < 1000, last.SGPT < 1000)
rm.df$extremeLast <- nrow(liver)

## Remove extreme values for admission to ICU
liver <- filter(liver, adm.to.ICU < 100)
rm.df$admissionExtreme <- nrow(liver)

## remove variables not needed any more
vars.remove <- c("donor.type", "final.status.of.liver", "organ",
	"recipient.height", "donor.height")
liver %<>% select(-one_of(vars.remove))
assert_factor(liver$recipientID, unique=TRUE)

## clean up and save
liver <- droplevels(liver)
saveRDS(liver, "./procdata/liversub.Rds")
saveRDS(rm.df, "./procdata/rmDF.Rds")
