# @Author: andreas.bender@stat.uni-muenchen.de
# @Date:   2016-12-08 16:32:17
# @Last Modified by:   andreas.bender@stat.uni-muenchen.de
# @Last Modified time: 2017-04-10 17:36:31

## libraries
library(checkmate) # input/format checks
library(magrittr)
library(stringr)
library(tidyverse)
source("./functions/helperFunctions.R")

## read in original data (17.03.2015) (updated data with corrected entries)
liver <- read.csv("./raw/Leberdaten17032015.csv", sep=";", header=TRUE, dec=".")
rm.df <- data.frame(orig = nrow(liver))
## read in original data (17.11.2014) (has additional variables that we need)
liverold <- read.csv("./raw/Leberdaten17112014.csv", sep=",", header=TRUE, dec=".")
# add "Diagnosis.new.category" and "donor.cause.of.death.new.category"
# from old data to recent dataset
liver$row.id <- paste0(liver$dummy.donor.number, liver$dummy.recipient.number)
liverold$row.id <- paste0(liverold$dummy.donor.number, liverold$dummy.recipient.number)

liver <- left_join(liver,liverold[, c("row.id","Diagnosis.new.category",
	"donor.cause.of.death.new.category")], by=c("row.id"="row.id")) %>%
	rename(
		recipient.diag = Diagnosis.new.category,
		donor.cod      = donor.cause.of.death.new.category)

assert_data_frame(liver, nrow=nrow(liverold))
rm(liverold)

## Check levels, set empty levels to missing
# donor cause of death
liver$donor.cod[liver$donor.cod==""] <- NA
liver$donor.cod[liver$donor.cod=="Hypoxia "] <- "Hypoxia"
liver <- droplevels(liver)

## recipient diagnosis
liver$recipient.diag[liver$recipient.diag==""] <- NA
liver$recipient.diag[liver$recipient.diag %in%
	c("Other ", "Other   ")] <- "Other"
liver$recipient.diag[liver$recipient.diag == "HCC"] <- "Other malignancy"
liver$recipient.diag <- as.character(liver$recipient.diag)
liver$recipient.diag <- ifelse(liver$recipient.diag=="Other malignancy", "Malignancy",
	liver$recipient.diag)
liver$recipient.diag <- as.factor(liver$recipient.diag)

liver <- droplevels(liver)

## donor/recipient weight was read in wrongly b/c of inconsistent decimal point
# donor
str(liver$donor.weight)
liver$donor.weight[liver$donor.weight == ""] <- NA
liver <- mutate(liver,
	donor.weight = sub(",", ".", donor.weight),
	donor.weight = as.numeric(donor.weight))
assert_numeric(liver$donor.weight, lower=0, finite=TRUE, all.missing=FALSE)
# recipient
str(liver$recipient.weight)
liver$recipient.weight[liver$recipient.weight == ""] <- NA
liver <- mutate(liver,
	recipient.weight = sub(",", ".", recipient.weight),
	recipient.weight = as.numeric(recipient.weight))
assert_numeric(liver$recipient.weight, lower=0, finite=TRUE, all.missing=FALSE)

## recode time from procurement to transplantation
str(liver$time.from.procurement.to.transplant..days.)
liver %<>%
	rename_(.dots=setNames("time.from.procurement.to.transplant..days.", "proc.to.transp"))
liver$proc.to.transp[liver$proc.to.transp%in% c("", "unk")] <- NA
liver %<>% mutate(
		proc.to.transp = sub(",", ".", proc.to.transp),
		proc.to.transp = as.numeric(proc.to.transp))
assert_numeric(liver$proc.to.transp, finite=TRUE, all.missing=FALSE)

## recode further vars
str(liver$last.noradrenaline..gamma.)
# levels(liver$last.noradrenaline..gamma.)
liver$last.noradrenaline..gamma.[liver$last.noradrenaline..gamma.  %in% c("")] <- NA
liver <- mutate(liver,
	noradrenaline = sub(",", ".", last.noradrenaline..gamma.),
	noradrenaline = as.numeric(noradrenaline))
assert_numeric(liver$noradrenaline, lower=0, finite=TRUE, all.missing=FALSE)
str(liver$last.adrenaline..gamma.)
# levels(liver$last.adrenaline..gamma.)
liver$last.adrenaline..gamma.[liver$last.adrenaline..gamma.  %in% c("")] <- NA
liver <- mutate(liver,
	adrenaline = sub(",", ".", last.adrenaline..gamma.),
	adrenaline = as.numeric(adrenaline))
assert_numeric(liver$adrenaline, lower=0, finite=TRUE, all.missing=FALSE)

## last SGOT
str(liver$last.SGPT)
liver$last.SGPT[liver$last.SGPT  %in% c("")] <- NA
liver <- mutate(liver,
	last.SGPT = sub(",", ".", last.SGPT),
	last.SGPT = as.numeric(last.SGPT),
	log.SGPT  = log(last.SGPT + 1))
assert_numeric(liver$last.SGPT, lower=0, finite=TRUE, all.missing=FALSE)
assert_numeric(liver$log.SGPT, finite=TRUE, all.missing=FALSE)

## last SGOT
str(liver$last.SGOT)
liver$last.SGOT[liver$last.SGOT  %in% c("")] <- NA
liver <- mutate(liver,
	last.SGOT = sub(",", ".", last.SGOT),
	last.SGOT = as.numeric(last.SGOT),
	log.SGOT = log(last.SGOT+1))
assert_numeric(liver$last.SGOT, lower=0, finite=TRUE, all.missing=FALSE)
assert_numeric(liver$log.SGOT, finite=TRUE, all.missing=FALSE)
## last GGT
str(liver$last.GGT)
liver$last.GGT[liver$last.GGT  %in% c("")] <- NA
liver <- mutate(liver,
	last.GGT = sub(",", ".", last.GGT),
	last.GGT = as.numeric(last.GGT),
	log.GGT  = log(last.GGT+1))
assert_numeric(liver$last.GGT, lower=0, all.missing=FALSE)
assert_numeric(liver$log.GGT, finite=TRUE, all.missing=FALSE)

# Data correction for dummy.donor.number=="d_13764"
# looks like height and weight have been flipped
ind.d_13764 <- liver$dummy.donor.number == "d_13764"
liver[ind.d_13764, c("donor.height", "donor.weight")] <-
	rev(liver[ind.d_13764, c("donor.height", "donor.weight")])


## new/transformed variables (for descriptives mainly)
# donor year month
ym <- do.call(rbind, str_split(liver$donor.registration.month, "_"))
liver$donor.year  <- as.factor(ym[, 1])
liver$donor.month <- as.factor(ym[, 2])
# year/month of donor registration as numeric
liver$donor.year.num  <- as.numeric(ym[, 1])
liver$donor.month.num <- as.numeric(ym[, 2])
assert_numeric(liver$donor.year.num, lower=2000, upper=20014, any.missing=FALSE)
assert_numeric(liver$donor.month.num, lower=1, upper=12, any.missing=FALSE)

## year and month of transplantation
liver$transplant.date <- liver$transplant.month
ym.transplant <- do.call(rbind, str_split(liver$transplant.month, "_"))
# as factor
liver$transplant.year  <- as.factor(ym.transplant[, 1])
liver$transplant.month <- as.factor(ym.transplant[, 2])
assert_factor(liver$transplant.year, levels=c("", as.character(2000:2014)))
assert_factor(liver$transplant.month, levels=c("", paste0(0, 1:9), 10:12))
## calculate the maximally possible censoring time
# will be used later to calculate censoring time for patients for which
# no other information than transplantation month is available
liver %<>%
	mutate(
		study.end = as.POSIXct(strptime("2013-12-31", format="%Y-%m-%d")),
		transp = as.POSIXct(strptime(
			x      = paste0(sub("_", "-", as.character(transplant.date)), "-15"),
			format = "%Y-%m-%d")),
		max.censtime = round(as.numeric(study.end - transp))) %>%
	select(-study.end, -transp)
## negative values will be excluded later, as we will only consider observations
# before the end of 2013

## Create indicator variable for Octogenerians
liver$donor80.indicator <- ifelse(liver$donor.age >= 80, 1L, 0L)
assert_integer(liver$donor80.indicator, lower=0, upper=1, any.missing=FALSE)

## Calculate BMI m/l^2 (m=weight in kg, l = length in meters)
# donor
liver %<>% mutate(donor.BMI = donor.weight/(donor.height/100)^2)
assert_numeric(liver$donor.BMI, lower=3, upper=250, all.missing=FALSE)
# recipient
liver %<>% mutate(recipient.BMI = recipient.weight/(recipient.height/100)^2)
## for some individuals weight/height appear to have been switched
# switch back
liver[which(liver$recipient.BMI > 80),
  c("dummy.recipient.number", "recipient.height", "recipient.weight")]
temp.rec.num <- liver[which(liver$recipient.BMI > 80), c("dummy.recipient.number")]
temp.r.hei   <- liver$recipient.height[liver$dummy.recipient.number %in% temp.rec.num]
liver$recipient.height[liver$dummy.recipient.number %in% temp.rec.num] <-
  liver$recipient.weight[liver$dummy.recipient.number %in% temp.rec.num]
liver$recipient.weight[liver$dummy.recipient.number %in% temp.rec.num] <- temp.r.hei
## recalculate recipient BMI
liver %<>% mutate(recipient.BMI = recipient.weight/(recipient.height/100)^2)
assert_numeric(liver$recipient.BMI, lower=2, upper=370, all.missing=FALSE)


## Transportation time
# use observations that have entries for either hours or minutes
liver$cold.ischemia.min[!is.na(liver$cold.ischemia.hrs) &
	is.na(liver$cold.ischemia.min)] <- 0
liver$cold.ischemia.hrs[is.na(liver$cold.ischemia.hrs) &
	!is.na(liver$cold.ischemia.min)] <- 0

# numeric ischemia time
liver$cold.ischemia <- liver$cold.ischemia.hrs + liver$cold.ischemia.min/60
assert_numeric(liver$cold.ischemia, lower=0, upper=80.5, all.missing=FALSE)

## create high urgency indicator
## Urgency
liver$Urgency <- NA
liver$Urgency[which(liver$Urgency.at.txp=="HU")] <- 1
liver$Urgency[!(is.na(liver$Urgency.at.txp) | liver$Urgency.at.txp=="HU")] <- 0

## rename variables for convenience
re.names <- setNames(
	c(
		"admission.to.ICU.xx.days.before.registration",
		"Death.xx.days.after.transplant",
		"Faildate.xx.days.after.transplant",
		"Date.Last.Seen.xx.days.after.transplant",
		"Reregistration.xx.days.after.transplant",
		"First.retxp.xx.days.after.this.txp",
		"dummy.donor.number",
		"dummy.recipient.number"),
	c(
		"adm.to.ICU",
		"death",
		"fail",
		"lastseen",
		"reregis",
		"retrans",
		"donorID",
		"recipientID"))
liver %<>% rename_(.dots=re.names)


## NOTE: later we will exclude observations where only failure time is given
# and not use the reregistration variable for calculation
# when reregistration occurs after declared failure time and no retransplantation
# time available, we replace the failure time with the reregistration time
liver %<>%
	mutate(
		repind = (fail - reregis) < 0 & !is.na(fail -reregis) & is.na(retrans),
		fail  = ifelse(repind, reregis, fail)) %>%
	select(-repind)

# when no failure time specified and reregistration time available we
# set the failure time to reregistration time
liver %<>%
	mutate(
		repind = is.na(fail) & !is.na(reregis) & is.na(retrans),
		fail = ifelse(repind, reregis, fail)) %>%
	select(-repind)

## keep only relevant variables
recip.vars <- c(
  "recipient.age","recipient.sex","recipient.BMI",
  "transplant.month", "transplant.year", "transplant.date",
  "noradrenaline","adrenaline",
  "recipientID","recipient.diag", "recipient.height",
  "max.censtime",
  "last.SGPT","last.SGOT","last.GGT",
  "log.SGPT", "log.SGOT", "log.GGT",
  "Urgency.at.txp", "Urgency","type.of.listing",
  "adm.to.ICU", "LAB.MELD")
donor.vars <- c(
  "donor.age","donor.BMI","donor.sex", "donor.height",
  "donor.month", "donor.year","donor.cod",
  "donorID", "donor80.indicator", "donor.month.num", "donor.year.num",
  "donor.type", "final.status.of.liver",
  "organ", "perfusion.fluid")
other.vars <- c(
  "cold.ischemia", "proc.to.transp",
  "dummy.transplant.center","transplant.center.in")
event.vars <- c("death","fail","lastseen","reregis","retrans")

mod.vars <- c(recip.vars, donor.vars, other.vars, event.vars)

liver %<>% select_(.dots=mod.vars) %>% tbl_df()

## reorder by recipient id
liver %<>% arrange(recipientID, transplant.year, transplant.month)


## save imported and preprocessed data
saveRDS(liver, "./procdata/liver.Rds")
saveRDS(rm.df, "./procdata/rmDF.Rds")
