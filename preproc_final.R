# @Author: andreas.bender@stat.uni-muenchen.de
# @Date:   2016-12-09 12:16:48
# @Last Modified by:   andreas.bender@stat.uni-muenchen.de
# @Last Modified time: 2017-06-01 11:09:38

## libraries
library(checkmate)
library(magrittr)
library(tidyverse)

liver <- readRDS("./procdata/liversub.Rds")
rm.df <- readRDS("./procdata/rmDF.Rds")

## rename factor labels
# event
liver %<>%
	mutate(event = factor(status, levels=c(0,1), labels=c("censored", "failed")))
# recipient and donor sex
liver %<>%
	mutate(
    recipient.sex = factor(
    	recipient.sex,
    	levels=c("M", "F"),
    	labels=c("Male", "Female")),
    donor.sex = factor(
    	liver$donor.sex,
    	levels=c("M", "F"),
    	labels=c("Male", "Female")))
# urgency
liver %<>%
	mutate(
		Urgency = factor(Urgency, levels=c(1, 0), labels=c("high", "other"))) %>%
	select(-Urgency.at.txp)
# perfusion fluid (Email Dr. Pratschke 08.04.2015)
liver %<>% left_join(
	data.frame(
		perfusion.fluid = unique(liver$perfusion.fluid),
		Perfusion = as.factor(c(
			"None/Other", "(Modified) UW", "HTK/Bretschneider", "(Modified) UW",
			"None/Other", "HTK/Bretschneider", "None/Other", "None/Other")))) %>%
	select(-perfusion.fluid)

# center id
liver %<>% rename(centerID = dummy.transplant.center)
# elderly donor indicator
liver %<>% mutate(
		octogenerian = factor(
			donor80.indicator,
			levels=c(0, 1),
			labels=c("< 80", ">= 80")))

## create categorical versions of variables
#Donor age 3 categories
liver %<>%
	mutate(
		donorage.cat = cut(
			x              = donor.age,
			breaks         = c(seq(16, 86, by=10), Inf),
			include.lowest = TRUE,
			right          = FALSE),
		recipientage.cat = cut(
			x              = recipient.age,
			breaks         = c(16, 60, 80, Inf),
			include.lowest = TRUE,
			right          = FALSE))


#LAB-MELD
liver %<>%
	mutate(
	LAB.MELD.cat = cut(
		x              = LAB.MELD,
		breaks         = seq(5,40,by = 5),
		include.lowest = TRUE,
		right          = FALSE),
	LAB.MELD.3cat = cut(
			x              = LAB.MELD,
			breaks         = c(5,35,40),
			include.lowest = TRUE,
			right          = FALSE),
	LAB.MELD.3cat = addNA(LAB.MELD.3cat, ifany = FALSE), # NA als Faktor-Level
	LAB.MELD.Cat  = LAB.MELD.3cat)
levels(liver$LAB.MELD.Cat) <- c("< 35", ">= 35", "missing")


# Kalte Ischämiezeit
liver %<>%
	mutate(
		ischemia.cat = cut(
			x              = cold.ischemia,
			breaks         = c(0,5,10,15,20,Inf),
			include.lowest = TRUE,
			right          = FALSE))

# Time from Procurement to Transplant
liver %<>%
	mutate(
		proc.to.transp = proc.to.transp * 24,
		proc.cat = cut(
			x              = proc.to.transp,
			breaks         = c(0,5,10,15,20,Inf),
			include.lowest = TRUE,
			right          = FALSE))

#Intensivstationszeit
liver %<>%
	mutate(
		adm.to.ICU.cat = cut(
			x              = adm.to.ICU,
			breaks         = c(seq(0,30,by = 5),Inf),
			include.lowest = TRUE,
			right          = FALSE))


liver <- droplevels(liver)
## check all variables for valid values
# factors
assert_factor(liver$event,         levels = c("censored", "failed"), any.missing = FALSE)
assert_factor(liver$recipient.sex, levels = c("Male", "Female"),     any.missing = FALSE)
assert_factor(liver$donor.sex,     levels = c("Male", "Female"),     any.missing = FALSE)
assert_factor(liver$Urgency,       levels = c("high", "other"),      any.missing = FALSE)
assert_factor(liver$octogenerian,  levels = c("< 80", ">= 80"),      any.missing = FALSE)
assert_factor(liver$recipientage.cat,
	levels=c("[16,60)",  "[60,80)"), any.missing=FALSE)
assert_factor(liver$donorage.cat,
	levels=c("[16,26)",  "[26,36)", "[36,46)", "[46,56)", "[56,66)", "[66,76)","[76,86)", "[86,Inf]"), 
	any.missing=FALSE)
assert_factor(liver$Perfusion,
	levels=c("None/Other", "(Modified) UW", "HTK/Bretschneider"), any.missing=FALSE)
assert_factor(liver$LAB.MELD.cat,
	levels=c("[5,10)", "[10,15)", "[15,20)", "[20,25)", "[25,30)", "[30,35)", "[35,40]"),
	all.missing=FALSE)
assert_factor(liver$LAB.MELD.Cat, levels=c("< 35",  ">= 35", "missing"),
	any.missing=FALSE)
assert_factor(liver$transplant.month, levels=c(paste0(0, 1:9), 10:12),
	any.missing=FALSE)
assert_factor(liver$transplant.year, levels=c(as.character(2002:2013)),
	any.missing=FALSE)
assert_factor(liver$donor.month, levels=c(paste0(0, 1:9), 10:12),
	any.missing=FALSE)
assert_factor(liver$donor.year, levels=c(as.character(2002:2013)),
	any.missing=FALSE)
assert_factor(liver$recipientID, unique=TRUE, any.missing=FALSE)

# remove observation with missing diagnosis
liver %<>% filter(!is.na(recipient.diag))
rm.df$noRecipientDiag <- nrow(liver)
assert_factor(liver$recipient.diag,
	levels=c("Acute liver failure", "ALCI", "BILE", "HCV", "Malignancy",
		"Metabolic", "Other", "Other cirrhosis", "Viral hepatitis"), any.missing=FALSE)

# create factor variable where acute liver failure is additionaly split
# by urgency
liver$recipient.diag2 <- as.character(liver$recipient.diag)
liver$recipient.diag2[liver$recipient.diag=="Acute liver failure" & liver$Urgency=="high"] <-
  "Acute liver failure (urgent)"
liver$recipient.diag2[liver$recipient.diag=="Acute liver failure" & liver$Urgency=="other"] <-
  "Acute liver failure (other)"
liver$recipient.diag2 <- as.factor(liver$recipient.diag2)

assert_factor(liver$recipient.diag2,
  levels=c("Acute liver failure (urgent)", "Acute liver failure (other)", "ALCI",
    "BILE", "HCV", "Malignancy", "Metabolic", "Other", "Other cirrhosis",
    "Viral hepatitis"), any.missing=FALSE)

# remove centers with only one observation
small.centers <- liver %>%
	group_by(centerID) %>%
	summarize(nobs=n()) %>%
	filter(nobs==1) %>% select(centerID)
liver %<>% anti_join(small.centers) %>% droplevels()
assert_factor(liver$centerID, n.levels=40, any.missing=FALSE)

# numerics
assert_numeric(liver$recipient.age, lower=16,  upper=78  , any.missing=FALSE)
assert_numeric(liver$donor.age,     lower=16,  upper=98  , any.missing=FALSE)
assert_numeric(liver$recipient.BMI, lower=2.4, upper=100 , any.missing=FALSE)
## remove observations with missing BMI
liver %<>% filter(!is.na(donor.BMI))
rm.df$noDonorBMI <- nrow(liver)
assert_numeric(liver$donor.BMI,     lower=14,  upper=60  , any.missing=FALSE)

assert_numeric(liver$noradrenaline, lower=0,   upper=166 , all.missing=FALSE)
assert_numeric(liver$adrenaline,    lower=0,   upper=10  , all.missing=FALSE)
assert_numeric(liver$max.censtime,  lower=16,  upper=5100, any.missing=FALSE)

## remove observations with missing SGPT/SGOT/GGT
liver %<>% filter(!is.na(last.SGPT))
rm.df$noSGPT <- nrow(liver) #1 octo
assert_numeric(liver$last.SGPT,     lower=0,   upper=8000, any.missing=FALSE)
liver %<>% filter(!is.na(last.SGOT))
rm.df$noSGOT <- nrow(liver) #2 octo
assert_numeric(liver$last.SGOT,     lower=0,   upper=6500, any.missing=FALSE)
liver %<>% filter(!is.na(last.GGT))
rm.df$noGGT <- nrow(liver) # 5 octo
assert_numeric(liver$last.GGT,      lower=0,   upper=2310, any.missing=FALSE)
assert_numeric(liver$log.SGPT,      lower=0,   upper=9,    any.missing=FALSE)
assert_numeric(liver$log.SGOT,      lower=0,   upper=9,    any.missing=FALSE)
assert_numeric(liver$log.GGT,       lower=0,   upper=9,    any.missing=FALSE)

## remove observations with missing admission to icu time
liver %<>% filter(!is.na(adm.to.ICU)) #10 octo
rm.df$noAdmToICU <- nrow(liver)
assert_numeric(liver$adm.to.ICU,     lower=0, upper=1500,  any.missing=FALSE)
assert_numeric(liver$LAB.MELD,       lower=6, upper=40  ,  all.missing=FALSE)
assert_numeric(liver$cold.ischemia,  lower=0, upper=24  ,  all.missing=FALSE)
assert_numeric(liver$proc.to.transp, lower=0, upper=24  ,  any.missing=FALSE)
assert_numeric(liver$death,          lower=1, upper=4202,  all.missing=FALSE)
assert_numeric(liver$lastseen,       lower=0, upper=4374,  all.missing=FALSE)
assert_numeric(liver$retrans,        lower=0, upper=5200,  all.missing=FALSE)
assert_numeric(liver$time,           lower=0, upper=5200,  any.missing=FALSE)

## clean up and save
liver  <- droplevels(liver)
liver %<>% mutate(ID = row_number())
liverm <- filter(liver, !is.na(LAB.MELD))
liverm <- droplevels(liverm)
saveRDS(liver, "./procdata/finalData.Rds")
saveRDS(liverm, "./procdata/modData.Rds")
