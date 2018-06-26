# @Author: andreas.bender@stat.uni-muenchen.de
# @Date:   2016-12-09 16:20:45
# @Last Modified by:   andreas.bender@stat.uni-muenchen.de
# @Last Modified time: 2016-12-21 11:14:36

# libraries
library(magrittr)
library(tidyverse)

## data
liver <- readRDS("procdata/liversub.Rds")
rm.df <- readRDS("./procdata/rmDF.Rds")

## create event time and event indicator variables (see also the README file)
# first we set the event time to retransplantation time when retransplantation
# time is given and consider it an event (status = 1)
liver %<>%
	mutate(
		status = NA,
		time   = retrans, # NA where retrans is not given
		status = ifelse(!is.na(retrans), 1, status), 
		tretrans = !is.na(retrans))
stopifnot(sum(is.na(liver$time))==sum(is.na(liver$status)))
## when retransplantation time is not available and death time is available,
# we set the event time to death time and status = 1
liver %<>%
	mutate(
		tdeath = is.na(retrans) & !is.na(death),
		time      = ifelse(tdeath, death, time),
		status    = ifelse(tdeath, 1, status))
stopifnot(sum(is.na(liver$time))==sum(is.na(liver$status)))

## when lastseen is given and nothing else, we consider lastseen as the
# censoring time and set status = 0
liver %<>%
	mutate(
		tlastseen = is.na(status) & is.na(fail) & is.na(reregis) & !is.na(lastseen),
		time      = ifelse(tlastseen, lastseen, time),
		status    = ifelse(tlastseen, 0, status))
stopifnot(all.equal(sum(rowSums(liver[, c("tretrans", "tdeath", "tlastseen")])), 
	sum(!is.na(liver$status))))
stopifnot(sum(is.na(liver$time))==sum(is.na(liver$status)))
## when neither time variable is given, we set administrative censoring at
# the end of 2013 (see import_raw.R and README)
liver %<>%
	mutate(
		all.na= is.na(status) & is.na(fail) & is.na(reregis) & is.na(lastseen) &
			!is.na(max.censtime),
		time      = ifelse(all.na, max.censtime, time),
		status    = ifelse(all.na, 0, status))
stopifnot(sum(is.na(liver$time))==sum(is.na(liver$status)))


## other cases will be removed as fail and reregis information is not reliable
liver %<>% filter(!is.na(status), time!=0)
rm.df$insufficientTimeInfo <- nrow(liver)

## save final data set
saveRDS(liver, "./procdata/liversub.Rds")
saveRDS(rm.df, "./procdata/rmDF.Rds")
