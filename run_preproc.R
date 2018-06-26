# @Author: andreas.bender@stat.uni-muenchen.de
# @Date:   2016-12-09 21:40:01
# @Last Modified by:   andreas.bender@stat.uni-muenchen.de
# @Last Modified time: 2016-12-09 21:41:30

source("./procdata/import_raw.R", echo=TRUE)
rm(list=ls())
source("./procdata/apply_exclusion.R", echo=TRUE)
rm(list=ls())
source("./procdata/create_survvars.R", echo=TRUE)
rm(list=ls())
source("./procdata/preproc_final.R", echo=TRUE)
