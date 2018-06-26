# Liver transplantation analysis - Time to graft failure

This is the Code used for analysis of graft failure after liver transplantation. Unfortunately, the data could not be shared, therefore
only the code is provided in this repository.

## Preprocessing

- `import_raw.R`: imports raw data, performs basic data cleaning, variable renaming and non-model based pre-selection, creates new variables.
	Does not perform general plausibility checks.

- Note that transplantation date was only given with monthly precission.

- `apply_exclusion.R`: applies exclusion criteria, stores number of deleted
observations per exclusion criterion.

- `create_survvars.R`: calculates the event/censoring time and status variable
depending on which information is given.

- The above steps are summarized in `run_preproc.R`


## creation of the survival time and event/censoring indicator

The calculation of survival times and event/censoring indicators is based on the following variables:

- `retrans`: When time until retransplantation is given we know the exact date
	of the procedure and know that the current liver did not *completely* fail
	until then, thus we consider this the most reliable information on the liver failure time

- `death`: When `retrans` and `death` are given, we use the `retrans` variable
	as time to failure, otherwise we use `death`.

- `lastseen`: When `lastseen` is given and nothing else, we consider `lastseen`
	as censoring time

- `fail` and `reregis`: for patients where both, `fail`/`reregis` and either
	`death` or `retrans` were given we have seen that these variables are not
	reliable as the time difference between them can be very large, thus we
	exclude observations where only `fail` or `reregis` are given

- When neither of the variables where given we set administrative censoring at
	`2013-12-31` and calculated the censoring time as `2013-12-31 - transplant.date`.


## Modeling

- The confounder model is fit in `confounder_model.R` using `mgcv::gam`
- The final model including donor age is fit in `donorage_model.R`
