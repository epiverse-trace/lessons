
# FA01 --------------------------------------------------------------------

#' what is the error in this code?

library(tidyverse)
library(covidregionaldata)
library(i2extras)
library(incidence2)

raw_dat <- get_national_data(countries = "Mexico") %>%
  filter(date > lubridate::ymd(20210501)) %>%
  filter(date < lubridate::ymd(20210801))

# Convert the data to an incidence object
dat <- incidence(
  x = raw_dat, 
  date_index = "date",
  interval = "week"
)

# Model the incidence
out <- fit_curve(dat, model = "poisson", alpha = 0.05)

# Print the result
growth_rate(out)

# Plot the result
plot(out)


## H01 ---------------------------------------------------------------------

raw_dat %>%
  ggplot() +
  aes(x = date, y = cases_new) +
  geom_line(alpha = 0.4)

dat

## S01 ---------------------------------------------------------------------

# FIX: Convert the data to an incidence object
dat <- incidence(
  x = raw_dat, 
  counts = "cases_new", # MISSING
  date_index = "date",
  interval = "week"
)


## RSE01 -------------------------------------------------------------------

#' add a warning when no count argument is added

# FA02 --------------------------------------------------------------------

library(tidyverse)
library(covidregionaldata)
library(incidence2)
library(EpiNow2)

generation_time <- get_generation_time(
  disease = "SARS-CoV-2", source = "ganyani", max = 10, fixed = TRUE
)

raw_dat <- covidregionaldataUK %>% as_tibble() %>% 
  filter(magrittr::is_in(region,c("England", "Scotland", 
                                  "Northern Ireland", "Wales"))) %>% 
  filter(date > lubridate::ymd(20200701)) %>%
  filter(date < lubridate::ymd(20201101))

# Convert the data to an incidence object
dat <- incidence(
  x = raw_dat, 
  counts = "cases_new",
  date_index = "date",
  interval = "day"
)

estimates <- epinow(
  reported_cases = dat,
  generation_time = generation_time_opts(generation_time),
  rt = rt_opts(prior = list(mean = 2, sd = 0.2)),
  horizon = 0,
  CrIs = c(0.5, 0.95),
  stan = stan_opts(samples = 1e3, cores = 4, control = list(adapt_delta = 0.99),),
  verbose = interactive()
)


## H02 ---------------------------------------------------------------------

?epinow

## S02 ---------------------------------------------------------------------

# # Adapt the column names for epinow2
reported_cases <- dat %>%
  dplyr::select(            # MISSING
    date    = date_index,
    confirm = count
  )


## RSE02 -------------------------------------------------------------------

#' add a control point to remind users to rename column names

# FA03 --------------------------------------------------------------------

#' what is the difference between using only the
#' generation time delay
#' and
#' joining the incubation and reporting time delays
#' for the estimation of
#' the time-varying reproductive number?

library(tidyverse)
library(covidregionaldata)
library(incidence2)
library(EpiNow2)

generation_time <- get_generation_time(
  disease = "SARS-CoV-2", source = "ganyani", max = 10, fixed = TRUE
)

generation_time <- get_generation_time(
  disease = "SARS-CoV-2", source = "ganyani", max = 10, fixed = TRUE
)
incubation_period <- get_incubation_period(
  disease = "SARS-CoV-2", source = "lauer", max = 10, fixed = TRUE
)

epinow(
  reported_cases = reported_cases,
  generation_time = generation_time_opts(generation_time),
  delays = delay_opts(incubation_period + reporting_delay),
  rt = rt_opts(prior = list(mean = 2, sd = 0.2)),
  horizon = 0,
  CrIs = c(0.5, 0.95),
  stan = stan_opts(samples = 1e3, cores = 4, control = list(adapt_delta = 0.99),),
  verbose = interactive()
)


## H03 ---------------------------------------------------------------------

#' review glosarry and visual aids for definitions 

## S03 ---------------------------------------------------------------------

#' generation time is compusory?
#' delays are compulsory?
#' what is the added value of consider them?
#' difference when no certainty about date of infection
#' of include the uncertainty of date of infection from report of case date only
