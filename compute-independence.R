# This file is a reproducible analyses of the independence computation between age and gender.





library(rstanarm)
library(tidyverse)
library(tictoc)


# download data from osf:
d <- read_csv("https://osf.io/cjxua/?action=download")

# codebook:
codebook_url <- "https://osf.io/v68t9/"


vars_expected_independent <-
  c("Dem_gender", "Dem_age")


d3 <-
  d %>%
  select(all_of(vars_expected_independent)) %>%
  mutate(is_female = ifelse(Dem_gender == "Female", 1, 0)) %>%
  select(-Dem_gender) %>%
  drop_na() %>%
  mutate(across(everything(),  #z-scaling
                ~ ((. - mean(.))/sd(.))))


write_csv(d3, file = "d3.csv")


options(mc.cores = parallel::detectCores())

tic()
# note that a simple regression with standardized variables is identical to a correlation
m1 <- stan_glm(Dem_age ~ is_female, data = d3)
toc()

write_rds(m1, file = "m1.rds")

m1
posterior_interval(m1, pars = "is_female", prob = .95)
