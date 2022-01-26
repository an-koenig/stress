library(rstanarm)
library(tidyverse)
library(tictoc)


# download data from osf:
d <- read_csv("https://osf.io/cjxua/?action=download")

# codebook:
codebook_url <- "https://osf.io/v68t9/"


# big five variables:
big_five <- c("ope", "con", "ext", "agr", "neu")

d2 <-
  d %>%
  select(all_of(big_five)) %>%
  drop_na()


# this variables are expected to be independent:
vars_expected_independent<- c("Dem_age", "Dem_gender")

d3 <-
  d %>%
  select(all_of(vars_expected_independent)) %>%
  mutate(is_female = ifelse(Dem_gender == "Female", 1, 0)) %>%
  select(-Dem_gender) %>%
  drop_na() %>%
  mutate(across(everything(),
                scale))


options(mc.cores = parallel::detectCores())
tic()
m1 <- stan_glm(Dem_age ~ is_female, data = d3)
toc()

write_rds(m1, file = "m1.rds")

m1
posterior_interval(m1, pars = "is_female")
