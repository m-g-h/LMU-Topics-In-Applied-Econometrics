library(tidyverse)

# Complete Dataset --------------------------------------------------------



# RDD NORMAL CASE ---------------------------------------------------------

data = tibble(D = -35:35,
              Treated = as.numeric(D >= 0),
              W = seq(from = 5, to = 15, length = length(D)) - 2*Treated + 0*rnorm(length(D), s = 0.2)
)

# Simple RDD
RDD = lm(W ~ Treated + D, data = data)

# left lm
lm_left = lm(W ~ D, data = data,
              subset = Treated == 0)
# right lm
lm_right = lm(W ~ D, data = data,
              subset = Treated == 1)
# Split RDD
RDD_split = lm(W ~ Treated + D*Treated, data = data)

data_RDD = bind_rows(tibble(D = data$D, RDD = RDD$fitted.values, model = "RDD"),
                     tibble(D = data$D, RDD = RDD_split$fitted.values, model = "Split RDD"))

data %>% 
  ggplot(mapping = aes(x = D, y = W)) +
  geom_point() +
  geom_line(data = data_RDD,
            mapping = aes(y = RDD, color = model))


# RDD DIFFERENT SLOPES CASE -----------------------------------------------

data_sloped = tibble(D = -35:35,
                     Treated = as.numeric(D >= 0),
                     W = seq(from = 5, to = 15, length = length(D)) - 2*Treated + 0*rnorm(length(D), s = 0.2)
) %>% 
  mutate(W = W - 0.15*D*Treated)

data_sloped %>% 
  ggplot(mapping = aes(x = D, y = W)) +
  geom_point()

# Simple RDD
RDD_sloped = lm(W ~ Treated + D, data = data_sloped)
# Split RDD
RDD_sloped_split = lm(W ~ Treated + D*Treated, data = data_sloped)

data_RDD_split = bind_rows(tibble(D = data$D, RDD = RDD_sloped$fitted.values, model = "RDD"),
                           tibble(D = data$D, RDD = RDD_sloped_split$fitted.values, model = "Split RDD"))

data_sloped %>% 
  ggplot(mapping = aes(x = D, y = W)) +
  geom_point() +
  geom_line(data = data_RDD_split,
            mapping = aes(y = RDD, color = model))


# DIFF IN DIFF ------------------------------------------------------------
data = tibble(D = -35:35,
              Treated = as.numeric(D >= 0),
              W = seq(from = 5, to = 5, length = length(D)) - 2*Treated + 0*rnorm(length(D), s = 0.2)
)


data_did = bind_rows(data %>% 
                       mutate(Year = 0),
                     data %>% 
                       mutate(W = W + 0 - 1*Treated,
                              Year = 1))

data_did %>% 
  ggplot(mapping = aes(x = D, y = W)) +
  geom_point() +
  facet_grid(cols = vars(Year))

# Year fixed effect
did_year = lm(W ~ Year, data = data_did)

# Year fixed effect + Marathon
did_year_T = lm(W ~ Treated + Year, data = data_did)

# full DiD
did_full = lm(W ~ Treated*Year + Treated + Year, data = data_did)

data_did_regressions = 
  bind_rows(tibble(D = data_did$D, Year = data_did$Year, DiD = did_year$fitted.values, model = "Year"),
            tibble(D = data_did$D, Year = data_did$Year, DiD = did_year_T$fitted.values, model = "Year + Marathon"),
            tibble(D = data_did$D, Year = data_did$Year, DiD = did_full$fitted.values, model = "Full"))

data_did %>% 
  ggplot(mapping = aes(x = D, y = W, shape = as.character(Year))) +
  geom_point() +
  geom_point(data = data_did_regressions %>% filter(model == "Year"),
            mapping = aes(y = DiD, color = model))

data_did %>% 
  ggplot(mapping = aes(x = D, y = W, shape = as.character(Year))) +
  geom_point() +
  geom_point(data = data_did_regressions %>% filter(model != "Full"),
             mapping = aes(y = DiD, color = model))

data_did %>% 
  ggplot(mapping = aes(x = D, y = W, shape = as.character(Year))) +
  geom_point() +
  geom_point(data = data_did_regressions,
             size = 0.7,
             mapping = aes(y = DiD, color = model))

