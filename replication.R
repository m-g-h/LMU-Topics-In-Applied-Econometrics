library(haven)
library(tidverse)
library(estimatr)

data = read_dta("ueaa053_replication_package/Replication_Package/well-being-data.dta")


data %>% 
  filter(between(daydiff, -35, 35)) %>% 
  filter(daydiff != 0) %>% 
  filter(y2013 == 1) %>% 
  lm_robust(avghappy123 ~ Bost + dayminus + dayplus, weights = wufnactwt1,
            fixed_effects = daydate,
            data = .) %>% 
  summary()

