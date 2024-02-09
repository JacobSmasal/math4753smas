ddt <- read.csv(file.choose("DDT-1.csv"))
library(dplyr)
ddt %>% filter(LENGTH > 30 & DDT < 1000)

ddt %>%
