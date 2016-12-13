library(tidyverse)
library(readxl)
library(stringr)

# Bartle2016: https://discover.ukdataservice.ac.uk/catalogue/?sn=852280

ukmp <- read_excel("dev/Bartle2016/UK_Preferences_2012.xlsx") %>% 
    setNames(tolower(names(.))) %>% 
    select(variable:n) %>% 
    gather(key = cp, 
           value = y, 
           `0`:`10`,
           na.rm = TRUE) %>% 
    mutate(cp = if_else(cp=="neutral", 5, as.numeric(cp)),
           cp = if_else(polar==1, 10-cp, cp)) %>% # make left always high
    group_by(house, date, variable) %>% 
        mutate(cutpoint = as.numeric(factor(cp)) - 1) %>% 
        arrange(-cutpoint) %>% 
        mutate(y_r = cumsum(y),
               variable_cp = paste(variable, cutpoint, sep="_gt"),
               year = as.numeric(str_extract(date, "\\d{4}")),
               t = last(y_r)) %>%
        mutate(y_r = ifelse(t <= 100, y_r*10, y_r),
               n = ifelse(t <= 100, t*10, t)) %>%  # assume 1000 resp surveyed if y as %
    ungroup() %>%
    filter(cutpoint!=0) %>% 
    group_by(year, variable_cp, variable, topic, cutpoint) %>% 
        summarize(y_r = round(sum(y_r)), # When two surveys ask the same question in
                  n = round(sum(n))) %>% # the same year, add samples together
    ungroup() %>% 
    mutate(ccode = 1,
           tcode = as.integer(year - min(year) + 1),
           qcode = as.numeric(factor(variable, levels = unique(variable))),
           rcode = as.numeric(factor(variable_cp, levels = unique(variable_cp))),
           ktcode = (ccode-1)*max(tcode)+tcode) %>%
    arrange(ccode, tcode, qcode, rcode)   
    
    