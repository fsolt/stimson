library(tidyverse)

# This should be revised to make use of the ordinal data available in Bartle2016:
# "dev/Bartle2016/UK_Preferences_2012.xlsx"
# https://discover.ukdataservice.ac.uk/catalogue/?sn=852280

ukdata <- read_tsv("dev/McGann2014/ukdata_3.txt") %>%
    group_by(year, variable, topic) %>% 
        summarize(y_r = sum(round(leftp/100 * n)), # When two surveys ask the same question in
                  n = sum(n)) %>% # the same year, add samples together
    ungroup() %>% 
    mutate(cutpoint = 1,
           variable_cp = paste(variable, cutpoint, sep="_gt"),
           ccode = 1,
           tcode = as.integer(year - min(year) + 1),
           qcode = as.numeric(factor(variable, levels = unique(variable))),
           rcode = as.numeric(factor(variable_cp, levels = unique(variable_cp))),
           ktcode = (ccode-1)*max(tcode)+tcode) %>%
    arrange(ccode, tcode, qcode, rcode)
