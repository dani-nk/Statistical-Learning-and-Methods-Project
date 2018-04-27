attach(visasclean)
library(tidyverse)

#define training set (up to the year) and test set (equal to the year) for each year 2013 on
visasclean %>% filter(decision_year %in% c(2008,2009,2010,2011,2012)) -> year2013.train
visasclean %>% filter(decision_year %in% c(2013)) -> year2013.test
visasclean %>% filter(decision_year %in% c(2008,2009,2010,2011,2012,2013)) -> year2014.train
visasclean %>% filter(decision_year %in% c(2014)) -> year2014.test
visasclean %>% filter(decision_year %in% c(2008,2009,2010,2011,2012,20133,2014)) -> year2015.train
visasclean %>% filter(decision_year %in% c(2015)) -> year2015.test
visasclean %>% filter(decision_year %in% c(2008,2009,2010,2011,2012,20133,2014,2015)) -> year2016.train
visasclean %>% filter(decision_year %in% c(2016)) -> year2016.test
visasclean %>% filter(decision_year %in% c(2008,2009,2010,2011,2012,20133,2014,2015,2016)) -> year2017.train
visasclean %>% filter(decision_year %in% c(2017)) -> year2017.test
