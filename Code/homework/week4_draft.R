##load vroom
library(vroom)
##load tidyverse
library("tidyverse")

##read in the data
wide_spp.1 <- vroom("https://raw.githubusercontent.com/chrit88/Bioinformatics_data/master/Workshop%203/to_sort_pop_1.csv")
wide_spp.2 <- vroom("https://raw.githubusercontent.com/chrit88/Bioinformatics_data/master/Workshop%203/to_sort_pop_2.csv") 

## code to reshape data
## first join the data using full join - this will keep all of the columns
long_spp <- full_join(wide_spp.1, wide_spp.2) %>%
  ## pivot the joined data frame, using species, primary_threat, secondary_threat, tertiary_threat as ID columns
  ## and using names-pattern to pull out the population number 
  ## and make a new column (called population) to store them in. 
  ##Drop the NAs. 
  pivot_longer(cols = -c(species, 
                         primary_threat, 
                         secondary_threat, 
                         tertiary_threat), 
               names_to = c("population", "date"),
               names_pattern = "(.*)_(.*)",
               values_drop_na = F, 
               values_to = "abundance")

##make a ggplot object
ggplot(data = long_spp, aes(x = date, y = abundance)) +
  geom_point()

library(lubridate)
long_spp$Date.corrected <- as_date(long_spp$date)
long_spp
ggplot(data = long_spp, aes(x = Date.corrected, y = abundance)) +
  geom_point(aes(col = species)) +
  theme(legend.position = "none") +
  facet_wrap(. ~ species)
