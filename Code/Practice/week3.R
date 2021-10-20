##load vroom
library(vroom)

##first we set the working directory (which is the location of the current file you are working on):
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##then read in the data using vroom:
wad_dat <- vroom('../Data/time_series_covid19_deaths_global.csv')

##use vroom to read in some data from github:
covid_dat <- vroom("https://raw.githubusercontent.com/chrit88/Bioinformatics_data/master/Workshop%203/time_series_covid19_deaths_global.csv")

##you can ignore this code for the moment if you want
##but to briefly summarise it is reading in some data included in base R
##and then splitting it into 3 differnt data.frame style objects based on the values in one of the columns ("cyl")
mt <- tibble::rownames_to_column(mtcars, "model")
purrr::iwalk(
  split(mt, mt$cyl),
  ##save this split files in to the default directory
  ~ vroom_write(.x, glue::glue("mtcars_{.y}.csv"), "\t")
)

##find files in the default directory which start with "mtcars" and end in "csv"
##save these file names as an object called "files"
files <- fs::dir_ls(glob = "mtcars*csv")

##these are then the names of the files matching the above arguments:
files

##then load these file names using vroom 
vroom(files)

##load in some RData
load("my_data/pathway/my_data.RData")

##install the tidyverse
install.packages("tidyverse")
##load the tidyverse
library("tidyverse")

##what class is the object
class(covid_dat)
##look at the data
covid_dat
##change the first two names of our data frame
names(covid_dat)[1:2] <- c("Province.State", "Country.Region")

#so this says take our data frame called covid_dat
covid_long <- covid_dat %>%
  ##and then apply this function 
  pivot_longer(cols = -c( Province.State, 
                          Country.Region, 
                          Lat, 
                          Long))

##look at the data
covid_long
##our data frame
covid_long <- covid_dat %>%
  ##and then apply this function 
  pivot_longer(cols = -c(Province.State:Long),
               names_to = "Date",
               values_to = "Deaths")
##change long to wide
covid_wide <- covid_long %>% 
  pivot_wider(names_from = Date,
              values_from = Deaths)
##look at the data
covid_wide
