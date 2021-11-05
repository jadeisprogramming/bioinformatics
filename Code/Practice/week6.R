##load the iris data set
data("iris")

library(tidyverse)

## Plot the sepal widths so we can visualise if there are differences between the different species
ggplot(iris, aes(x=Species, y=Sepal.Width, col=Species)) + 
  geom_jitter() +
  theme_bw()

ggplot(iris, aes(x=Sepal.Width, fill = Species)) +
  ## bin width determines how course the histogram is
  ## the alpha determines the transparency of the bars
  ## position allows you to determine what kind of histogram you plot (e.g. stacked vs overlapping). try changing to position="stack"
  geom_histogram(binwidth = .1, alpha = .5, position="identity")

##fit a glm()
mod_iris <- glm(Sepal.Width ~ Species,
                ##specify the data
                data = iris,
                ##specify the error structure
                family = "gaussian")
##display the class of the model object
class(mod_iris)
##display the class of the model object
plot(mod_iris)
##summarise the model outputs
summary(mod_iris)

## load the multcomp pack
library(multcomp)
## run the multiple comparisons, and look at the summary output:
summary(glht(mod_iris, mcp(Species="Tukey")))

library(vroom)
##read in the data
wide_spp.1 <- vroom("https://raw.githubusercontent.com/chrit88/Bioinformatics_data/master/Workshop%203/to_sort_pop_1.csv")
wide_spp.2 <- vroom("https://raw.githubusercontent.com/chrit88/Bioinformatics_data/master/Workshop%203/to_sort_pop_2.csv") 

## code to reshape data
## first join the data using full join - this will keep all of the columns
long_spp <- full_join(wide_spp.1, wide_spp.2) %>%
  pivot_longer(cols = -c(species, 
                         primary_threat, 
                         secondary_threat, 
                         tertiary_threat), 
               names_to = c("population", "date"),
               names_pattern = "(.*)_(.*)",
               values_drop_na = T, 
               values_to = "abundance")
print(long_spp)

library(lubridate)
long_spp$date <- as_date(long_spp$date)
print(long_spp)

single_spp <- filter(long_spp, long_spp$species == "Trichocolea tomentella")

##make the plot
p1 <- ggplot(single_spp, aes(x=date, y=abundance)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  ylab("Abundance") +
  xlab("Year")
##add the loess smoothing:
p1 + geom_smooth(method="loess")

## calculate a new column (`standardised_time`) which is the difference between the
## starting date of the time series and each other date in weeks (see ?difftime)
## we will set this to a numeric vector
single_spp <- single_spp %>%
  mutate(standardised_time = as.numeric(difftime(as.Date(date),
                                                 min(as.Date(date)),
                                                 units = "weeks")))

print(single_spp[,c("abundance", "date", "standardised_time")], 30)

##fit a glm()
mod1 <- glm(abundance ~ standardised_time,
            data = single_spp,
            family = "gaussian")

##return the predicted (response) values from the model
##and add them to the single species tibble:
single_spp$pred_gaussian <- predict(mod1,
                                    type="response")
##return the model residuals and add to the single species tibble:
single_spp$resid_gaussian <- resid(mod1)

## plot the abundances through time
p2 <- ggplot(single_spp, aes(x = standardised_time,
                             y = abundance)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  ylab("Abundance") +
  xlab("standardised_time")

##add in a line of the predicted values from the model
p2 <- p2 + geom_line(aes(x = standardised_time,
                         y = pred_gaussian),
                     col = "dodgerblue",
                     size = 1)

## we can also add in vertical blue lines which show the redsidual error of the model
## (how far the observed points are from the predicted values).
## in geom_segement we specify where we want the start and end of the segments (lines)
## to be. Without any prompting ggplot assumes that we want the start of the lines to
## be taken from the x and y values we are plotting using the ggplot() function
## i.e. standardised_time and abundance, so we just need to specify the end points of
## the lines:
p2 <- p2 +
  geom_segment(aes(xend = standardised_time,
                   yend = pred_gaussian),
               col="lightblue")

## add a title
p2 <- p2 + ggtitle("Fitted model (gaussian with identity link)")

##print the plot
p2

##plot a histogram of the residuals from the model using geom_histogram()
p3 <- ggplot(single_spp, aes(x = resid_gaussian)) +
  geom_histogram(fill="goldenrod") +
  theme_minimal() +
  ggtitle("Histogram of residuals (gaussian with identity link)")
## print the plot
p3

##make the plot
p4 <- ggplot(single_spp, aes(x=pred_gaussian, y=resid_gaussian)) +
  geom_point() +
  theme_minimal() +
  ggtitle("Predicted vs residual (gaussian with identity link)")+
  xlab("Predicted Values") +
  ylab("Residuals")
##add the loess smoothing:
p4 + geom_smooth(fill="lightblue", col="dodgerblue")

##plot the qq plot for the residuals from the model assuming a normal distribution,
## and add the straight line the points should fall along:
qqnorm(single_spp$resid_gaussian); qqline(single_spp$resid_gaussian)

## fit a glm with a poisson distribution
mod2 <- glm(abundance ~ standardised_time,
            data = single_spp,
            family = "poisson")
## fit a glm with a gaussian distribution with a log link
mod3 <- glm(abundance ~ standardised_time,
            data = single_spp,
            family = gaussian(link = "log"))
## we could also try a guassian model with an inverse link
mod4 <- glm(abundance ~ standardised_time,
            data = single_spp,
            family = gaussian(link = "inverse"))

install.packages("gamlr")
##install the gamlr package, and then:
library(gamlr)

##compare the models
AIC_mods <- data.frame(model = c("mod1", "mod2", "mod3", "mod4"),
                       AICc = c(AICc(mod1), AICc(mod2), AICc(mod3), AICc(mod4)))

## rank them by AIC using the order() function
AIC_mods[order(AIC_mods$AICc),]

## plot the abundances through time
p5 <- ggplot(single_spp, aes(x = standardised_time,
                             y = abundance)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  ylab("Abundance") +
  xlab("standardised_time")
##return the predicted (response) values from the mod3
##and add them to the single species tibble:
single_spp$pred_gaussian_log <- predict(mod3,
                                    type="response")
##return the model residuals and add to the single species tibble:
single_spp$resid_gaussian_log <- resid(mod3)
##add in a line of the predicted values from the mod3
p5 <- p5 + geom_line(aes(x = standardised_time,
                         y = pred_gaussian_log),
                     col = "dodgerblue",
                     size = 1)
p5 <- p5 +
  geom_segment(aes(xend = standardised_time,
                   yend = pred_gaussian_log),
               col="lightblue")
## add a title
p5 <- p5 + ggtitle("Fitted model (gaussian with log link)")
p5

##plot the diagnostic graphics for model 3
plot(mod3)

##summarise the model outputs
summary(mod3)

## first off let's plot the data again
p6 <- ggplot(single_spp, aes(x=standardised_time, y=abundance)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  ylab("Abundance") +
  xlab("standardised_time")

## use the geom_smooth() function to add the regression to the plot.
## unlike earlier here we are specifying the model type (glm), the formula,
## and the error structure and link
p6 <- p6 + geom_smooth(data=single_spp,
                       method="glm",
                       method.args = list(family = gaussian(link="log")),
                       formula = y ~ x,
                       col = "dodgerblue",
                       fill = "lightblue")

##print the plot
p6
