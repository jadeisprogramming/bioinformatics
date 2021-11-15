library(tidyverse)
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

library(lubridate)
long_spp$date <- as_date(long_spp$date)
print(long_spp)

## calculate standarised time per pop and per species. 
long_spp <-  long_spp %>% 
  group_by(species, population) %>%
  mutate(standardised_time = as.numeric(difftime(as.Date(date), 
                                                 min(as.Date(date)), 
                                                 units="weeks")))

## just look four of the columns to visualise the data 
## (you can look at all in Rstudio) but printing all the columns in 
## Rmarkdown means we won't see the standardised_time column
print(long_spp[,c("species", "population", "abundance", "standardised_time")], 10)

## make a new column in long_spp which is the number of threats recorded for each population
##this is the length of the non-NA values in each row of the colunns primary_threat, secondary_threat, and tertiary_threat: 
long_spp <- long_spp %>% 
  rowwise %>%
  mutate(n.threats = length(which(!is.na(c(primary_threat, secondary_threat, tertiary_threat)))))

##load in the scales package for the psudo_log transformation
library(scales)

##build the ggplot, setx§ing the date to a date format
ggplot(long_spp, aes(x=as.Date(date), 
                     y=abundance, 
                     col=n.threats))+
  ##set the theme to minimal
  theme_minimal() +
  ##position the legend at the top on the left
  theme(legend.position = "top", 
        legend.justification="left") +
  ## change the automatic label for the legend to the following
  ## note it is done twice (once for fill and once for col) as 
  ## we change the colour of the lines and the fill of the smooth according to the number of threats
  labs(col="Number of threats", fill="Number of threats") +
  ## add in the lines of the population, using the group and interaction functions
  ## to specify how the data are grouped and get 1 line per population 
  geom_line(aes(group = interaction(population, 
                                    species, 
                                    n.threats, 
                                    population)), 
            alpha=0.2, 
            size=1) + 
  ## add in a smoothed line across all of the population time series for each number of threats
  ## this visualises the mean change across the populations
  geom_smooth(aes(group = n.threats, 
                  fill = n.threats), alpha=0.3, size=1) +
  ##change the colours to reflect the number of threats, using a gradient
  scale_color_gradient(low = "#745745", 
                       high = "#d14124") +
  scale_fill_gradient(low = "#745745", 
                      high = "#d14124") +
  ## transform the y axis using a psudo_log transformation 
  ## (as we have some 0 counts and you can't log 0)
  scale_y_continuous(trans="pseudo_log") +
  ##change the x and y axis labels
  ylab("Population abundance") +
  xlab("standardised_time")

## we will specify the x and y asethics here but specify the groups for the lines later
## this is because doing so means that the data for the geom_smooth() will then be all of the 
## data for each of the facets, but we can specify grouped data for the lines in geom_line
ggplot(long_spp, aes(x = standardised_time,
                     y = abundance)) + 
  ##add the points. we can use the group function to specify
  ## groups within the data (in this case species and population)
  ## that we want to treat as seperate data for the sake of plotting
  ## the `interaction()` function allows us to specify multiple levels to these data
  geom_line(aes(group = interaction(species, population))) + 
  facet_wrap(~n.threats) +
  theme_minimal() +
  ##fit a linear regression to the data to help visualise
  geom_smooth(method = "lm")
## I havent run this, but here is an example of what happens when we group 
## in the initial ggplot() function rather than in the geom_line()
## try running it and seeing what happens
ggplot(long_spp, aes(x = standardised_time,
                     y = abundance,
                     group = interaction(species, population))) + 
  geom_line() + 
  facet_wrap(~n.threats) +
  theme_minimal() +
  geom_smooth(method = "lm")

install.packages("nloptr")

library("glmmTMB")

##fit a random effects model
m_mod1 <- glmmTMB(abundance ~ 
                    standardised_time + 
                    n.threats + 
                    standardised_time:n.threats + 
                    (1|species), 
                  data=long_spp, 
                  family="gaussian")

##look at the model summary:
summary(m_mod1)

9602 / (9602+4447)

##fit a random effects model with nested random effects
m_mod2 <- glmmTMB(abundance ~ standardised_time + 
                    n.threats + 
                    standardised_time:n.threats + 
                    (1|species/population), 
                  data=long_spp, 
                  family="gaussian")

##look at the model summary:
summary(m_mod2)

(8.988e+03+5.554e-93)/(8.988e+03+5.554e-93+3.755e+03)

install.packages("DHARMa")
##load DHARMa
library(DHARMa)

## simulate the residuals from the model
##setting the number of sims to 1000 (more better, according to the DHARMa help file)
m_mod2_sim <- simulateResiduals(m_mod2, n = 1000)

##plot out the residuals
plot(m_mod2_sim)

##a function to return data up to and including the first 0 count:
single_zero <- function(x, group_info){
  if(nrow(x)>0){
    x <- x[ order(x$standardised_time) ,]
    if( min(x$abundance) == 0){
      return( x[1:min(which(x$abundance == 0)),] )
    }else{return(as_tibble(x))}
  }
}

## make a new data frame
species_single0 <- long_spp %>% 
  ##we want to cut the long_spp data up by the values in species and population
  group_by(species, population) %>% 
  ##the group_map function allows us to apply a function to these groups
  ##and we want to keep the grouping variables, hence keep = T
  group_map(single_zero, .keep = T) %>% 
  ##and then bind it all back together again to a tibble
  ##otherwise it is returned as a list of the group data
  bind_rows()

install.packages("fitdistrplus")
##load the fitdistrplus package
library(fitdistrplus)    

##fit a poisson distribution to our data:
fit_pois <- fitdist(species_single0$abundance, 
                    distr = "pois")
##plot the data
plot(fit_pois)
##look at the summary statistics
gofstat(fit_pois)

##fit a nbinom to the data instead
fit_nbinom <- fitdist(species_single0$abundance, 
                      dist = 'nbinom')
##again we get warnings from those missing values and can ignore

##plot it out:
plot(fit_nbinom)

##the goodness of fit
gofstat(fit_nbinom)

## fit a poisson model
ps_mod <- glmmTMB(abundance ~ scale(standardised_time) * scale(n.threats) + (1 | species/population), 
                  data = species_single0,
                  family="poisson")
##summarise the model output
summary(ps_mod)

##function to calculate a psudo R2 value for our GLMM
r2.corr.mer <- function(m) {
  lmfit <-  lm(model.response(model.frame(m)) ~ fitted(m))
  summary(lmfit)$r.squared
}

## apply it to our poisson model
r2.corr.mer(ps_mod)

##function to calculate overdispersion 
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

##apply to our data
overdisp_fun(ps_mod)

###try with nbinom: 
nb1_mod <- update(ps_mod, family="nbinom1")

##and the second parameterisation of nb
nb2_mod <- update(ps_mod, family="nbinom2")

##zero inflated version of these with zeros being attributed to number of threats:
Zi_nb1_mod <- update(nb1_mod, ziformula = ~n.threats)
Zi_nb2_mod <- update(nb2_mod, ziformula = ~n.threats)

##and we can also look at the zero inflated version of the poisson model:
Zi_ps_mod <- update(ps_mod, ziformula = ~n.threats)

##compare the models
anova(ps_mod, 
      Zi_ps_mod,
      nb1_mod, 
      nb2_mod,
      Zi_nb1_mod,
      Zi_nb2_mod)

##calculate the psudo r2 value of Zi_nb1_mod:
r2.corr.mer(Zi_nb1_mod)

##fit a zero inflated negative binomial model with autocorrelation structure defined by the time variable
Zi_nb1_ar1_mod <- glmmTMB(abundance ~ scale(standardised_time) * scale(n.threats) + (1|species/population) + ar1(factor(scale(standardised_time)) - 1|species/population), 
                          data = species_single0,
                          ziformula=~n.threats,
                          family="nbinom1")
##fit a zero inflated negative binomial model with autocorrelation structure defined by the time variable
Zi_nb1_ar1_mod <- glmmTMB(abundance ~ scale(standardised_time) * scale(n.threats) + (1|species/population) + ar1(factor(scale(standardised_time))-1|species/population), 
                          data = species_single0,
                          ziformula=~n.threats,
                          family="nbinom1",
                          ##the control parameter specifying the optimizer to use:
                          control=glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS")))
##show the fixed effects
fixef(Zi_nb1_ar1_mod)

##look at the model
Zi_nb1_mod

##zero inflated negative binomial model with autocorrelation and population as random effects
Zi_nb1_ar1_mod <- glmmTMB(abundance ~ scale(standardised_time) * scale(n.threats) + (1|population) + ar1(factor(scale(standardised_time))-1|population), 
                          data = species_single0,
                          ziformula=~n.threats,
                          family="nbinom1",
                          ##the control parameter specifying the optimizer to use:
                          control=glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS")))

## simualte the residuals from the model
##setting the number of sims to 1000 (more better, according to the DHARMa help file)
Zi_nb1_mod_sim <- simulateResiduals(Zi_nb1_mod, n = 1000)

##plot out the residuals
plot(Zi_nb1_mod_sim)

## test to see where there are outliers, in our case not significant so we dont need to worry
testOutliers(Zi_nb1_mod_sim, 
             plot = TRUE)
## tests if the simulated dispersion is equal to the observed dispersion,
## again not significant so no need to worry
testDispersion(Zi_nb1_mod_sim, 
               plot = TRUE)
## compares the distribution of expected zeros in the data against the observed zeros
## this is right on the borderline of being significant, suggesting there might be a better structure for
## our zero inflation parameter (remember we used ~n.threats). That might be worth looking into further
testZeroInflation(Zi_nb1_mod_sim, 
                  plot = TRUE)
## see if there is temporal autocorrelation in the residuals
## not significant, so it turns out we didnt need to try and fit the autocorrelation model earlier on!
testTemporalAutocorrelation(Zi_nb1_mod_sim,
                            time = species_single0$standarised_time,
                            plot = TRUE) 
## add in the predicted values from the model:
species_single0$predicted <- predict(Zi_nb1_mod, 
                                     data = species_single0, 
                                     type = "response")

##plot the predicted against the observed
ggplot(species_single0, aes(x = abundance, 
                            y = predicted)) + 
  geom_point(col="grey") + 
  geom_abline(slope = 1) +
  theme_minimal() +
  xlab("Observed") +
  ylab("Predicted")

##summarise the model 
summary(Zi_nb1_mod)

## make a blank data set which includes the variables in the model
## we will then use this to generate predicted values from the model for 
## various combinations of number of threats, standardised time, species,
## and population
## we can use the unique() function across the columns in our data.frame
## to retrieve every unique combination of:
##n.threats, standardised_time, species, and population
new_data <-  unique(species_single0[,c("n.threats",
                                       "standardised_time", 
                                       "species", 
                                       "population")])
##scale the relevant columns (remember our model is expecting scaled data)
new_data$n.threats<-scale(new_data$n.threats)
new_data$standardised_time<-scale(new_data$standardised_time)

##set the random effects of the model to zero
X_cond <- model.matrix(lme4::nobars(formula(Zi_nb1_mod)[-2]), new_data)
beta_cond <- fixef(Zi_nb1_mod)$cond
pred_cond <- X_cond %*% beta_cond
ziformula <- Zi_nb1_mod$modelInfo$allForm$ziformula
X_zi <- model.matrix(lme4::nobars(ziformula), new_data)
beta_zi <- fixef(Zi_nb1_mod)$zi
pred_zi <- X_zi %*% beta_zi

##transform point estimates of the unconditional counts to the response scale and multiply
##(because they are logged and on logit link)
pred_ucount = exp(pred_cond)*(1-plogis(pred_zi))

##load the MASS library
library(MASS)

##set the random number generator seed
set.seed(101)

##use posterior predictive simulations to generated upper and lower confidence intervals
## and median predicted counts
##ignoring variabtion in the random effects

##conditional
pred_condpar_psim = mvrnorm(1000,mu=beta_cond,Sigma=vcov(Zi_nb1_mod)$cond)
pred_cond_psim = X_cond %*% t(pred_condpar_psim)

##zero inflation parameter
pred_zipar_psim = mvrnorm(1000,mu=beta_zi,Sigma=vcov(Zi_nb1_mod)$zi)
pred_zi_psim = X_zi %*% t(pred_zipar_psim)

##transform them
pred_ucount_psim = exp(pred_cond_psim)*(1-plogis(pred_zi_psim))

##calculate 95% CIs
ci_ucount = t(apply(pred_ucount_psim,1,quantile,c(0.025,0.975)))
ci_ucount = data.frame(ci_ucount)

##rename
names(ci_ucount) = c("ucount_low","ucount_high")

##put into a data frame
pred_ucount = data.frame(new_data, 
                         pred_ucount, 
                         ci_ucount)

##we need to reverse the scaling of our predictor variables for our plots to make sense
##the scale() function stores attributes of the scaling in the vectors of scaled data 
## try running new_data$n.threats and looking at the bottom values
##write a function to do this:
unscale <- function(x){
  x * attr(x, 'scaled:scale') + attr(x, 'scaled:center')
}

##unscale the variables
pred_ucount$n.threats_unscaled <- unscale(pred_ucount$n.threats)
pred_ucount$standardised_time_unscaled <- unscale(pred_ucount$standardised_time)

install.packages("viridis")
##load the viridis package (colourblind friendly palletes)
library(viridis)

##plot out the predicted median values for abundance
## in response to time (x-axis)
##and grouped by the number of threats
ggplot(pred_ucount, aes(x = standardised_time_unscaled, 
                        y = pred_ucount, 
                        group = n.threats_unscaled, 
                        col = n.threats_unscaled))+ 
  ##median lines for each number of threats
  geom_line() +
  ##add in a geom_ribbon to show the 95% CI
  geom_ribbon(aes(ymin = ucount_low,
                  ymax = ucount_high), 
              alpha = 0.1, 
              col = "grey", 
              linetype=0) +
  ##minimal theme
  theme_minimal() +
  ##set x and y axes labels
  ylab("Predicted\nabundance") + xlab("Time\n(weeks)") +
  ##viridis colour pallette for continuous data
  scale_colour_viridis_c() +
  ##move legend to the top
  theme(legend.position = "top") +
  ##rename the legend
  labs(colour="Number of threats")
