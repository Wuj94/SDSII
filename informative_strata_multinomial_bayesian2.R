#This is the new one 2  ! D:D:D:D:D

library(tidyr)
library(dplyr)
library(readr)
library(tidyimpute)
library(ggplot2)

# Load data
df <- read.csv("./data/CriminalityEastEurope.csv", stringsAsFactors = F)

# drop cols of no use
vars <- colnames(df)
vars
temp.vars <- c(5:7, 30) #event_date,year,time_precision,timestamp
nouse.vars <- c(2:4, 10:12, 13:15, 17, 20:21, 25:28, 31) #iso, evt_id_cnty, event_id_no_cnty, actor1, 
#assoc_actor_1,inter1,actor2, region, assoc_actor2,inter2, admin2, admin3,
#geo_precision, source, source_scale, notes, iso3

unwanted.vars <- c(temp.vars, nouse.vars)
vars <- vars[-unwanted.vars]
df <- df[vars]
# rename columns
colnames(df) <- c("id", "event", "sub_event", "interaction", "country",
                  "city", "location", "latitude", "longitude", "fatalities")


# summaries of what's left
summary(df)

# boxplot interaction 
ggplot(data=df, aes(x=country, y=interaction)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, geom="point", shape=23, size=4)

# scatter interaction vs fatalities
ggplot(data=df, aes(x=interaction, y=fatalities)) +
  geom_point()
# Comment: no relationship seems to lie between the number of
# people involved and the number of fatalities.

# plot |city| stacked barplot; 
# for c : city 
#   for e : event
#     add barplot of stacked subevents in e
ggplot(data=df, aes(x=city, y=event, fill=sub_event)) +
  geom_bar(stat="identity", ) + 
  theme(axis.text.y = element_blank()) + 
  labs(title='Violence in border eastern Europe')

# sub events group by events
df.analysis <- df %>% spread(sub_event, event)
ds <- list()
ds[[1]] <- df.analysis %>% filter(df.analysis$country == 'Belarus') %>%
  select(unique(df$sub_event))
ds[[2]] <- df.analysis %>% filter(df.analysis$country == 'Greece') %>%
  select(unique(df$sub_event))
ds[[3]] <- df.analysis %>% filter(df.analysis$country == 'Russia') %>%
  select(unique(df$sub_event))
ds[[4]] <- df.analysis %>% filter(df.analysis$country == 'Ukraine') %>%
  select(unique(df$sub_event))
ds[[5]] <- df.analysis %>% filter(df.analysis$country == 'Moldova') %>%
  select(unique(df$sub_event))

# Vectorization ?? Well, I tried
#ds <- mapply(FUN = function(df, country, sub_event) df %>% filter(df$country == country) %>% select(sub_event),
#              df=df.analysis, 
#             country=unique(df.analysis$country), 
#             sub_event = unique(df$sub_event))


##f <- function(lds,ids,c) {as.integer(lds[[ids]][,c])}
##mapply(FUN = f, lds = ds, ids=1:length(ds), c=1:length(colnames(ds[[1]])))
## doesn't work >: 
for(i in 1:length(ds)){
  for(j in 1:length(colnames(ds[[1]])))
    ds[[i]][,j] <- replace_na(ds[[i]][,j], 0)
  ds[[i]][ ds[[i]] != "0" ] <- "1"
  ds[[i]] <- data.matrix(ds[[i]])
}

library(rstan)
# Configure Stan library to use multiple cores
options(mc.cores = parallel::detectCores())

# Avoid to recompile each and every time
rstan_options(auto_write = TRUE)

# Grab the data
## The order of the country is "Belarus" "Greece"  "Russia"  "Ukraine" "Moldova"
## Do not permute the columns of the data set! 
## Everything is hardcoded in the .stan file!
n_sub_event <- length(unique(df$sub_event))
N <- c(nrow(ds[[1]]), nrow(ds[[2]]), nrow(ds[[3]]),
       nrow(ds[[4]]), nrow(ds[[5]]))

## The prior are elicited from 
# https://www.numbeo.com/crime/country_result.jsp?country=<Country>
informativeness <- 10  # Parameter to be tuned / chosen

alpha1 <- informativeness * c(2/13, 3/13, 3/13, 1/13, 1/13, 2/13, 1/13) # Border East
alpha2 <- informativeness * c(3/20, 3/20, 3/20, 3/20, 2/20, 5/20, 1/20) # Middle

# Compile and F(/h)it the model!
stn_code <- read_file("stan/informative_strata_multinomial_bayesian2.stan")
stan_data <- list(N=N, n_sub_event=n_sub_event,
                  ds1=ds[[1]], ds2=ds[[2]], ds3=ds[[3]], ds4=ds[[4]], ds5=ds[[5]],
                  alpha1=alpha1, alpha2=alpha2)
iter <- 15000 
fit <- stan(model_code=stn_code, data= stan_data, iter = iter, 
            chains = 4, control = list(adapt_delta = 0.99), 
            warmup = 2*floor(iter/3))

# Get the draws sampled
post <- as.data.frame(fit)

# Traceplot to check convergence of MCMC
traceplot(fit, pars=colnames(post))

# Plot CI's for posterior theta's
plot(fit, pars=c("theta1[1]", "theta2[1]", "theta3[1]", "theta4[1]", "theta5[1]"), yaxt='n', ann=FALSE) +
  ggtitle("Mob Violence 95/80% CI", paste(unique(df$country), collapse=" ")) +
  scale_y_discrete()

plot(fit, pars=c("theta1[2]", "theta2[2]", "theta3[2]", "theta4[2]", "theta5[2]"), yaxt='n', ann=FALSE) +
  ggtitle("Protest w/ Interventions 95/80% CI", paste(unique(df$country), collapse=" ")) +
  scale_y_discrete()

plot(fit, pars=c("theta1[3]", "theta2[3]", "theta3[3]", "theta4[3]", "theta5[3]"), yaxt='n', ann=FALSE) +
  ggtitle("Violent demostration 95/80% CI", paste(unique(df$country), collapse=" ")) +
  scale_y_discrete()

plot(fit, pars=c("theta1[4]", "theta2[4]", "theta3[4]", "theta4[4]", "theta5[4]"), yaxt='n', ann=FALSE) +
  ggtitle("Attack 95/80% CI", paste(unique(df$country), collapse=" ")) +
  scale_y_discrete()

plot(fit, pars=c("theta1[5]", "theta2[5]", "theta3[5]", "theta4[5]", "theta5[5]"), yaxt='n', ann=FALSE) +
  ggtitle("Abduction 95/80% CI", paste(unique(df$country), collapse=" ")) +
  scale_y_discrete()

plot(fit, pars=c("theta1[6]", "theta2[6]", "theta3[6]", "theta4[6]", "theta5[6]"), yaxt='n', ann=FALSE) +
  ggtitle("Excessive force against civilians 95/80% CI", paste(unique(df$country), collapse=" ")) +
  scale_y_discrete()

plot(fit, pars=c("theta1[7]", "theta2[7]", "theta3[7]", "theta4[7]", "theta5[7]"), yaxt='n', ann=FALSE) +
  ggtitle("Granede 95/80% CI", paste(unique(df$country), collapse=" ")) +
  scale_y_discrete()

bayesplot::mcmc_areas(post[c("theta1[1]", "theta2[1]", "theta3[1]", "theta4[1]", "theta5[1]")]) +
  ggtitle("Mob Violence Posteriors", "with medians and 80% intervals") +
  scale_y_discrete(labels=c("Moldova", "Ukraine","Russia", "Greece", "Belarus"))

bayesplot::mcmc_areas(post[c("theta1[2]", "theta2[2]", "theta3[2]", "theta4[2]", "theta5[2]")]) +
  ggtitle("Protest w/ intervention Posteriors", "with medians and 80% intervals") +
  scale_y_discrete(labels=c("Moldova", "Ukraine","Russia", "Greece", "Belarus"))

bayesplot::mcmc_areas(post[c("theta1[3]", "theta2[3]", "theta3[3]", "theta4[3]", "theta5[3]")]) +
  ggtitle("Violent demostration Posteriors", "with medians and 80% intervals") +
  scale_y_discrete(labels=c("Moldova", "Ukraine","Russia", "Greece", "Belarus"))

bayesplot::mcmc_areas(post[c("theta1[4]", "theta2[4]", "theta3[4]", "theta4[4]", "theta5[4]")]) +
  ggtitle("Attack Posteriors", "with medians and 80% intervals") +
  scale_y_discrete(labels=c("Moldova", "Ukraine","Russia", "Greece", "Belarus"))

bayesplot::mcmc_areas(post[c("theta1[5]", "theta2[5]", "theta3[5]", "theta4[5]", "theta5[5]")]) +
  ggtitle("Abduction Posteriors", "with medians and 80% intervals") +
  scale_y_discrete(labels=c("Moldova", "Ukraine","Russia", "Greece", "Belarus"))

bayesplot::mcmc_areas(post[c("theta1[6]", "theta2[6]", "theta3[6]", "theta4[6]", "theta5[6]")]) +
  ggtitle("Excessive force against civilians Posteriors", "with medians and 80% intervals") +
  scale_y_discrete(labels=c("Moldova", "Ukraine","Russia", "Greece", "Belarus"))

bayesplot::mcmc_areas(post[c("theta1[7]", "theta2[7]", "theta3[7]", "theta4[7]", "theta5[7]")]) +
  ggtitle("Grenade Posteriors", "with medians and 80% intervals") +
  scale_y_discrete(labels=c("Moldova", "Ukraine","Russia", "Greece", "Belarus"))

## plot difference and frequency of crimes
## in different populations
draws <- c()
country <- c()
country_names <- unique(df.analysis$country)
outcomes <- c(1,2,3,4,5,6,7) # 1:"Mob violence", ..., 7:"Grenade"
for(i in 1:5){
  thetas <- data.frame(post %>% select(contains(paste0("theta", i))))
  thetas <- as.vector(mapply(mean, x=thetas))
  draws <- c(draws,sample(outcomes, 1000, replace=TRUE, prob=thetas))
  country <- c(country, rep(country_names[i], length(draws)))
}
ggplot(data=data.frame(draws,country), aes(x=country, y=draws, fill=draws)) +
  geom_bar(stat="identity") + 
  theme(axis.text.y = element_blank()) + 
  labs(title='Violence in border eastern Europe')

