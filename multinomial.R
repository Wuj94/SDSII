#This is the new one! D:D:D:D:D

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
df.wrangled <- df.analysis %>% select(df$sub_event)

df.wrangled[!(is.na(df.wrangled))] <- 1
df.wrangled[is.na(df.wrangled)] <- 0

library(rstan)
# Configure Stan library to use multiple cores
options(mc.cores = parallel::detectCores())

# Avoid to recompile each and every time
rstan_options(auto_write = TRUE)

# Grab the data
N <- nrow(df.wrangled)
n_sub_event <- length(unique(df$sub_event))
ds <- df.wrangled

# Fit the model!
stn_code <- read_file("stan/bayes_analysis2.stan")
fit <- stan(model_code=stn_code, data=list(N=N, n_sub_event=n_sub_event,alpha=alpha, ds=ds), iter=5000, chains = 4)

post <- as.matrix(fit)
plot(fit)
plot_title <- ggtitle("Posterior Distribution", "with medians and 80% intervals")

bayesplot::mcmc_areas(post[, c("theta[1]", "theta[2]")]) + plot_title

