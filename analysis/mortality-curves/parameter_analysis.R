rm(list=ls())

library(dplyr)
library(ggplot2)

df<-read.csv("./analysis/mortality-curves/data_model_parameters.csv")
df$max_day<-NA
countries<-unique(df$country)

for (i in 1:length(countries)){
  sub<-subset(df, country==countries[i])
  time<-max(sub$dss,na.rm=T)
  df[rownames(sub),"max_day"]<-time
}
# Y0 is actually P0, the initial population of infected at at time 0
# dss - day since start ( the current duration of the epidemic)

# Logistic population growth model (LPGM)
# P(t) = (K * P0 * exp(r*TIME) ) / ( K + P0(exp(r * TIME) - 1) )
# where
# P(t) = cummulative count of inected cases (population)
# K  - carrying capacity, maximum cumulative cases, maximum number that would get sick(i.e. detected), cases that would require medical intevention
# r - maximum/potentia growth rate (allowed within this model)
# P0 - initial number of people that triggered case detection (conceptually hard, for estimation)

# select only those countries whose growth fits well to the LPGM
df_g<-df[which(df$rsq>0.9),]
model_r<-lm(log(r)~log(n_population_2018)+log(max_day),data=df_g)
model_K<-lm(log(K)~log(n_population_2018)+log(max_day),data=df_g)
model_Y0<-lm(log(Y0)~log(n_population_2018)+log(max_day),data=df_g)
# r - maximum daily growth rate (potential growth rate)
# summary(model_K)
summary(model_Y0)
dg <-df_g %>% tibble::as_tibble()
dg %>% glimpse()

g1 <- dg %>%
  dplyr::filter(!country %in% c("China", "India")) %>%
  # ggplot(aes(x = log(max_day), y = log(K)))+
  # ggplot(aes(x = log(n_population_2018), y = log(K)))+
  # ggplot(aes(x = n_population_2018, y = K, label = country, size = max_day))+
  # ggplot(aes(x = n_population_2018, y = max_day))+
  geom_point(shape = 21)+
  geom_smooth(method = "loess")+
  # geom_smooth(method = "lm")+
  ggpmisc::stat_poly_eq(
    formula = y ~ + x
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE, vjust = 7
  ) +
  theme_bw()
g1 <- plotly::ggplotly(g1)
g1



sub_US<-subset(df_g,country==countries[191])
pars_IT<-subset(df_g,country=="Italy")
sub_Ca<-subset(df_g,country=="Canada")

pars_US<-c(unique(sub_US$K),unique(sub_US$r),unique(sub_US$Y0),unique(sub_US$max_day))
names(pars_US)<-c("K","r","Y0","max_day")
pars_IT<-c(unique(pars_IT$K),unique(pars_IT$r),unique(pars_IT$Y0),unique(pars_IT$max_day))
names(pars_IT)<-c("K","r","Y0","max_day")
#adjustment for max_day
pars_IT["K"]<-pars_IT["K"]*(pars_US["max_day"]^3.06854)*(unique(sub_US$n_population_2018)^0.336)/((pars_IT["max_day"]^3.06854)*(unique(pars_IT$n_population_2018)^0.336))
pars_IT["r"]<-pars_IT["r"]*(pars_US["max_day"]^(-0.319))/(pars_IT["max_day"]^(-0.319))
pars_Ca<-c(unique(sub_Ca$K),unique(sub_Ca$r),unique(sub_Ca$Y0),unique(sub_Ca$max_day))
names(pars_Ca)<-c("K","r","Y0","max_day")
pars_Ca["K"]<-pars_Ca["K"]*(pars_US["max_day"]^3.06854)*(unique(sub_US$n_population_2018)^0.336)/((pars_Ca["max_day"]^3.06854)*(unique(sub_Ca$n_population_2018)^0.336))
pars_Ca["r"]<-pars_Ca["r"]*(pars_US["max_day"]^(-0.319))/(pars_Ca["max_day"]^(-0.319))

USA<-pars_US["K"]*pars_US["Y0"]*exp(pars_US["r"]*seq(1:90))/(pars_US["K"]+pars_US["Y0"]*(exp(pars_US["r"]*seq(1:90))-1))/unique(sub_US$n_population_2018)
UA<-pars_IT["K"]*pars_IT["Y0"]*exp(pars_IT["r"]*seq(1:90))/(pars_IT["K"]+pars_IT["Y0"]*(exp(pars_IT["r"]*seq(1:90))-1))/unique(sub_US$n_population_2018)
CA<-pars_Ca["K"]*pars_Ca["Y0"]*exp(pars_Ca["r"]*seq(1:90))/(pars_Ca["K"]+pars_Ca["Y0"]*(exp(pars_Ca["r"]*seq(1:90))-1))/unique(sub_US$n_population_2018)

# plot shows difference between covid dynamics in US, UA, and CA after adjustment for effect of length of observation and population size
plot(USA*100,type="l",xlab="days since the first case",ylab="cumulative cases, % of population",lwd=2)
lines(UA*100,col="blue",lwd=2)
lines(CA*100,col="red",lwd=2)

#r maximum patient increase rate (fractional), affected by habits, contacts, quarantine measures, intrinsic infectiousness of pathogen
#K number of cases after which no new cases will be detected;
#Y0 - initial number of cases

# sub<-subset(df,country==countries[191])
# model<-nls(n_cases_sum~K*Y0*exp(r*dss)/(K+Y0*(exp(r*dss)-1)),data=sub,start=list(K=100,Y0=0.4,r=0.2),algorithm="port",
#            lower=list(K=10,Y0=0.0001,r=0.0001),upper=list(K=1000000,Y0=2,r=0.5))
# sdat<-summary(model)
# df[rownames(sub),"rsq"]<- cor(predict(model),(sub$n_cases_sum[which(sub$n_cases_sum!=0)]))^2
# df[rownames(sub),"K"]<- sdat$coefficients[1]
# df[rownames(sub),"Y0"]<- sdat$coefficients[2]
# df[rownames(sub),"r"]<- sdat$coefficients[3]
