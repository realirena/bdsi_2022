## import data 
library(lme4)
library(ggplot2)
library(factoextra)

leData <- read.csv("~/irena/bdsi_2022/leData.csv")
head(leData)

### plot of time and adult mortality 
random_countries <- sample(unique(leData$Country), 10, replace = FALSE)

ggplot(data=leData[leData$Country%in%random_countries,], aes(x=Year, y=Adult.Mortality)) +
  geom_point(color="#378e8e") +  
  geom_smooth(method = "loess", se = TRUE) +
  labs(title="Adult Mortality Over Time") + 
  theme(plot.title = element_text(hjust=0.5, size=20)) +
  theme_bw()


## plot of time and adult mortality, grouped by country 
ggplot(data=leData[leData$Country%in%random_countries,], aes(x=Year, y=Adult.Mortality, group=Country, color=Country)) +
  geom_point(size=3) + 
  geom_line(size=1.25) + 
  labs(title="Adult Mortality Over Time by Country") + 
  theme(plot.title = element_text(hjust=0.5, size=20)) +
  theme_bw()
  


## linear regression 
life_exp_lm <- lm(Adult.Mortality~Alcohol + Year,data=leData)
summary(life_exp_lm)

## linear l=mixed model 
life_exp_lmm <-  lmer(Adult.Mortality~Alcohol+ Year + (1|Country),data=leData)
summary(life_exp_lmm)
confint(life_exp_lmm)


## clustering algorithm 
library(cluster)
random_countries <- sample(unique(leData$Country), 50, replace = FALSE)
leData_final <- na.omit(leData[leData$Year==2000&leData$Country%in%random_countries,c("Country","Life.expectancy","Measles" ,"infant.deaths","Adult.Mortality", "percentage.expenditure", "GDP", "Schooling")])
rownames(leData_final) <- leData_final$Country
PAM3 =pam(leData_final[,2:8], 3)
clusplot(PAM3, labels=3)


## time series example 

timeData <- leData[leData$Country=="Botswana",]

ggplot(data=timeData, aes(x=Year, y=Life.expectancy)) +
  geom_point(color="#378e8e") +  
  labs(title="Life Expectancy Over Time in Botswana") + 
  theme(plot.title = element_text(hjust=0.5, size=20)) +
  theme_bw()

timeData <- dplyr::arrange(timeData, Year)

timeData_lm <- lm(Life.expectancy ~ Year, data=timeData)
summary(timeData_lm)
timeData$fitted_lm <- timeData$Life.expectancy- residuals(timeData_lm)

ggplot(data=timeData, aes(x=Year, y=Life.expectancy)) +
  geom_point(color="#378e8e") +  
  geom_line(color="#378e8e") + 
  geom_point(aes(x=Year, y=fitted_lm), color="red") + 
  geom_line(aes(x=Year, y=fitted_lm), color="red") + 
  labs(title="Life Expectancy Over Time in Botswana", subtitle="plotted with predictions from Linear Regression") + 
  theme(plot.title = element_text(hjust=0.5, size=20)) +
  theme_bw()

## compare with Time Series Model 

timeData_ts <- ts(timeData$Life.expectancy)
timeData_ar <- arima(timeData_ts, order = c(1, 0, 0))
timeData_ar

timeData$fitted <- timeData_ts - residuals(timeData_ar)

ggplot(data=timeData, aes(x=Year, y=Life.expectancy)) +
  geom_point(color="#378e8e") +  
  geom_line(color="#378e8e") + 
  geom_point(aes(x=Year, y=fitted), color="red") + 
  geom_line(aes(x=Year, y=fitted), color="red") + 
  labs(title="Life Expectancy Over Time in Botswana", subtitle="plotted with predictions from AR(1) Model") + 
  theme(plot.title = element_text(hjust=0.5, size=20),
        plot.subtitle = element_text(hjust=0.5, size=18)) 


## linear mixed model 



### spatial model 
library(maptools)
library(spdep)
library(maps)
library(usmap)
library(CARBayes)
#US = map("state",fill=TRUE,plot=FALSE)
#US.poly = map2SpatialPolygons(US,IDs=sapply(strsplit(US$names,":"),function(x) x[1]),
#                             proj4string=CRS("+proj=longlat + datum=wgs84"))
#US.nb = poly2nb(US.poly)
#US.weights = nb2WB(US.nb)
#US.list.w = nb2listw(US.nb)

US.list.w <- readRDS("~/irena/bdsi_2022/us.list.w.rds")
covid_data <- read.csv("~/irena/bdsi_2022/us_covid19_cases_by_state.csv")

# get weights
adj.US = US.weights$adj
rep.US = rep(1:nrow(covid_data),US.weights$num)
W = matrix(0, nrow(covid_data), nrow(covid_data))
for (i in 1:nrow(covid_data)) {
  W[i, adj.US[rep.US == i]] = rep(1, US.weights$num[i])
} 

covid_car = S.CARleroux(formula = case_rate_7_days ~ num_airports_2011,
                        data=covid_data,
                           W=W,
                           family = "poisson",
                           burnin = 50000,
                           n.sample = 200000,
                           thin = 1,
                           rho=1,
                           prior.mean.beta = NULL,
                           prior.var.beta = NULL,
                           prior.nu2 = NULL,
                           prior.tau2 = NULL,
                           verbose = TRUE)

covid_car$summary.results


covid_samples = covid_car$samples$phi
covid_median = as.numeric(apply(covid_samples, 2, median))

