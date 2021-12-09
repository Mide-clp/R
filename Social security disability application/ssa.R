## Loading library

library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

### Importing the social security disability claim application data set
ssa <- read_csv("http://594442.youcanlearnit.net/ssadisability.csv")
glimpse(ssa)
summary(ssa)
### converting data from wide to long
ssa_long <- gather(ssa, month, Application, -Fiscal_Year)
glimpse(ssa_long)
### putting dates into  format for use
ssa_long$Fiscal_Year <- str_replace(ssa_long$Fiscal_Year, "FY", "20")
ssa_long <- separate(ssa_long, month, c("month", "application_method"), sep = "_")
unique(ssa_long$month)
join_date <- dmy(paste("1", ssa_long$month, ssa_long$Fiscal_Year))
ssa_long <- ssa_long %>% 
  mutate(date = join_date)
print()
### drop fiscal_year and month
ssa_long$Fiscal_Year <- NULL
ssa_long$month <- NULL
### saving application method as factor
ssa_long$application_method <- as.factor(ssa_long$application_method)
unique(ssa_long$application_method)
### Government use fiscal year
#### changing date from fiscal year to actual year
ddd <- which(month(ssa_long$date) >=10)
year(ssa_long$date) <- year(ssa_long$date[ddd]) - 1
ssa_long

### converting data from wide to long
ssa_wide <- spread(ssa_long, application_method, Application)
### percentage of internet collection

ssa_wide$percentage_online <-  (ssa_wide$Internet/ssa_wide$Total) * 100
print(ssa_wide, n=20)
### time series of growth internet 

ggplot(data = ssa_wide)+
  geom_point(mapping = aes(x = date, y = percentage_online, color = "orange"))
