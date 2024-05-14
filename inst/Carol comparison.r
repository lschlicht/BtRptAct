Gilsenan_et_al_2019 <- readRDS("/ds/grpkempenaers/Lotte/R Studio projects/Data for BtRptAct/Carol's data/Gilsenan_et_al_2019.rds")
g = copy(Gilsenan_et_al_2019)

g[, z_laying_date := scale(laying_date, center = TRUE, scale = TRUE), by = .(Season)]
g[, z_Arrival_date := scale(Arrival_date, center = TRUE, scale = TRUE), by = .(Season)]

#are the joint data points identical?
tmp = merge(g, wd[[1]], by.x = c("ID", "Season"), by.y = c("ID", "season"), all = FALSE)
tmp[, Arrival_date := yday(as.IDate(Arrival_date))]
tmp[, Arrival_date := ifelse(Arrival_date < yday("1970-08-01"), Arrival_date, Arrival_date-365), by = Season]
plot(tmp$Arrival_date ~ tmp$arrivalArea) #three deviating data points are from smart feeders that were only used by Gilsenan et al. 2019
plot(yday(tmp$laying_date)  ~ yday(tmp$firstEgg))
#otherwise the dataset are identical where they overlap.

#subset datasets to include only the data relevant for this analysis (in order to judge sample sizes)
g = subset(g, !is.na(laying_date) & !is.na(Arrival_date) & sex == "female")
l = subset(wd[[1]], !is.na(firstEgg) & !is.na(z_useArrival) & sex == 2 & age == 2)
nrow(g)
nrow(l)

#investigate relationship
m = lmer(z_laying_date ~ z_Arrival_date + (1|ID), data = g)
summary(glht(m))

m = lmer(z_firstEgg ~ z_useArrival + (1|ID), data = subset(l, season %in% c(2015:2017)))
summary(glht(m))
#estimate relatively similar, but sample size in second model smaller, because Smart Feeders were not included.

m = lmer(z_firstEgg ~ z_useArrival + (1|ID), data = subset(l, season %in% c(2010:2019)))
summary(glht(m))
#estimate much smaller, despite overall larger sample size => estimate disappears due to the additioinal years of the study.

#Note that the actual estimates differ slightly from the numbers published in the corresponding publication, because we also scaled the data within sex, not only within season.
