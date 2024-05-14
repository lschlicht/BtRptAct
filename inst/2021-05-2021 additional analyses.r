#correlation with Smart Feeder data#####
#create Smart Feeder Data
con = dbcon("lschlicht")
sf = dbq(con, "SELECT DISTINCT DATE(datetime_) as date_, transp FROM SFatWESTERHOLZ.feeders_v1")
sf[, season := year(date_)]
sf[, date_ := yday(date_)]
sf[, date_ := ifelse(date_ < yday("1970-08-01"), date_, date_-max(date_)), by = season] #max_visit to account for "Schaltjahre"
sf[, season := ifelse(date_ >= 0, season, season + 1)]
sf[, min_date := min(date_, na.rm = TRUE), by = .(season, transp)]
sf[, date_ := NULL]
sf = unique(sf)
#add ID
tr = unique(rbind(dbq(con, "SELECT DISTINCT ID, transponder as transp FROM BTatWESTERHOLZ.ADULTS where transponder is not NULL"),
      dbq(con, "SELECT DISTINCT ID, transponder as transp FROM BTatWESTERHOLZ.CHICKS where transponder is not NULL")))
sf = merge(sf, tr, by = "transp")

#load SNB data
#load("/ds/grpkempenaers/Lotte/R Studio projects/Data for BtRptAct/Data/2020-06-25 11:54:23_wd_data.RData")
snb = copy(wd[[4]])

#merge the two data sets
sf = merge(sf, snb, by = c("season", "ID"))
sf2 = sf[, .(ID, season, arrivalArea, min_date)]

#C.Gilsenan used the earliest report of a bird (either by box or feeder), so for comparison take the minimum value of arrivalArea and min_date and paste that into min_date
sf2[, arrivalGilsenan := min(arrivalArea, min_date), by = rownames(sf2)]

#remove NAs
sf2 = sf2[!is.na(arrivalArea) & !is.na(arrivalGilsenan)]

plot(sf2$arrivalArea ~ sf2$arrivalGilsenan)
cor(sf2$arrivalArea, sf2$arrivalGilsenan, use = "pairwise.complete")
nrow(sf2)

#correlation between timing of activity and timing of provisioning during breeding#####
plot(med_time_to_sunrise_min.inc ~ med_bt_time_to_firstFeed_min, data = wd[[4]])
plot(med_time_to_sunset_min.inc ~ med_bt_time_to_lastFeed_min, data = wd[[4]])
m = lmer(med_bt_time_to_firstFeed_min ~ med_time_to_sunrise_min.inc + (1|ID), data = wd[[4]])
m = lmer(med_bt_time_to_lastFeed_min ~ med_time_to_sunset_min.inc + (1|ID), data = wd[[4]])
m = lmer(med_bt_midpointFeed_min ~ med_bt_midpoint.inc + (1|ID), data = wd[[4]])
m = lmer(med_activeFeed ~ med_active.inc + (1|ID), data = wd[[4]])
hist(resid(m))
qqnorm(resid(m))
summary(m)
dt(2.99, 332)
