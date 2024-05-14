#repair arrival, birds that got implanted in a given season were not removed
#0. remove arrival dates of birds that acquired their transponder within a given season
con = dbcon()
firstTr = dbq(con, "SELECT DISTINCT MIN(date_time) AS implant_date, ID, transponder FROM (SELECT capture_date_time AS date_time, ID, transponder FROM BTatWESTERHOLZ.ADULTS WHERE transponder IS NOT NULL
      UNION
      SELECT date_time, ID, transponder FROM BTatWESTERHOLZ.CHICKS WHERE transponder IS NOT NULL) a
      GROUP BY a.transponder")

closeCon(con)

firstTr[, season := year(implant_date)]
firstTr[, season := ifelse(yday(implant_date) <= yday(as.IDate("2019-07-31")), season, season+1)]
firstTr[, season := min(season), by = ID]
firstTr[, implant_date := NULL]
firstTr[, transponder := NULL]
firstTr = unique(firstTr)

load("/ds/grpkempenaers/Lotte/R Studio projects/Data for BtRptAct/Data/2021-05-21 11:27:11_wd_data.RData")

for(i in 1 : length(wd)) {
    wd[[i]] = merge(wd[[i]], firstTr, by = "ID", all.x = TRUE, suffixes = c("", ".implant"))
    wd[[i]][season <= season.implant, arrivalArea := NA]

    tmp = unique(subset(wd[[i]], select = c('arrivalArea', 'season', 'sex')))[, z_arrivalArea := scale(arrivalArea), by = .(season, sex)]
    wd[[i]] = merge(wd[[i]], tmp, by = c('arrivalArea', 'season', 'sex'), all.x = TRUE)
    rm(tmp)
}
save(wd, file = "/ds/grpkempenaers/Lotte/R Studio projects/Data for BtRptAct/Data/2024-03-18_wd_data.RData")
