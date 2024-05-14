setwd("/ds/grpkempenaers/Lotte/R Studio projects/Data for BtRptAct")

require(SNB2)
df = data.table(box = rep(1:277, 10), from = rep(paste0(2010:2019, "-01-01 00:00:00"), each = 277), to = rep(paste0(2010:2019, "-12-31 00:00:00"), each = 277))
hooray = eva('lschlicht', df)
save(hooray, file = paste0(Sys.time(), "_raw_data.RData"))

load("/ds/grpkempenaers/Lotte/R Studio projects/Data for BtRptAct/Data/2020-01-27 12:50:56.071_raw_data.RData")

setwd("/ds/grpkempenaers/Lotte/R Studio projects/Data for BtRptAct")
xtmp = create_sleep_data(hooray)
wd = list()
a = Sys.time()
for(i in 1 : 20) {
print(i)
x2 = calculate_bt_variables(xtmp, N_bt = i)
x2 = create_ind_data(x2, N_season = i)
wd[[i]] = create_feeding_visits(hooray, N_bt = i, N_season = i, sea = x2[[2]], ind_br = x2[[1]])
wd[[i]][, z_useArrival := z_arrivalArea]

}
Sys.time() - a #takes about 3 mins
save(wd, file = paste0(getwd(), "/Data/", Sys.time(), "_wd_hatched>3_data.RData"))


