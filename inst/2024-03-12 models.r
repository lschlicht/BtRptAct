#removed effects of partner ID and removed run of individual seasons
pvals = function(t, df) return(2*pt(abs(t), df = df, lower.tail = FALSE))
require(lme4)
require(ggplot2)
require(gridExtra)
require(grid)
require(rptR)
require(data.table)
setwd("/ds/grpkempenaers/Lotte/R Studio projects/Data for BtRptAct")

#set main variables
MALE_COLOUR = "#5000FFFF"
FEMALE_COLOUR = "#FF758AFF"
INT_COLOUR = "grey"
BOTH_COLOUR = "black"
NNN = 4 #use min no. data points

##create data script stopped working
#load("/ds/grpkempenaers/Lotte/R Studio projects/Data for BtRptAct/Data/2024-03-12_wd.RData")

load("/ds/grpkempenaers/Lotte/R Studio projects/Data for BtRptAct/Data/2024-03-18_wd_data.RData")


#1: repeatabilities (including age class!)#####
vars = c("z_firstEgg", "z_useArrival", "med_bt_time_to_sunrise_min", "med_bt_time_to_sunset_min", "med_bt_midpoint", "med_active", "med_bt_time_to_firstFeed_min", "med_bt_time_to_lastFeed_min", "med_bt_midpointFeed_min", "med_activeFeed")
runthrough = data.table(resp = vars)
runthrough[, form := "+ (1|ID)" ]
runthrough[resp == "z_firstEgg", form := "+ (1|ID) + (1|IDpartner)" ]
runthrough[, grn := "c('ID')" ]
runthrough[resp == "z_firstEgg", grn := "c('ID', 'IDpartner')" ]
runthrough[, form := paste0("rpt(", resp, " ~ factor(age) ", form, ", grname = ", grn, ", data = tmp_sex, datatype = 'Gaussian', npermut = NP, ncores = 40, nboot = 1000)")]

#models ######
a = Sys.time()
R = list()
NP = 1000
N_ind_use = NNN
for(i in 1:nrow(runthrough)) {
  tmp = wd[[N_ind_use]]
  if(grepl("Feed", runthrough[i,resp])) tmp = subset(tmp, Nbr_Feed >= N_ind_use)
  if(grepl("med", runthrough[i,resp])) tmp = subset(tmp, Nall >= N_ind_use)
  for(j in 1:2) {
    tmp_sex = subset(tmp, sex == j & !is.na(get(runthrough[i, resp])) & !is.na(age))
    if(nrow(tmp_sex) <= 10) next;
    r = eval(parse(text = runthrough[i,form]))
    R[[length(R)+1]]  = data.table(R = r$R$ID, CIlow = r$CI_emp[[1]][1],  CIup = r$CI_emp[[2]][1], Rpartner = ifelse(is.null(r$R$IDpartner), NA, r$R$IDpartner), CIlowpartner = r$CI_emp[[1]][2],  CIuppartner = r$CI_emp[[2]][2], P_permut = r$P$P_permut, P_LRT = r$P$LRT_P, var = runthrough[i, resp], sex = j, N_ID = length(levels(r$mod@frame$ID)))
  }
}
Sys.time() - a # ca. 10 minutes for 1000 permutations

##tables ######
RR = rbindlist(R)
RR[P_permut >= 0.001, P_permut := round(P_permut, 3)]; RR[, P_permut := as.character(P_permut)]; RR[P_permut < 0.001, P_permut := "< 0.001"]
RR = data.table(cbind(RR[, round(.SD, digits = 2), .SDcols = sapply(RR, is.numeric)], RR[, var], P_permut = RR[, P_permut]))
RR[, txt := paste0(R, " [", CIlow, ";", CIup, "]")]
RR[!is.na(Rpartner), txt := paste0(txt, " ", Rpartner, " [", CIlowpartner, ";", CIuppartner, "]")]
RR = unique(RR[, .(V2, sex, txt, P_permut, N_ID)])
RR = merge(subset(RR, sex == 1), subset(RR, sex == 2), by = "V2", suffixes = c(".m", ".f"))

varsT = data.table(V2 = vars, ord = 1:length(vars))
RR = merge(RR, varsT)
setorder(RR, ord)
RR[, ord := NULL]
RR[, V2 := niceNames(V2)]
unique(RR)
write.csv(RR, file = paste0(getwd(), "/Results/repeatabilities.csv"), quote = FALSE, row.names = FALSE)
RR = data.table(read.csv(file = paste0(getwd(), "/Results/repeatabilities.csv")))

#2: all comparisons #####
#to exchange response and explanatory variable rename the respective columns in data.table "runthrough"
#Data (load if possible, takes a while to run)######
vars = c("z_firstEgg", "z_useArrival", "med_bt_time_to_sunrise_min", "med_bt_time_to_sunset_min", "med_bt_midpoint", "med_active", "med_bt_time_to_firstFeed_min", "med_bt_time_to_lastFeed_min", "med_bt_midpointFeed_min", "med_activeFeed")

runthrough = data.table(t(combn(vars,2)))
runthrough[, mod := "lmer(formula(V1 ~ V2 + factor(age) + (1|ID)), data = tmp_sex)"]
runthrough[, modInt := "lmer(formula(V1 ~ V2*factor(sex) + factor(age) + (1|ID)), data = tmp)"]
runthrough[, modComb := "lmer(formula(V1 ~ V2 + factor(age) + (1|ID)), data = tmp)"]
runthrough[, type := "all_noIDpartner"]

runthrough3 = list()
for(ii in unique(wd[[1]]$season)) {
  runthrough3[[length(runthrough3)+1]] = copy(runthrough)
  runthrough3[[length(runthrough3)]][, type := ii]
  runthrough3[[length(runthrough3)]][, mod := "lm(formula(V1 ~ V2 + factor(age)), data = tmp_sex)"]
  runthrough3[[length(runthrough3)]][, modInt := "lm(formula(V1 ~ V2*factor(sex) + factor(age)), data = tmp)"]
  runthrough3[[length(runthrough3)]][, modComb := "lm(formula(V1 ~ V2 + factor(age)), data = tmp)"]
}
runthrough3 = rbindlist(runthrough3)

runthrough = rbindlist(list(runthrough, runthrough3))

a = Sys.time()
M1 = list()
for(N_ind in 1:10){ # runs ca. 6 minutes
  print(paste0("N = ", N_ind))
  for(i in 1 : nrow(runthrough)) {
    tmp = copy(wd[[N_ind]])
    if(nchar(runthrough[i, type]) == 4) { tmp = subset(tmp, season == runthrough[i,type])}
    if(grepl("Feed", runthrough[i,V1]) | grepl("Feed", runthrough[i,V2])) tmp = subset(tmp, Nbr_Feed >= N_ind)
    if(grepl("med", runthrough[i,V1]) | grepl("med", runthrough[i,V2])) tmp = subset(tmp, Nall >= N_ind)
    tmp[, V1 := get(runthrough[i, V1])]
    tmp[, V2 := get(runthrough[i, V2])]
    tmp = subset(tmp, !is.na(V1) & !is.na(V2))
    if(nrow(tmp) <= 10 | length(unique(tmp$age)) <= 1) next;

    #models for males and females
    for(s in 1:2) {
        tmp_sex = subset(tmp, sex == s)
        if(nrow(tmp_sex) > 10 & length(unique(tmp_sex$age)) > 1 & length(unique(tmp_sex[, ID])) > nrow(tmp_sex)) {
          m = eval(parse(text = runthrough[i,mod]))
          print(i)
          hist(resid(m))
          #Sys.sleep(0.5)
          qqnorm(resid(m))
          #Sys.sleep(0.5)
          if(nchar(runthrough[i, type]) > 4) { N = nrow(m@frame) } else N = nrow(m$model)
          m = summary(m)$coefficients[2,]
          M1[[length(M1)+1]] = data.table(resp = runthrough[i, V1], expl = runthrough[i, V2], Est = m[1], SE = m[2], t = m[3], sex = s, N_ind = N_ind, N = N, type = runthrough[i, type])
        }
    }

    #model interaction
    if(length(unique(tmp$sex)) > 1 & length(unique(tmp$age)) > 1) {
    m = eval(parse(text = runthrough[i,modInt]))
    print(i)
    hist(resid(m))
    #Sys.sleep(0.5)
    qqnorm(resid(m))
    #Sys.sleep(0.5)
    if(nchar(runthrough[i, type]) > 4) { N = nrow(m@frame) } else N = nrow(m$model)
    m = summary(m)$coefficients[5,]
    M1[[length(M1)+1]] = data.table(resp = runthrough[i, V1], expl = runthrough[i, V2], Est = m[1], SE = m[2], t = m[3], sex = "int", N_ind = N_ind, N = N, type = runthrough[i, type])
    }

  #both sexes combined
  m = eval(parse(text = runthrough[i,modComb]))
  print(i)
  hist(resid(m))
  #Sys.sleep(0.5)
  qqnorm(resid(m))
  #Sys.sleep(0.5)
  if(nchar(runthrough[i, type]) > 4) { N = nrow(m@frame) } else N = nrow(m$model)
  m = summary(m)$coefficients[2,]
  M1[[length(M1)+1]] = data.table(resp = runthrough[i, V1], expl = runthrough[i, V2], Est = m[1], SE = m[2], t = m[3], sex = "both", N_ind = N_ind, N = N, type = runthrough[i, type])
  }
}


Sys.time() - a
save(M1, file = paste0(getwd(), "/Results/", Sys.Date(), "_M1.RData"))


#Figure ######
#load(file = paste0(getwd(), "/Results/2023-04-19_M1.RData"))

MM1 = rbindlist(M1)
MM1[, ":=" (Est = round(Est, 2), SE = round(SE, 2), t = round(t, 2))]
MM1[, CIlow := Est-1.96*SE]
MM1[, CIup := Est+1.96*SE]
OFFSET = 0.2
MM1[sex == 1, ":=" (COL = MALE_COLOUR, offset = OFFSET)]
MM1[sex == 2, ":=" (COL = FEMALE_COLOUR, offset = 0.5*OFFSET)]
MM1[sex == "int", ":=" (COL = INT_COLOUR, offset = -0.5*OFFSET)]
MM1[sex == "both", ":=" (COL = BOTH_COLOUR, offset = -OFFSET)]


MM1.sensitivity = subset(MM1, nchar(type) > 4)
tests = unique(subset(MM1.sensitivity, select = c(resp, expl)))

for(k in unique(MM1.sensitivity[, type])) {
  jpeg(filename = paste0(getwd(), "/Figures/RF1_Sensitivity analysis_Table 2.jpeg"), width = 1800, height = 1200, quality = 100)
par(mfrow = c(5,9))
par(las = 1)
par(mar = c(2.5, 3.5, 0.3, 0.4))
par(cex = 1.5)
for(i in 1 : nrow(tests)) {
  tmp0 = subset(MM1.sensitivity, type == k & resp == tests[i,resp] & expl == tests[i, expl])
  plot(c(1,10), c(min(tmp0[, CIlow]), max(tmp0[,CIup])), type = "n", xlab = "", ylab = "", yaxt = 'n')
  title(ylab = paste(niceNamesFigures(tests[i, resp]), "~", niceNamesFigures(tests[i, expl])), line = 2.5)
  axis(2, at = (-10:10)/10, labels = (-10:10)/10)
  for(j in rev(unique(tmp0$sex))) {
  tmp = subset(tmp0, sex == j)
  points(x = tmp$N_ind+tmp$offset, y = tmp$Est, pch = 16, col = tmp$COL)
  arrows(x0 = tmp$N_ind+tmp$offset, y0 = tmp$CIlow, x1 = tmp$N_ind+tmp$offset, y1 = tmp$CIup, code = 3, angle = 90, length = 0.05, col = tmp$COL)
  abline(h = 0, col = "dark green")
  }

}
dev.off()
}


#based on this figure above, use N = 4 for table and for figure of individual years.

#Tables for export######
MM2 = subset(MM1, N_ind == NNN)
#significance level
MM2[, p := round(dt(t, N), digits = 3)]
MM2[p < 0.05 & p >= 0.01, isSig := "*"]
MM2[p < 0.01 & p >= 0.001, isSig := "**"]
MM2[p < 0.001, isSig := "***"]
MM2[p >= 0.05, isSig := ""]
MM2[, txt := paste0(Est, "±", SE, isSig)]

#add whether interaction is significant for Table 2
MM2[sex == "int", txt_main := ifelse(isSig == "", 0, 1)]
MM2[, txt_main := na.omit(txt_main), by = .(resp, expl, type)]
MM2[sex == 1 | sex == 2, tmp1 := txt]
MM2[, tmp1 := paste(na.omit(tmp1), collapse = " "), by = .(resp, expl, type)]
MM2[, txt_main := ifelse(txt_main == 0, txt, tmp1)]

#add years for supplementary tables
MM2[nchar(type) == 4, sameSign :=  paste0(max(c(length(which(sign(Est) < 0 | Est == 0)), length(which(sign(Est) > 0 | Est == 0)))), "/", .N), by = .(resp, expl, sex)]
MM2[, sameSign := unique(na.omit(sameSign)), by = .(resp, expl, sex)]
MM2[, txt2 := paste0(txt, "(N=", N, ";", sameSign, ")")]
MM2 = MM2[nchar(type) > 4, ]

MM2[, ":=" (Est = NULL, SE = NULL, t = NULL, CIlow = NULL, CIup = NULL, COL = NULL, offset = NULL, sameSign = NULL)]
MM2


NAMES = unique(c(MM2$resp, MM2$expl))
niceNAMES = niceNames(NAMES)

#Table 2 #######
out = matrix(nrow = length(NAMES), ncol = length(NAMES), dimnames = list(NAMES, NAMES))
for(i in 1 : nrow(out)){
  for(j in 1:ncol(out)) {
    if(j > i) TXT = MM2[resp == rownames(out)[i] & expl == colnames(out)[j] & sex == "both", txt_main]
    if(j < i) TXT = "-"
    if(j == i) TXT = "-"
    if(length(TXT) == 0) TXT = "/"
    out[i,j] = TXT
  }
}
rownames(out) = niceNAMES
colnames(out) = niceNAMES
out
write.csv(out, file = paste0(getwd(), "/Results/Table2.csv"), quote = FALSE)


#Table S1 #######
out = matrix(nrow = length(NAMES), ncol = length(NAMES), dimnames = list(NAMES, NAMES))
for(i in 1 : nrow(out)){
  for(j in 1:ncol(out)) {
    if(j > i) TXT = MM2[resp == rownames(out)[i] & expl == colnames(out)[j] & sex == "both", txt2]
    if(j < i) TXT = MM2[expl == rownames(out)[i] & resp == colnames(out)[j] & sex == "int", txt2]
    if(j == i) TXT = "-"
    if(length(TXT) == 0) TXT = "/"
    out[i,j] = TXT
  }
}
rownames(out) = niceNAMES
colnames(out) = niceNAMES
out
write.csv(out, file = paste0(getwd(), "/Results/TableS1.csv"), quote = FALSE)

#Table S2 #######
out = matrix(nrow = length(NAMES), ncol = length(NAMES), dimnames = list(NAMES, NAMES))
for(i in 1 : nrow(out)){
  for(j in 1:ncol(out)) {
      if(j > i) TXT = MM2[resp == rownames(out)[i] & expl == colnames(out)[j] & sex == 1, txt2]
      if(j < i) TXT = MM2[expl == rownames(out)[i] & resp == colnames(out)[j] & sex == 2, txt2]
      if(j == i) TXT = "-"
      if(length(TXT) == 0) TXT = "/"
    out[i,j] = TXT
  }
}
rownames(out) = niceNAMES
colnames(out) = niceNAMES
out
write.csv(out, file = paste0(getwd(), "/Results/Table S2.csv"), quote = FALSE)

Sys.time() - a



