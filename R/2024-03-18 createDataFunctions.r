
#### 1. subfunctions ####
{
  addID = function(con, x) {
    copy(x) -> X
    IDs = unique(dbq(con, "SELECT ID, transponder FROM BTatWESTERHOLZ.ADULTS WHERE transponder IS NOT NULL UNION
SELECT ID, transponder FROM BTatWESTERHOLZ.CHICKS WHERE transponder IS NOT NULL;"))
    X = merge(X, IDs, by.x = "transp", by.y = "transponder", all.x = TRUE)
    return(X)
  }
  addBreeding = function(con, x) {
    copy(x) -> X
    males = dbq(con, "SELECT year_, box, IDmale as ID, DATE(firstEgg) as firstEgg, DATE(hatchDate) as hatchDate, DATE(lastHatchDate) as lastHatchDate, DATE(fledgeDate) as fledgeDate, clutch, hatched, fledged, laying_gap FROM BTatWESTERHOLZ.BREEDING where IDmale is not NULL")
    females = dbq(con, "SELECT year_, box, IDfemale as ID, DATE(firstEgg) as firstEgg, DATE(hatchDate) as hatchDate, DATE(lastHatchDate) as lastHatchDate, DATE(fledgeDate) as fledgeDate, clutch, hatched, fledged, laying_gap FROM BTatWESTERHOLZ.BREEDING where IDfemale is not NULL")
    br = as.data.table(rbind(males, females))
    #take the first of multiple breeding attempts
    br[, firstAttempt := (firstEgg == min(firstEgg)), by = list(year_, ID)]
    br = subset(br, firstAttempt == TRUE)
    br[, firstAttempt := NULL]
    br = unique(br, by = names(br))
    X[, season := ifelse(yday(date_) > 200 | year(date_) != year_, year_ + 1, year_)]

    X = merge(X, br, by.x = c('season', 'ID'), by.y = c('year_', 'ID'), all.x = TRUE, suffixes = c('.sleep', '.breeding'))
    X[, rel_day := as.numeric(date_ - as.IDate(firstEgg))]
    X[, rel_day_dusk := as.numeric(date_ - as.IDate(firstEgg))-1]

    X = subset(X, rel_day > -300 | is.na(rel_day))
    return(X)
  } #this function also removes true replacement broods!

  addWeather = function(con, x) {
    copy(x) -> X

    #fetch weather data
    w = dbq(con, "SELECT HOUR(datetime_) as hour_, DATE(datetime_) as date_, temperature, precipitation, humidity, wind, radiation FROM LOGGERSatWESTERHOLZ.ENVIRONMENTAL")
    w[, date_ := as.IDate(date_)]

    X[, hour_ := ifelse(!is.na(out_), hour(as.ITime(out_)), hour(as.ITime(in_)))]
    X = merge(X, w, by = c('date_', 'hour_'), all.x = TRUE, all.y = FALSE)
    X[, hour_ := NULL]

    #dusk
    w[, date_ := as.IDate(date_)+1] #so that the dusk variables are for the previous night
    X[, hour_ := ifelse(!is.na(in_), hour(as.ITime(in_)), hour(as.ITime(out_)))]
    X = merge(X, w, by = c('date_', 'hour_'), all.x = TRUE, all.y = FALSE, suffixes = c('_dawn', '_dusk'))
    X[, hour_ := NULL]

    #daylength (daily averages of the day of entry/the day before emergence)
    w[, avgRainfall := mean(precipitation, na.rm = TRUE), by = date_]
    w[, avgTemperature := mean(temperature, na.rm = TRUE), by = date_]
    w[, avgHumidity := mean(humidity, na.rm = TRUE), by = date_]
    w = subset(w, select = c('avgRainfall', 'avgTemperature', 'avgHumidity', 'date_'))
    w = unique(w, by = names(w))
    X = merge(X, w, by = 'date_', all.x = TRUE, all.y = FALSE)




    return(X)
  }

  addIndVar = function(con, x) {
    copy(x) -> X
    ind1 = dbq(con,  "SELECT season as capture_season, ID, tarsus, age FROM BTatWESTERHOLZ.ADULTS")
    ind2 = dbq(con,  "SELECT year_ as capture_season, ID, tarsus FROM BTatWESTERHOLZ.CHICKS")
    ind2[, age := 0]

    ind = rbind(ind1, ind2)
    ind[ , ageAtFirstCapture := min(age, na.rm = TRUE), by = ID]
    ind[, seasonOfFirstCapture := capture_season[which.min(age)[1]], by = ID]
    ind[ageAtFirstCapture == 'Inf', ':=' (seasonOfFirstCapture = min(capture_season), ageAtFirstCapture = NA)]

    ind[,tarsus := mean(tarsus, na.rm = TRUE), by = ID]
    ind[, capture_season := NULL]
    ind[, age := NULL]
    ind = unique(ind, by = names(ind))
    X = merge(X, ind, by = 'ID', all.x  = TRUE)
    X[, minAge := year_ - seasonOfFirstCapture + ageAtFirstCapture + ifelse(month(date_) > 6, 1, 0)]
    X[, age := ifelse(minAge > 1, 2, 1)]
    X[, ageAtFirstCapture := NULL]
    X[, seasonOfFirstCapture := NULL]
    return(X)
  }

  addSunriseSunset = function(x) {
    copy(x) -> X

    crds <- matrix(c(10.88, 48.13), nrow=1)
    dates <- as.POSIXct("2009-03-01", tz="Etc/GMT-2")
    crds_seq <- seq(from=dates, length.out=90000, by="days")

    #sunrise
    up <- as.data.table(sunriset(crds, crds_seq, direction="sunrise", POSIXct.out=TRUE))
    up[, date_ := as.Date(as.POSIXct(up$time))]
    up = up[, c('time', 'date_'), with = FALSE]
    names(up) = c('sunrise', 'date_')

    #sunset
    down <- as.data.table(sunriset(crds, crds_seq, direction="sunset", POSIXct.out=TRUE))
    down[, date_ := as.Date(as.POSIXct(down$time))]
    down = down[, c('time', 'date_'), with = FALSE]
    names(down) = c('sunset', 'date_')
    down[, date_ := date_+1]

    updown = merge(up, down, by = 'date_')

    #some out-durations are too long, set all that are longer than 500 seconds to 500 seconds (N = 697, 0.7%)
    X[out_duration > 500, out_duration := 500]


    X = merge(X, updown, by = 'date_')
    X[, time_to_sunrise := (out_time+out_duration)-as.numeric(as.ITime(sunrise))]
    X[, time_to_sunset := in_time-as.numeric(as.ITime(sunset))]
    X[, time_to_sunrise_min := as.numeric(time_to_sunrise)/60]
    X[, time_to_sunset_min := as.numeric(time_to_sunset)/60]


    return(X)
  }

  addNestStage = function(con, x) {
    copy(x) -> X
    nests = dbq(con, "SELECT * FROM BTatWESTERHOLZ.NESTS")
    nests[, nest_start := as.IDate(min(c(date_B, date_C, date_LIN), na.rm = TRUE)), by = npk]
    nests[, nest_completed := as.IDate(date_LIN)]
    nests = subset(nests, select = c('box', 'year_', 'nest_start', 'nest_completed'))
    X = merge(X, nests, by.x = c('season', 'box.breeding'), by.y = c('year_', 'box'), all.x = TRUE)
    X[is.na(nest_start) & is.na(nest_completed), ':=' (nest_start = as.IDate(firstEgg), nest_completed = as.IDate(firstEgg))]
    X[!is.na(nest_start) & is.na(nest_completed), ':=' (nest_completed = as.IDate(firstEgg))]
    return(X)
  }

  addSleep = function(x, threshold_sleep = 180) {
    copy(x) -> X
    X[, sleep := 0]
    X[!is.na(out_) & (as.Date(in_)+1) == as.Date(out_) , sleep := 0.5]
    X[abs(time_to_sunset_min) < threshold_sleep & abs(time_to_sunrise_min) < threshold_sleep & !is.na(in_) & !is.na(out_) & (as.Date(in_)+1) == as.Date(out_) , sleep := 1]
    return(X)
  }

  addSex = function(con, x) {
    copy(x) -> X
    sex = dbq(con, "SELECT ID, sex FROM BTatWESTERHOLZ.SEX where SEX is not NULL")
    X = merge(X, sex, by = "ID", all.x = TRUE)
    return(X)
  }

  addEPP = function(con, x) {

    eppGain = dbq(con, " select * from (
                (SELECT DISTINCT year_, sum(epy) AS EPP_gain, father AS IDmale
                FROM
                (SELECT year_, epy, father FROM BTatWESTERHOLZ.PATERNITY P
                WHERE father IS NOT NULL) AS p1

                GROUP BY year_, father HAVING  sum(epy) IS NOT NULL) AS Y
                LEFT OUTER JOIN

                --  NO OF EP FEMALES
                ( SELECT DISTINCT year_, count(DISTINCT mother) AS EP_females, father AS IDmale
                FROM
                (SELECT year_, epy, mother, father FROM BTatWESTERHOLZ.PATERNITY P
                WHERE  father IS NOT NULL) AS p2

                WHERE father IS NOT NULL AND epy = 1
                GROUP BY year_, father ) AS F
                ON
                Y.IDmale = F.IDmale AND Y.year_ = F.year_ ) ; ")
    eppGain = eppGain[, -6, with  =FALSE]
    eppGain = eppGain[, -4, with  =FALSE]
    setnames(eppGain, "IDmale", "ID")
    setnames(eppGain, "year_", "season")

    eppLossM = dbq(con, "SELECT year_ as season, father as ID, sum(epy) as epy FROM BTatWESTERHOLZ.PATERNITY WHERE father is not NULL and epy is not NULL group by year_, father")
    eppLossF = dbq(con, "SELECT year_ as season, mother as ID, sum(epy) as epy FROM BTatWESTERHOLZ.PATERNITY WHERE mother is not NULL and epy is not NULL group by year_, mother")
    eppLoss = rbind(eppLossM, eppLossF)

    x = merge(x, eppGain, by = c('season', 'ID'), all.x = TRUE)
    x = merge(x, eppLoss, by = c('season', 'ID'), all.x = TRUE)
    setnames(x, "epy", "EPP_loss")
    x[is.na(EP_females) & sex == 1, EP_females := 0]
    return(x)
  }

  markReplacements = function(con, x) {
    X = copy(x)
    fe = dbq(con, "SELECT DATE(firstEgg) as firstEgg, year_ as breeding_season, box FROM BTatWESTERHOLZ.BREEDING")
    #fe[, firstEgg := scale(as.IDate(firstEgg), center = TRUE, scale = FALSE), by = breeding_season]
    fe[, firstEgg := as.numeric(as.IDate(firstEgg))]
    fe[, max_ := ifelse(firstEgg > boxplot.stats(firstEgg)$stats[5], 1, 0), by = breeding_season]
    fe = subset(fe, max_ == 1)
    fe[, firstEgg := as.IDate(firstEgg, origin = "1970-01-01")]

    X[, replacementClutchOutlier := 0]
    X[paste(season, box.breeding) %in% paste(fe[,breeding_season], fe[,box]), replacementClutchOutlier := 1]

    return(X)
  }

  markAndRemoveExperiments = function(con, x) {

    #LEDs 2012, 2013
    exp12 = dbq(con, "SELECT box, date(Installed) as installed, date(turnedOff) as turnedOff FROM EXTRA_BTatWESTERHOLZ.2012_LS_LEDs where ExpOrControl = 'exp'")
    exp13 = dbq(con, "SELECT box, date(installed) as installed, date(turnedOff) as turnedOff FROM EXTRA_BTatWESTERHOLZ.2013_LS_LEDs where ExpOrControl = 'exp'")

    exp12[, turnedOff := as.IDate(turnedOff)]
    exp13[, turnedOff := as.IDate(turnedOff)]
    exp12[is.na(turnedOff), turnedOff := as.IDate("2012-05-30")]
    exp13[is.na(turnedOff), turnedOff := as.IDate("2013-05-30")]
    expLED = rbind(exp12, exp13)
    expLED = split(expLED, rownames(expLED))
    expLED = rbindlist(lapply(expLED, FUN = function(y) { dates = as.IDate(y[1, installed]) : as.IDate(y[1, turnedOff]); y = data.table(date_ = as.IDate(dates), box.sleep = rep(y[1,box], length(dates))); return(y)}))
    expLED[, remove := 1]
    x = merge(x, expLED, all.x = TRUE, by = c("date_", "box.sleep"))
    x[is.na(remove), experimental := 0]
    x[remove == 1, experimental := 1]
    x[, remove := NULL]

    #predation experiment 2017 - part 1
    exp17.1 = dbq(con, "SELECT * FROM FIELD_2017_BTatWESTERHOLZ.EXPERIMENTS")[1,]

    a = exp17.1$'function';  a = gsub("\r", ' ', a);  a = gsub("\n", ' ', a);  a = gsub("\t", ' ', a); a = substring(a, 1, 2122); a = paste(a, "}", sep = ''); exp17.1 = eval(parse(text = a))
    exp17.1 = as.data.table(exp17.1())
    exp17.1 = subset(exp17.1, treatment == "Predator")
    exp17.1[, treatment := NULL]
    exp17.1[, date_ := as.IDate(date_)]
    setnames(exp17.1, "box", "box.sleep")

    #predation experiment 2017 - part 2
    exp17.2 = dbq(con, "SELECT * FROM FIELD_2017_BTatWESTERHOLZ.EXPERIMENTS")[2,]
    a = exp17.2$'function';  a = gsub("\r", ' ', a);  a = gsub("\n", ' ', a);  a = gsub("\t", ' ', a); a = substring(a, 1, 6329); a = paste(a, "}", sep = ''); exp17.2 = eval(parse(text = a))
    exp17.2 = as.data.table(exp17.2())
    exp17.2 = subset(exp17.2, experimenter %in% c('BA', 'L', 'T')) #predator: BA, L, T; control: W, BU, C
    exp17.2[, experimenter := NULL]
    exp17.2[, date_ := as.IDate(date_)]
    setnames(exp17.2, "box", "box.sleep")

    #combine both 2017 experiments
    exp17 = rbind.data.frame(exp17.1, exp17.2)
    exp17[, remove := 1]
    x = merge(x, exp17, all.x = TRUE, by = c("date_", "box.sleep"))
    x[remove == 1, experimental := 1]
    x[, remove := NULL]

    return(x)
  }
}

#### 2. function calls ####
{

  create_sleep_data = function(hooray) {
    #reduce to sleep
    dat = hooray[as.IDate(in_)+1 == as.IDate(out_),]
    dat[, date_ := as.IDate(out_)]
    dat[, date_in := as.IDate(in_)]
    dat[, year_ := year(date_)]
    dat[, in_time := as.ITime(in_)]
    dat[, out_time := as.ITime(out_)]
    copy(dat) -> x
    con = dbcon()
    x = addID(con, x)
    x = addSex(con, x)
    x = addBreeding(con, x)
    x = addIndVar(con, x) #warning message about "Inf" is ok
    x = addSunriseSunset(x)
    x = addEPP(con, x)
    x[, EPP_lossYN := ifelse(EPP_loss > 0, 1, EPP_loss)]
    x[, EPP_gainYN := ifelse(EPP_gain > 0, 1, EPP_gain)]
    x[, yid := paste(substring(firstEgg, 1, 4), ID, sep = '_')]
    x = markReplacements(con, x)
    x = markAndRemoveExperiments(con, x)
    x = subset(x, experimental == 0)
    setkey(x, year_, box.breeding, ID, date_)
    closeCon(con)

    #further transformations
    x = subset(x, !is.na(ID) & !is.na(sex))
    x[, in_r_pk := NULL]
    x[, out_r_pk := NULL]
    x[, direction_detail := NULL]
    x[, direction := NULL]
    x[, type := NULL]
    x = unique(x)
    x[, firstEgg := as.IDate(firstEgg)]
    #remove individuals that are recorded as sleeping on the same date twice: this can occur for several reasons:
    #1. data bug: apparently the same data recorded twice, e.g. data fragments on SD cards, misplacement (only very old data)
    #2. errors in the assignment of the directions: if a nestbox (e.g. breeding box) was visited in the evening and morning, while sleep took place in another box. Remove all such data (sample size of removed data see below)
    tmp = subset(x, select= c('date_', 'ID'))
    tmp[, N := .N, by = .(date_,ID)]
    tmp = subset(tmp, N > 1)
    nrow(tmp)/nrow(x) #2.8%; 1.4% is actually wrong, but I'm not sure which, so I'm removing all just in case
    x = subset(x, !(paste0(date_, ID) %in% paste0(tmp[, date_],tmp[, ID])))

    #scale first egg within season, but take into account that individual data points were measured multiple times!
    tmp = unique(subset(x, select = c('firstEgg', 'season', 'box.breeding')))[, z_firstEgg := scale(firstEgg), by = season]
    x = merge(x, tmp, by = c('firstEgg', 'season', 'box.breeding'))
    #checks
    tmp = unique(subset(x, select = c("ID", "year_", "sex")))
    tmp[, count_ := .N, by = list(ID)]
    tmp[, ":=" (year_ = NULL)]
    tmp = unique(tmp)
    table(tmp$count_, tmp$sex)

    #do NOT restrict to breeding individuals
    #x = subset(x, !is.na(firstEgg))
    x[, yid := paste0(year_, ID)]

    #add hatch date ("birth") ###RESET
    con = dbcon()
    ha = dbq(con, "SELECT year_, box, date(hatchDate) as birth, date(firstEgg) as birthEgg FROM BTatWESTERHOLZ.BREEDING where hatchDate is not NULL")
    ha[, birth := yday(birth)]
    ha[, birthEgg := yday(birthEgg)]
    ch = dbq(con, "SELECT year_, box, ID FROM BTatWESTERHOLZ.CHICKS")
    ha = merge(ha, ch)
    ha[, boxBirth := paste(box, year_, sep = '_')]
    ha[, ":=" (box = NULL, year_ = NULL)]
    closeCon(con)
    x = merge(x, by = "ID", ha, all.x = TRUE, all.y = FALSE)



    #add day-of-year column
    x[, YDAY := yday(date_)]
    x[YDAY > 250, YDAY := as.integer(YDAY - 366)] #to make sure that fall data is attributed to the upcoming breeding season

    #add time span active
    x[, active := as.numeric(in_time - out_time)/60/60]

    #cut off data to exclude only defintely wrong data
    #do not use outlier definition but cut off anything that is definitely wrong:
    #for time_to_sunrise_min use - 1.5 hours because it"s still pitch dark for sunrise and + 1.5 hours because we know almost all females have left by then (see Schlicht et al. 2012) and use the same for the evening for similar reasons. Note that because we are using medians for the transformations and sensitivity analyses for the number of required points the exact cutoff does not matter.
    #create boxplot elsewhere
    x2 = x[time_to_sunrise_min >= -90 & time_to_sunrise_min <= 90 & time_to_sunset_min >= -90 & time_to_sunset_min <= 90,]
    nrow(x2) / nrow(x) #0.93% of data points

    #remove any data after fledging of the offspring / rel_day <= 50 (there's a strong drop there anyway)
    tmp = nrow(x2)
    x2 = subset(x2, is.na(rel_day) | rel_day <= 50) #30 rows
    nrow(x2)/tmp #99.97%


    #add visits to nestboxes to dataset
    con = dbcon()
    v = dbq(con, 'SELECT DISTINCT date(datetime_) as Visit, site as box, transponder as transp, site_type FROM BTatWESTERHOLZ.transponders')
    closeCon(con)
    v[, Visit := as.IDate(Visit)]
    v = subset(v, !is.na(Visit))
    v[, season := year(Visit)]
    #define "season" as defined by Carol: 01 August - 31 July (J. Anim. Ecol. 2019)
    v[, season := ifelse(yday(Visit) <= yday(as.IDate("2019-07-31")), season, season+1)]
    v[, Visit := yday(Visit)]
    setkey(v, season, transp, Visit)

    hist(v$Visit, 200)
    v[, Visit := ifelse(Visit < yday("1970-08-01"), Visit, Visit-max(Visit)), by = season] #max_visit to account for "Schaltjahre"
    hist(v$Visit, 200)
    v[, arrivalArea := min(Visit, na.rm = TRUE), by = list(season, transp)]
    v = subset(v, site_type == 1)
    v[, arrival := min(Visit, na.rm = TRUE), by = list(season, transp, box)]
    v[, Visit := NULL]

    v = unique(v)

    x2 = merge(x2, v, by.x = c('transp', 'season', 'box.breeding'), by.y = c('transp', 'season', 'box'), all.x = TRUE, all.y = FALSE)


    x2 = subset(x2, minAge > 0) #all these are an SNB errors.

    #create breeding and non-breeding
    x2[, br := ifelse(rel_day < -21 | yday(date_) < 75 | yday(date_) > 180, "0_nb", "1_br")] #for non breeding individuals use 15th of March and end of June as a cutoff

    # data from non-breeding individuals that take place during the breeding season are removed (N = 5256, 6% of all data)
    x2 = subset(x2, !is.na(br))

    #use only females during incubation (defined as first hatchling - 12 to day before first hatchling; first egg produces problems, because there are some (few) nests where the females did not start incubating immidiately); the nestlings might have hatched on the day before we do the nest check.
    x2 = x2[br == "0_nb" | (yday(date_) >= yday(hatchDate) - 12 & yday(date_) < (yday(hatchDate)-1) )]
    #also redefine rel_day as rel_day to first hatchling
    x2[br == "1_br", rel_day := yday(date_) - yday(hatchDate)]

    #if a bird recieved a new transponder within one season, take only the first (available) transponder for arrival
    x2[, arrival := min(arrival, na.rm = TRUE), by = .(ID, season)]
    #x2[, arrival_anywhere := min(arrival_anywhere, na.rm = TRUE), by = .(ID, season)]
    #x2[, arrival_territory := min(arrival_territory, na.rm = TRUE), by = .(ID, season)]
    #x2[, arrival_delta := max(arrival_delta, na.rm = TRUE), by = .(ID, season)]
    #x2[, arrival_deltaTerrVSall := min(arrival_deltaTerrVSall, na.rm = TRUE), by = .(ID, season)]

    #update variables
    #scale within season, but take into account that individual data points were measured multiple times!
    tmp = unique(subset(x2, select = c('arrival', 'season', 'box.breeding', 'sex')))[, z_arrival := scale(arrival), by = .(season, sex)]
    x2 = merge(x2, tmp, by = c('arrival', 'season', 'box.breeding', 'sex'), all.x = TRUE)
    tmp = unique(subset(x2, select = c('arrivalArea', 'season', 'box.breeding', 'sex')))[, z_arrivalArea := scale(arrivalArea), by = .(season, sex)]
    x2 = merge(x2, tmp, by = c('arrivalArea', 'season', 'box.breeding', 'sex'), all.x = TRUE)

    #add id of partner
    con = dbcon()
    par = rbind(dbq(con, "SELECT IDmale as ID, IDfemale as IDpartner, box, year_ as season FROM BTatWESTERHOLZ.BREEDING where IDmale is not NULL and IDfemale is not NULL"),
                dbq(con, "SELECT IDfemale as ID, IDmale as IDpartner, box, year_ as season FROM BTatWESTERHOLZ.BREEDING where IDmale is not NULL and IDfemale is not NULL"))
    closeCon(con)
    setnames(par, "box", "box.breeding")

    x2 = merge(x2, par, by = c('ID', 'box.breeding', 'season'), all.x = TRUE, all.y = FALSE)



    #save(x, file = paste0("Data/", Sys.time(), "_full_data.RData"))
    #save(x2, file = paste0("Data/", Sys.time(), "_model_data.RData"))

    return(x2)
  }

  median_n = function(x, N) { if(length(na.omit(x)) >= N) return(median(x, na.rm = TRUE)) else return(NA)}

  calculate_bt_variables = function(x2, N_bt) {
    copy(x2) -> X
    #add second variable
    X[, bt_sunrise := as.numeric(median_n(out_time + out_duration, N_bt)), by = yday(date_)]
    X[, bt_sunset := as.numeric(median_n(in_time, N_bt)), by = yday(date_)]
    X[, bt_time_to_sunrise := as.numeric((out_time+out_duration)-as.numeric(as.ITime(bt_sunrise)))]
    X[, bt_time_to_sunset := as.numeric(in_time-as.numeric(as.ITime(bt_sunset)))]
    X[, bt_time_to_sunrise_min := as.numeric(bt_time_to_sunrise)/60]
    X[, bt_time_to_sunset_min := as.numeric(bt_time_to_sunset)/60]

    X[, bt_midpoint := ((in_time - ((in_time - out_time)/2))/60/60) - 12]
    X[, bt_sunrise_time := as.numeric(as.ITime(bt_sunrise))]
    X[, bt_sunset_time := as.numeric(as.ITime(bt_sunset))]
    X[, bt_midnight := ((bt_sunset_time - ((bt_sunset_time - bt_sunrise_time)/2))/60/60) - 12]
    X[, bt_midpoint := bt_midpoint - bt_midnight]

    return(X)
  }

  scale3 = function(x) {x = x - median(x, na.rm = TRUE); x = x / sd(x, na.rm = TRUE); return(x)}

  calc_indVar = function(data, variable){
    copy(data) -> dat

    dat[, tmp := scale3(get(variable)), by = group]

    dat[, tmp1 := median(tmp, na.rm = TRUE), by = list(ID, season, br)]
    setnames(dat, "tmp1", paste0("med1_", variable))
    if(!("Nbr" %in% names(dat))) { dat[, Nbr := .N, list(ID, season, br)]}

    dat[, tmp := median(tmp, na.rm = TRUE), by = list(ID, season)]
    setnames(dat, "tmp", paste0("med_", variable))
    if(!("Nall" %in% names(dat))) { dat[, Nall := .N, list(ID, season)]}


    return(dat)
  }


  create_ind_data = function(x2, N_season = 3) {
    #compute average values for an individual based on the day to first egg (breeding season) or the week of the year(outside of breeding) and within season
    copy(x2) -> ind
    ind[, sex := factor(sex)]
    ind[, age := factor(age)]
    ind[, bt_midpoint := as.numeric(bt_midpoint)]

    #subset to those days where at least 5 data points are avaiable
    ind[br == "0_nb", group := paste(sex, as.character(date_))]
    ind[br == "1_br", group := paste(sex, rel_day, season)]
    ind[, use := .N, by = group]
    table(ind$use, ind$br, ind$sex)
    ind = subset(ind, use >= N_season)
    nrow(ind)/nrow(x2) #~0.98% left
    nrow(unique(subset(ind, sex == 1 & br == "1_br", select = c("ID", "season"))))/nrow(unique(subset(x2, sex == 1 & br == "1_br", select = c("ID", "season")))) #0.91
    nrow(unique(subset(ind, sex == 2 & br == "1_br", select = c("ID", "season"))))/nrow(unique(subset(x2, sex == 2 & br == "1_br", select = c("ID", "season")))) #1.00
    nrow(unique(subset(ind, sex == 1 & br == "0_nb", select = c("ID", "season"))))/nrow(unique(subset(x2, sex == 1 & br == "0_nb", select = c("ID", "season")))) #0.99
    nrow(unique(subset(ind, sex == 2 & br == "0_nb", select = c("ID", "season"))))/nrow(unique(subset(x2, sex == 2 & br == "0_nb", select = c("ID", "season")))) #0.95


    ind = calc_indVar(data = ind, variable = "time_to_sunrise_min")
    ind = calc_indVar(data = ind, variable = "bt_time_to_sunrise_min")
    ind = calc_indVar(data = ind, variable = "time_to_sunset_min")
    ind = calc_indVar(data = ind, variable = "bt_time_to_sunset_min")
    #ind = calc_indVar(data = ind, variable = "midpoint")
    ind = calc_indVar(data = ind, variable = "bt_midpoint")
    ind = calc_indVar(data = ind, variable = "active")

#RESET
    #ind_br = unique(subset(ind, select = c('ID', 'IDpartner', 'season', 'br', 'sex', 'age', 'minAge', 'firstEgg', 'arrival', 'arrivalArea', names(ind)[grep("med1", names(ind))], "hatchDate", "Nbr", "Nall")))
    #setorder(ind_br, "ID", "season", "br")
    #ind = unique(subset(ind, select = c('ID', 'IDpartner', 'season', 'sex', 'age', 'minAge', 'firstEgg', 'arrival', 'arrivalArea', names(ind)[grep("med_", names(ind))], "hatchDate", "Nall")))
    #setorder(ind, "ID", "season")

    ind_br = unique(subset(ind, select = c('ID', 'IDpartner', 'season', 'br', 'sex', 'age', 'minAge', 'firstEgg', 'arrival', 'arrivalArea', names(ind)[grep("med1", names(ind))], "hatchDate", "Nbr", "Nall", "birth", "birthEgg", "boxBirth")))
    setorder(ind_br, "ID", "season", "br")
    ind = unique(subset(ind, select = c('ID', 'IDpartner', 'season', 'sex', 'age', 'minAge', 'firstEgg', 'arrival', 'arrivalArea', names(ind)[grep("med_", names(ind))], "hatchDate", "Nall", "birth", "birthEgg", "boxBirth")))
    setorder(ind, "ID", "season")


    ind_br[, z_hatchDate := scale(yday(hatchDate))]

    setnames(ind_br, names(ind_br), gsub("1", "", names(ind_br)))

    #make dataset for entire season (first egg and arrival)
    sea = copy(ind)

    sea = unique(sea)
    sea[, z_hatchDate := scale(yday(hatchDate))]

    return(list(ind_br, sea))


  }

  #create feeding visits
  create_feeding_visits = function(hooray, N_bt = 3, N_season = 3, sea = sea, ind_br = ind_br) {
    con = dbcon()
    stage = data.table(rbind(dbq(con, "SELECT year_ as season, box, IDfemale as ID, IDmale as IDpartner, 2 as sex, date(hatchDate) as hatchDate, date(firstEgg) as firstEgg, hatched FROM BTatWESTERHOLZ.BREEDING"),
      dbq(con, "SELECT year_ as season, box, IDmale as ID, IDfemale as IDpartner, 1 as sex, date(hatchDate) as hatchDate, date(firstEgg) as firstEgg, hatched FROM BTatWESTERHOLZ.BREEDING")))
    #add transponder
    tr = data.table(rbind(dbq(con, "SELECT ID, transponder as transp FROM BTatWESTERHOLZ.ADULTS where transponder is not NULL"), dbq(con, "SELECT ID, transponder as transp FROM BTatWESTERHOLZ.CHICKS where transponder is not NULL")))
    closeCon(con)
    stage = merge(stage, tr)

    #subset to include only the first attempt
    stage[, firstAttempt := min(firstEgg) == firstEgg, by = .(season, ID)]
    stage = stage[firstAttempt == TRUE, ]
    stage[, firstAttempt := NULL]


    #define from and to range for selecting the late nestling stage: 10 - 18 days after hatching
    stage[, start := yday(hatchDate) + 11]
    stage[, end := yday(hatchDate) + 17]
    stage = unique(stage)
    #attach to hooray for subsetting
    feeds = copy(hooray)
    feeds[, season := year(out_)]
    feeds = merge(feeds, stage, by = c("transp", "season", "box"))
    feeds = feeds[yday(out_) >= start & yday(out_) <= end,]
    #remove visits that go overnight
    feeds = subset(feeds, yday(in_) == yday(out_))
    feeds[, date_ := as.IDate(in_)]

    #use direction assignment
    feeds[direction == "IN" | direction == "IN-OUT", firstFeed := min(in_, na.rm = TRUE), by = .(transp, date_)]
    feeds[direction == "OUT" | direction == "IN-OUT", lastFeed := max(out_, na.rm = TRUE), by = .(transp, date_)]
    feeds = unique(subset(feeds, select = c("season", "ID", "date_", "firstFeed", "lastFeed", "hatchDate", "sex", "IDpartner", "firstEgg", "hatched")))


    #add age
    con = dbcon()
    feeds[, year_ := season]
    feeds =   addIndVar(con, feeds)
    #warnings are ok
    closeCon(con)

    feeds[, rel_day_hatch := yday(date_) - yday(hatchDate)]
    feeds[, hatchDate := NULL]
    #use only those where first and last feed exist (to make the data less messy)
    feeds = subset(feeds, !is.na(firstFeed) & !is.na(lastFeed))
    feeds[, firstFeed := as.ITime(firstFeed)]
    feeds[, lastFeed := as.ITime(lastFeed)]
    feeds[, midpointFeed_min := ((firstFeed + lastFeed)/2 - (12*60*60))/60]
    feeds[, activeFeed := (lastFeed - firstFeed)/60/60]

    #the outliers shouldn't matter given that I take the medians of several points
    #hist(as.numeric(feeds$lastFeed)/60/60)
    #hist(as.numeric(feeds$firstFeed)/60/60)
    #hist(as.numeric(feeds$midpointFeed)/60)
    #hist(as.numeric(feeds$activeFeed)/60/60)


    #add second variable
    feeds[, bt_firstFeed := as.numeric(median_n(firstFeed, N_bt)), by = date_]
    feeds[, bt_lastFeed := as.numeric(median_n(lastFeed, N_bt)), by = date_]
    feeds[, bt_time_to_firstFeed := as.numeric((firstFeed)-as.numeric(as.ITime(bt_firstFeed)))]
    feeds[, bt_time_to_lastFeed := as.numeric((lastFeed)-as.numeric(as.ITime(bt_lastFeed)))]
    feeds[, bt_time_to_firstFeed_min := as.numeric(bt_time_to_firstFeed)/60]
    feeds[, bt_time_to_lastFeed_min := as.numeric(bt_time_to_lastFeed)/60]
    feeds[, bt_midpointFeed_min := ((firstFeed + lastFeed)/2 - (12*60*60))/60]
    hist(as.numeric(feeds$bt_time_to_lastFeed_min))
    hist(as.numeric(feeds$bt_time_to_firstFeed_min))
    hist(as.numeric(feeds$bt_midpointFeed_min))
    hist(as.numeric(feeds$activeFeed))

    #the outliers shouldn't matter given that I take the medians of several points, so I'm just removing the very strange values over +/- 2 hours
    hist(as.numeric(feeds$bt_time_to_lastFeed_min))
    hist(as.numeric(feeds$bt_time_to_firstFeed_min))

    feeds = subset(feeds, abs(bt_time_to_firstFeed_min) < 120)
    feeds = subset(feeds, abs(bt_time_to_lastFeed_min) < 120)
    #hist(as.numeric(feeds$bt_time_to_lastFeed_min))
    #hist(as.numeric(feeds$bt_time_to_firstFeed_min))
    #hist(as.numeric(feeds$bt_midpointFeed_min))
    #hist(as.numeric(feeds$activeFeed))

    #use only those where first and last feed exist (to make the data less messy)
    feeds = subset(feeds, !is.na(bt_firstFeed) & !is.na(bt_lastFeed))

    feeds[, group := paste(sex, rel_day_hatch, season)]
    feeds[, use := .N, by = group]
    table(feeds$use, feeds$sex)
    feeds = subset(feeds, use >= N_season)
    feeds[, br := "1_br"]

    feeds[, tmp := scale3(bt_time_to_firstFeed_min), by = group]
    feeds[, med_bt_time_to_firstFeed_min := median(tmp, na.rm = TRUE), by = list(ID, season, br)]

    feeds[, tmp := scale3(bt_time_to_lastFeed_min), by = group]
    feeds[, med_bt_time_to_lastFeed_min := median(tmp, na.rm = TRUE), by = list(ID, season, br)]

    feeds[, tmp := scale3(bt_midpointFeed_min), by = group]
    feeds[, med_bt_midpointFeed_min := median(tmp, na.rm = TRUE), by = list(ID, season, br)]


    feeds[, tmp := scale3(activeFeed), by = group]
    feeds[, med_activeFeed := median(tmp, na.rm = TRUE), by = list(ID, season, br)]


    feeds[!is.na(activeFeed), Nbr_Feed := .N, by = list(ID, season, br)]
    feeds[, Nbr_Feed := first(na.omit(Nbr_Feed)), by = list(ID, season, br)]


    jpeg(filename = paste0("Figures/", Sys.time(), "_Feeding and hatchling age.jpeg"))
    par(mfrow = c(2,1))
    par(mar = c(3.1, 4.5, 0.1, 0.1))
    par(las = 1)
    boxplot(bt_firstFeed/60/60 ~ rel_day_hatch, data = feeds, outline = FALSE, ylab = "First feed")
    boxplot(bt_lastFeed/60/60 ~ rel_day_hatch, data = feeds, outline = FALSE, ylab = "Last feed")
    dev.off()

    jpeg(filename = paste0("Figures/", Sys.time(), "_Feeding and hatchling number.jpeg"))
    par(mfrow = c(2,2))
    par(mar = c(3.1, 4.5, 0.1, 0.1))
    par(las = 1)
    boxplot(I(bt_firstFeed/60/60) ~ hatched, data = feeds, outline = FALSE, ylab = "First feed"); abline(v = 5.5)
    boxplot(I(bt_lastFeed/60/60) ~ hatched, data = feeds, outline = FALSE, ylab = "Last feed"); abline(v = 5.5)
    boxplot(I(bt_midpointFeed_min/60) ~ hatched, data = feeds, outline = FALSE, ylab = "Midpoint feeds"); abline(v = 5.5)
    boxplot(I(activeFeed/60/60) ~ hatched, data = feeds, outline = FALSE, ylab = "Activity feeds"); abline(v = 5.5)
    dev.off()


    feeds[, sex := as.factor(sex)]
    feeds[, age := as.factor(age)]
    feeds[, firstEgg := as.IDate(firstEgg)]
    feeds = unique(subset(feeds, select = c("season", "ID", "med_bt_time_to_firstFeed_min", "med_bt_time_to_lastFeed_min", "med_bt_midpointFeed_min", "med_activeFeed", "Nbr_Feed", "IDpartner", "sex", "age", "minAge", "firstEgg", "hatched")))

    #fetch birth ###RESET
    con = dbcon()
    ha = dbq(con, "SELECT ID, year_, box FROM BTatWESTERHOLZ.CHICKS")
    br = dbq(con, "SELECT box, year_, date(hatchDate) as birth, date(firstEgg) as birthEgg FROM BTatWESTERHOLZ.BREEDING")
    closeCon(con)
    ha = merge(br, ha, by = c("box", "year_"))
    ha[, boxBirth := paste(box, year_, sep = '_')]
    ha = subset(ha, select = c("ID", "birth", "birthEgg", "boxBirth"))
    feeds = merge(feeds, ha, by = "ID", all.x =TRUE)


    #make wide dataset
    #winter columns come from ind_br
    d1 = subset(ind_br, br == "0_nb")
    d1[, Nbr := NULL]
    #spring columns come from feeds
    d2 = feeds
    d2[, birth := yday(birth)]###RESET
    d2[, birthEgg := yday(birthEgg)]###RESET

    #female incubation comes from ind_br
    d3 = subset(ind_br, sex == 2 & br == "1_br")
    d3[, Nall := NULL]
#RESET
    #d12 = merge(d1, d2, by = c("ID", "season", "IDpartner", "firstEgg", "age", "minAge", "sex"), all = TRUE)
    #wd = merge(d12, d3, by = c("ID", "season", "IDpartner", "firstEgg", "age", "minAge", "sex"), all = TRUE, suffixes = c("", ".inc"))

    d12 = merge(d1, d2, by = c("ID", "season", "IDpartner", "firstEgg", "age", "minAge", "sex", "birth", "birthEgg", "boxBirth"), all = TRUE)
    wd = merge(d12, d3, by = c("ID", "season", "IDpartner", "firstEgg", "age", "minAge", "sex", "birth", "birthEgg", "boxBirth"), all = TRUE, suffixes = c("", ".inc"))

    #remove very late nests here, based on ALL BROODS! (not just the ones we have data on)
    con = dbcon()
    fe = dbq(con, "SELECT year_ as season, date(firstEgg) as firstEgg FROM BTatWESTERHOLZ.BREEDING where firstEgg is not NULL and year_ is not NULL")
    closeCon(con)
    fe[, out_firstEgg := boxplot(yday(firstEgg), plot = FALSE)$stats[5], by = season]
    fe[, firstEgg := NULL]
    fe = unique(fe)
    wd = merge(wd, fe, all.x = FALSE, all.y = FALSE, by = "season")
    wd[yday(firstEgg) > out_firstEgg, ":=" ( #DO remove late broods
    #wd[yday(firstEgg) > (out_firstEgg+500), ":=" ( #DO NOT remove late broods
        med_bt_time_to_firstFeed_min = NA,
      Nbr_firstFeed = NA,
      med_bt_time_to_lastFeed_min = NA,
      Nbr_lastFeed = NA,
      med_bt_midpointFeed_min = NA,
      Nbr_midpointFeed = NA,
      med_activeFeed = NA,
      Nbr_activeFeed = NA,
      br.inc = NA,
      arrival.inc = NA,
      arrivalArea.inc = NA,
      med_time_to_sunrise_min.inc = NA,
      med_bt_time_to_sunrise_min.inc = NA,
      med_time_to_sunset_min.inc = NA,
      med_bt_time_to_sunset_min.inc = NA,
      med_bt_midpoint.inc = NA,
      med_active.inc = NA,
      hatchDate.inc = NA,
      Nbr.inc = NA,
      Nall.inc = NA,
      z_hatchDate.inc = NA,
      firstEgg = NA
    )]

    #remove nests with less than 6 hatchlings, because the feeding behaviour may not be representative, see figure above.
    #check how many lines are removed when reducing broods where only few young hatched
    nrow(subset(wd, hatched < 6))/nrow(wd) #0.054
    nrow(subset(wd, hatched < 6)) #134
    nrow(wd) #2478
    wd = subset(wd, hatched < 6)
    wd[, hatched := NULL]

    #scale first egg and arrival
    #scale within season, but take into account that individual data points were measured multiple times!
    tmp = unique(subset(wd, select = c('arrival', 'season', 'ID', 'sex')))[, z_arrival := scale(arrival), by = .(season, sex)]
    wd = merge(wd, tmp, by = c('arrival', 'season', 'ID', 'sex'), all.x = TRUE)
    tmp = unique(subset(wd, select = c('arrivalArea', 'season', 'ID', 'sex')))[, z_arrivalArea := scale(arrivalArea), by = .(season, sex)]
    wd = merge(wd, tmp, by = c('arrivalArea', 'season', 'ID', 'sex'), all.x = TRUE)
    tmp = unique(subset(wd, select = c('firstEgg', 'season', 'ID', 'sex')))[, z_firstEgg := scale(yday(firstEgg)), by = .(season)]
    wd = merge(wd, tmp, by = c('firstEgg', 'season', 'ID', 'sex'), all.x = TRUE)

    save(wd, file = paste0("Data/", Sys.Date(), "_N", N_bt, "_wide dataset.RData"))
    return(wd)

  }
}

