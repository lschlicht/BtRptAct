niceNames = function(x) {
  #x = unique(c(MM2$resp, MM2$expl))
  x = gsub("z_", "", x)
  x = gsub("bt_", "", x)
  x = gsub("med_", "", x)
  x = gsub("time_to_", "", x)
  x = gsub("_min", "", x)
  x = gsub("sunrise", "start", x)
  x = gsub("sunset", "end", x)
  x = gsub("useA", "a", x)
  x
}


niceNamesFigures = function(x) {
  x[which(x == "z_firstEgg")] = "1st egg"
  x[which(x == "z_useArrival")] = "Arrival"
  x[which(x == "med_bt_time_to_sunrise_min")] = "Act. start"
  x[which(x == "med_bt_time_to_sunset_min")] = "Act. end"
  x[which(x == "med_bt_midpoint")] = "Act. mid."
  x[which(x == "med_active")] = "Total act."
  x[which(x == "med_bt_time_to_firstFeed_min")] = "Prov. start"
  x[which(x == "med_bt_time_to_lastFeed_min")] = "Prov. end"
  x[which(x == "med_bt_midpointFeed_min")] = "Prov. mid."
  x[which(x == "med_activeFeed")] = "Total prov."
  return(x)
}
