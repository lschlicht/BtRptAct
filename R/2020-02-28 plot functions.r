plot_1 = function(data, OFFSET = 0.05, labels = "variable", ...) {
  copy(data) -> plot_data
  setnames(plot_data, names(plot_data)[2], "SE")
  plot_data[, Estimate := as.numeric(Estimate)]
  plot_data[, SE := as.numeric(SE)]
  if(labels[1] == "variable") { plot_data[, labels := "variable"] }  else plot_data[, labels := labels]
  plot_data[, up := Estimate + 1.96*SE]
  plot_data[, low := Estimate - 1.96*SE]
  plot_data[sex == 2, ":=" (offset = -OFFSET, COL = FEMALE_COLOUR)]
  plot_data[sex == 1, ":=" (offset = OFFSET, COL = MALE_COLOUR)]
  plot_data[, YY := ceiling((1:nrow(plot_data))/2)]

  par(mar = c(4.6, 11.6, 0.1, 0.1))
  par(las = 1)
  plot(c(min(plot_data[, low]),max(plot_data[, up])), c(0.5, (max(plot_data[, YY]) + 0.5)), type = 'n', ylab = '', xlab = "Estimate±SE", yaxt = 'n', ...)
  points(plot_data[, Estimate], plot_data[, YY+offset], pch= 16, col = plot_data[, COL])
  arrows(plot_data[, low], plot_data[, YY+offset], plot_data[, up], plot_data[, YY+offset], code = 3, angle = 90, length = 0.05, col = plot_data[, COL])
  axis(2, at = plot_data[sex == 1, YY], labels = plot_data[sex == 1, labels])
  abline(v = 0, lty = 3)
}

###

plot_2 = function(dat, XLAB = "Estimate±CI", POSITION = "bottomleft") {
  copy(dat) -> data
  #prepare data
  setorder(data, XX, br, sex)
  data[!(Name %in% c("First egg", "Arrival")), XX := XX + ((-1)*(as.numeric(factor(br, levels = c("all", "0_nb", "1_br")))-6)/8 - 0.25)]
  data[, XX := XX + ( (as.numeric(as.character(sex)) - 1.5)/30)]
  data[, COL := ifelse(sex == 1, MALE_COLOUR, FEMALE_COLOUR)]
  data[, CIup := Est + 1.96*SE]
  data[, CIlow := Est - 1.96*SE]
  data[br == "all", LTY := 1]
  data[br == "0_nb", LTY := 2]
  data[br == "1_br", LTY := 3]


  #plot
  par(mar = c(4.6, 11.6, 0.1, 0.1))
  par(las = 1)
  plot(c(0.5,length(data[, unique(Name)])+0.5)~ c(0, 0.8), type = 'n', xlab = XLAB, yaxt = 'n', ylab = "", xlim = c(min(data[, CIlow]), max(data[, CIup])))
  points(data[,Est], data[,XX], col = data[, COL], pch = 16)
  arrows(data[,CIlow], data[,XX], data[,CIup], data[, XX], code = 3, angle = 90, length = 0.05, col = data[, COL], lty = data[, LTY])
  abline(v = 0, lty = 3)
  tmp = subset(data, select = c('XX', 'Name'))
  tmp[, XX := round(XX)]
  tmp = unique(tmp)
  axis(2, at = tmp$XX, labels = tmp$Name)
  legend(POSITION, legend = c("all", "non-breeding", "breeding", "", "male", "female"), lty = c(1:3, 1, 1, 1), col = c(1, 1, 1, "white", MALE_COLOUR, FEMALE_COLOUR), bty = "n")
}
