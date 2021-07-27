#!/usr/bin/env Rscript

library(data.table)
library(plotrix)

#Get the "Time series of nationwide vaccinations (version 2)" from https://impfdashboard.de/daten
vac = data.table::fread(input = "https://impfdashboard.de/static/data/germany_vaccinations_timeseries_v2.tsv")


#BundesLand 2019 population estimates (https://www.citypopulation.de/en/germany/cities/); it matches the ones from RKI dashboard
de_pop_raw = c(SH = 2903773, HH = 1847253, NI = 7993608, HB = 681202, NW = 17947221, 
               HE = 6288080, RP = 4093903, BW = 11100394, BY = 13124737, SL = 986887, 
               BE = 3669491, BB = 2521893, MV = 1608138, SN = 4071971, ST = 2194782, 
               TH = 2133378)

tot_pop = sum(de_pop_raw)

vac_company = vac[,.(dosen_johnson_kumulativ, dosen_biontech_kumulativ, dosen_moderna_kumulativ, dosen_biontech_zweit_kumulativ)]

vac = vac[,.(date, dosen_erst_kumulativ, dosen_zweit_kumulativ)]
vac[, date := as.Date(date)]

vac[, percent_single_dose := dosen_erst_kumulativ/tot_pop]
vac[, percent_double_dose := dosen_zweit_kumulativ/tot_pop]


max_vac = max(vac[,percent_single_dose])

dir.create(path = "./tmp_pngs/", showWarnings = FALSE)

for(row_idx in 1:nrow(vac)){
  png(filename = paste0("./tmp_pngs/", vac[row_idx, date], ".png"), width = 5, height = 5, units = "in", bg = "white", res = 100)
  
  single_dose_r = vac[row_idx, percent_single_dose] * 2.5 #2.5 is 100%
  double_dose_r = vac[row_idx, percent_double_dose] * 2.5 
  par(bg = "#ffffff", family = "mono")
  par(mar = c(2, 1, 2, 1))
  
  plot(NA, xlim = c(0, 10), ylim = c(0, 10), axes = FALSE, frame.plot = FALSE, xlab = NA, ylab = NA)
  plotrix::draw.circle(x = 5, y = 5, radius = 2.5, col = "#bdc3c7", border = "#bdc3c7")
  #Single dose
  plotrix::draw.circle(x = 5, y = 5, radius = single_dose_r, col = "#d35400", border = "#d35400")
  #Double dose
  plotrix::draw.circle(x = 5, y = 5, radius = double_dose_r, col = "#c0392b", border = "#c0392b")
  sd_mil = format(round(vac[row_idx, dosen_erst_kumulativ]), big.mark=",")
  sd_pct = round(100*vac[row_idx, dosen_erst_kumulativ]/tot_pop, 2)
  
  dd_mil = format(round(vac[row_idx, dosen_zweit_kumulativ]), big.mark=",")
  dd_pct = round(100*vac[row_idx, dosen_zweit_kumulativ]/tot_pop, 2)
  
  text(x = 1, y = 0, labels = paste0("Single: ", sd_mil), col = "#d35400", adj = 0, cex = 0.75)
  text(x = 1, y = -1, labels = paste0("Double: ", dd_mil), col = "#c0392b", adj = 0, cex = 0.75, xpd = TRUE)
  title(main = "Germany's vaccination drive", font.main = 1, adj = 0, line = 1)
  title(main = paste0(vac[row_idx, date], ": ", sd_pct, "%"), font.main = 1, adj = 0, line = 0)
  
  vac_prop = unlist(vac_company[row_idx])
  pie(
    vac_prop,
    labels = unlist(data.table::tstrsplit(
      x = names(vac_prop),
      spli = "_",
      keep = 2
    )),
    col = c("#1abc9c", "#16a085", "#f1c40f", "#f39c12"),
    border = NA
  )
  
  dev.off()
}
system(command = "convert -loop 0 -delay 3 tmp_pngs/*.png DE_vaccination.gif")


