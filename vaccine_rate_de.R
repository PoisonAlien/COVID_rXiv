#!/usr/bin/env Rscript


# Source code: https://github.com/PoisonAlien/COVID_rXiv
#
# MIT License
# Copyright (c) 2020 Anand Mayakonda <anandmt3@gmail.com>
#
# Code for visualizing COVID19 vaccination in Germany. Data from: https://impfdashboard.de


library(data.table)
library(plotrix)

#Get the "Time series of nationwide vaccinations (version 2)" from https://impfdashboard.de/daten
vac = data.table::fread(input = "https://impfdashboard.de/static/data/germany_vaccinations_timeseries_v2.tsv")

#Year and month
vac$ym = substr(x = vac$date, start = 1, stop = 7)
vac[,ym := as.factor(as.character(ym))]


#BundesLand 2019 population estimates (https://www.citypopulation.de/en/germany/cities/); it matches the ones from RKI dashboard
de_pop_raw = c(SH = 2903773, HH = 1847253, NI = 7993608, HB = 681202, NW = 17947221, 
               HE = 6288080, RP = 4093903, BW = 11100394, BY = 13124737, SL = 986887, 
               BE = 3669491, BB = 2521893, MV = 1608138, SN = 4071971, ST = 2194782, 
               TH = 2133378)

tot_pop = sum(de_pop_raw)

vac_company = vac[,.(dosen_johnson_kumulativ, dosen_biontech_kumulativ, dosen_moderna_kumulativ, dosen_astra_kumulativ)]

vac = vac[,.(ym, date, dosen_erst_kumulativ, dosen_zweit_kumulativ)]
vac[, date := as.Date(date)]

vac[, percent_single_dose := dosen_erst_kumulativ/tot_pop]
vac[, percent_double_dose := dosen_zweit_kumulativ/tot_pop]

#Daily single, double, and total doses administered
vac$single_dose_administered = c(0,diff(vac$dosen_erst_kumulativ))
vac$double_dose_administered = c(0,diff(vac$dosen_zweit_kumulativ))
vac[, total_dose_administered := single_dose_administered + double_dose_administered]

vac[,single_dose_administered := single_dose_administered/1000] #in 1000s
vac[,double_dose_administered := double_dose_administered/1000] #in 1000s


max_vac = max(vac[,percent_single_dose])

dir.create(path = "./tmp_pngs/", showWarnings = FALSE)

for(row_idx in 1:nrow(vac)){
  png(filename = paste0("./tmp_pngs/", vac[row_idx, date], ".png"), width = 600, height = 400, units = "px", bg = "white")
  
  lo = layout(mat = matrix(data = c(1, 1, 2, 3, 2, 4), nrow = 3, byrow = T), heights = c(1, 1, 3))
  par(mar = c(2, 1, 2, 1), bg = "#ffffff", family = "mono")
  
  par(mar = c(1, 1, 1, 2)+0.1)
  plot(x = NA, ylim = c(0, 1), xlim = c(0, nrow(vac)), axes = FALSE, frame.plot = FALSE, xlab = NA, ylab = NA)
  berryFunctions::roundedRect(xleft = 0, ybottom = 0, xright = nrow(vac), ytop = 0.5, col = "#f1f2f6", rounding = 0.2, border = FALSE)
  berryFunctions::roundedRect(xleft = 0, ybottom = 0, xright = row_idx, ytop = 0.5, col = "#a4b0be", rounding = 0.2, border = FALSE)
  segments(x0 = cumsum(x = vac[,.N,ym][,N]), y0 = 0, x1 = cumsum(x = vac[,.N,ym][,N]), y1 = 0.5)
  axis(side = 3, at = cumsum(vac[,.N,ym][,N]), labels = vac[,.N,ym][,ym], las = 1, col = "#34495e", col.axis = "#34495e", line = -3, cex.axis = 1.25, tick = FALSE)
  title(main = "Timeline", font.main = 1, cex.main = 1.6)
  
  single_dose_r = vac[row_idx, percent_single_dose] * 2.5 #2.5 is 100%
  double_dose_r = vac[row_idx, percent_double_dose] * 2.5 
  
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
  
  legend(x = 1, y = 2, legend = c(paste0("First: ", sd_mil), paste0("Second: ", dd_mil)), pch = 15, 
         col = c("#d35400", "#c0392b"), cex = 1.4, bty = "n", title = "Doses administered")
  
  title(main = "Germanys'\nvaccination drive", font.main = 4, adj = 0.5, line = -2, xpd = TRUE, cex.main = 1.6)
  title(main = paste0("%vaccinated*: ", sd_pct, "%"), font.main = 1, adj = 0, line = -7, cex.main = 1.6)
  text(x = -0.2, y = -.5, labels = "*At-least one dose", col = "black", adj = 0, cex = 1.2, xpd = TRUE)
  
  vac_prop = unlist(vac_company[row_idx])
  
  par(mar = c(0, 1, 2, 1), bg = "#ffffff", family = "mono")
  barplot(as.matrix(vac_prop/sum(vac_prop)), beside = FALSE, col = c("#9b59b6", "#16a085", "#f1c40f", "#f39c12"), horiz = TRUE, ylim= c(0, 2))
  legend(
    x = 0,
    y = 2,
    legend = c("J&J", "Pfizer", "Moderna", "AZ"),
    bty = "n",
    ncol = 4,
    pch = 15, cex = 1.6,
    col = c("#9b59b6", "#16a085", "#f1c40f", "#f39c12"),
    xpd = TRUE
  )
  title("Vaccine manufacturer", cex.main = 1.6, adj = 0.1, line = 0.75)
  
  if(nrow(vac) > 1){
    par(mar = c(5, 2, 5, 1), bg = "#ffffff", family = "mono")
    vac_rate  = data.table::melt(vac[1:row_idx, .(ym, single_dose_administered, double_dose_administered)])
    vac_rate$variable = gsub(pattern = "_dose_administered", replacement = "", x = vac_rate$variable)
    x = data.table::dcast(vac_rate[,sum(value), .(variable, ym)], formula =  ym ~ variable, value.var = "V1", drop = FALSE, fill = 0)
    data.table::setDF(x = x, rownames = as.character(x$ym))
    x$ym = NULL
    x = x[,c("single", "double")]
    
    barplot(height = t(x), col = c("#d35400", "#c0392b"), xlab = NA, ylab = NA, las = 2)
    mtext(text = "x1000", side = 2, line = 3.2)
    title("Vaccination rate", cex.main = 1.6, adj = 0.1, line = 1)
    legend(x = "topright", legend = c("First", "second"), col = c("#d35400", "#c0392b"), pch = 15, bty = "n", ncol = 1)

  }
  
  
  dev.off()
}

#Convert to gif
system(command = "convert -loop 0 -delay 5 tmp_pngs/*.png DE_vaccination.gif")
