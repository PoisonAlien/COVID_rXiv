#!/usr/bin/env Rscript
# Source code: https://github.com/PoisonAlien/COVID_rXiv
#
# MIT License
# Copyright (c) 2020 Anand Mayakonda <anandmt3@gmail.com>
#
# Code for current number of COVID19 cases from the Robert Koch Institute.
#
# Description of the data of the RKI Covid-19 (https://www.arcgis.com/home/item.html?id=dd4580c810204019a7b8eb3e0b329dd6)
# The data source contains the following parameters:
#   IdBundesland: Id of the federal state of the case with 1 = Schleswig-Holstein to 16 = Thuringia
#   State: Name of the federal state
#   District ID: ID of the district of the case in the usual coding 1001 to 16077 = LK Altenburger Land
#   District: Name of the district
#   Age group: Age group of the case from the 6 group 0-4, 5-14, 15-34, 35-59, 60-79, 80+ and unknown
#   Age group2: Age group of the case from 5-year groups 0-4, 5-9, 10-14, ..., 75-79, 80+ and unknown
#   Gender: Gender of the case M0 male, W = female and unknown
#   NumberCases: Number of cases in the corresponding group
#   Number of deaths: Number of deaths in the corresponding group
#   Reporting date: the date when the health department became aware of the case
#   Data status: Date when the data record was last updated
#   New case: 
#     0: Case is included in the publication for the current day and in the one for the previous day
#   1: Case is only included in the current publication
#   -1: Case is only included in the previous day's publication
# this results in: number of cases in the current publication as a sum (number of cases), if new case in (0.1); Delta on the previous day as a sum (number of cases) if new case in (-1.1)
# New death:
# 0: The case is one death in the publication for the current day and in the one for the previous day
# 1: The case is a death in the current publication, but not in the publication of the previous day
# -1: The case is not a death in the current publication, but it was a death in the previous day's publication
#   -9: The case is not a death in either the current publication or that of the previous day
#   this results in: number of deaths in the current publication as a total (number of deaths) if new death in (0.1); Delta on the previous day as total (number of deaths) if new deaths in (-1.1)
#   Reference date: date of illness or, if this is not known, the reporting date
#   Number of recovered: Number of recovered in the corresponding group
#   NewGenesen:
#     0: The case is recovery in the publication for the current day and in the one for the previous day
#   1: Case is recovered in the current publication, but not in the previous day's publication
# -1: Case is not recovered in the current publication, but was recovered in the previous day's publication
#   -9: The case is neither in the current publication nor in that of the previous day recovered 
#   this results in: number of geneses of the current publication as the sum (number of genes) if new genes in (0.1); Delta to the previous day as sum (number of generations) if new generation in (-1.1)
#   Is onset of illness: 1 if the reference date is the onset of illness, 0 otherwise


library(data.table)
library(plotrix)

# COVID19 cases CSV file, maintained by RKI: https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0/data
# Its updated everyday. Columns are as described as above
#download.file(url = "https://opendata.arcgis.com/api/v3/datasets/dd4580c810204019a7b8eb3e0b329dd6_0/downloads/data?format=csv&spatialRefId=4326", destfile = "RKI_COVID19.csv")
rki = data.table::fread(file = "RKI_COVID19.csv", sep = ",")
rki[, reporting_date := as.Date(substr(x = Meldedatum, start = 1, stop = 10))]

#Add BundesLand ISO code for stats for ease of use https://www.iso.org/obp/ui/#iso:code:3166:DE
bundes_codes = rki[,.N,.(IdBundesland, Bundesland)][order(IdBundesland)]
bundes_codes[, BundeslandCode := c("SH", "HH", "NI", "HB", "NW", "HE", "RP", "BW", "BY", "SL", "BE", "BB", "MV", "SN", "ST", "TH")]
bundesland_codes = bundes_codes$BundeslandCode
names(bundesland_codes) = bundes_codes$IdBundesland
rki[, BundeslandCode := bundesland_codes[IdBundesland]]
rm(bundes_codes)

bundesland_colors = c("#A6CEE3FF", "#1F78B4FF", "#B2DF8AFF", "#33A02CFF", "#FB9A99FF", 
                      "#E31A1CFF", "#FDBF6FFF", "#FF7F00FF", "#CAB2D6FF", "#6A3D9AFF", 
                      "#FFFF99FF", "#9E0142FF", "#D53E4FFF", "#F46D43FF", "#000000FF", 
                      "#EE82EEFF")
names(bundesland_colors) = bundesland_codes

#BundesLand 2019 population estimates (https://www.citypopulation.de/en/germany/cities/); it matches the ones from RKI dashboard
de_pop_raw = c(SH = 2903773, HH = 1847253, NI = 7993608, HB = 681202, NW = 17947221, 
               HE = 6288080, RP = 4093903, BW = 11100394, BY = 13124737, SL = 986887, 
               BE = 3669491, BB = 2521893, MV = 1608138, SN = 4071971, ST = 2194782, 
               TH = 2133378)
de_pop = de_pop_raw/100000 #In 100,000

date_lvls = seq.Date(range(rki[,reporting_date])[1], range(rki[,reporting_date])[2], 1)
rki[,reporting_date := factor(reporting_date, levels = as.character.Date(date_lvls), ordered = TRUE)]

#Counts by federal states
#by_state = data.table::as.data.table(table(rki$reporting_date, rki$BundeslandCode))
by_state = rki[,sum(AnzahlFall), .(reporting_date, BundeslandCode)]
by_state[,reporting_date := factor(reporting_date, levels = as.character.Date(date_lvls), ordered = TRUE)]
colnames(by_state) = c("reporting_date", "BundeslandCode", "ncases")
by_state = data.table::dcast(data = by_state, reporting_date ~ BundeslandCode, fill = 0, drop = FALSE, value.var = 'ncases')
data.table::setDF(x = by_state, as.character(by_state$reporting_date))
by_state$reporting_date = NULL

#Death rates
deaths = rki[,sum(AnzahlTodesfall), .(reporting_date, BundeslandCode)]
deaths[,reporting_date := factor(reporting_date, levels = as.character.Date(date_lvls), ordered = TRUE)]
colnames(deaths) = c("reporting_date", "BundeslandCode", "ncases")
deaths = data.table::dcast(data = deaths, reporting_date ~ BundeslandCode, fill = 0, drop = FALSE, value.var = 'ncases')
data.table::setDF(x = deaths, as.character(deaths$reporting_date))
deaths$reporting_date = NULL

death_rate_colors = c("#FFFFCC", "#FFEDA0", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", 
                      "#E31A1C", "#BD0026", "#800026")
death_rate_lvls = cut(x = seq(0, 5.25, 0.25), breaks = seq(0, 5.25, 0.25), labels = paste0("L", 1:21)) #Cut death rate into levels
death_rate_colors = colorRampPalette(colors = death_rate_colors)(21)
names(death_rate_colors) = levels(death_rate_lvls)

xaxt = cumsum(table(substr(x = rownames(by_state), 1, 7))) #Tick marks on x-axis
xaxt = c("2019-12" = 0, xaxt)

#Get the "Time series of nationwide vaccinations (version 2)" from https://impfdashboard.de/daten
vac = data.table::fread(input = "https://impfdashboard.de/static/data/germany_vaccinations_timeseries_v2.tsv")
vac = vac[date <= tail(date_lvls, 1)]

#Vaccination distributon by providers (moderna, bioNtech, j&j, AZ)
vac_company = vac[,.(date, dosen_johnson_kumulativ, dosen_biontech_kumulativ, dosen_moderna_kumulativ, dosen_astrazeneca_kumulativ)]
vac_company[, date := as.Date(date)]
vac_company[,date := factor(date, levels = as.character.Date(date_lvls), ordered = TRUE)]
vac_company = data.table::melt(data = vac_company, id.vars = "date") |> data.table::dcast(formula =  date~ variable, drop = FALSE, fill = 0)
data.table::setDF(x = vac_company, rownames = as.character(vac_company$date))
vac_company$date = NULL

#Cumulative first and second doses 
vac = vac[,.(date, dosen_erst_kumulativ, dosen_zweit_kumulativ)]
vac[, date := as.Date(date)]
vac[,date := factor(date, levels = as.character.Date(date_lvls), ordered = TRUE)]
vac = data.table::melt(data = vac, id.vars = "date") |> data.table::dcast(formula =  date~ variable, drop = FALSE, fill = 0)
#% vaccinated
tot_pop = sum(de_pop_raw)
vac[, percent_single_dose := dosen_erst_kumulativ/tot_pop]
vac[, percent_double_dose := dosen_zweit_kumulativ/tot_pop]
max_vac = max(vac[,percent_single_dose])
data.table::setDF(x = vac, rownames = as.character(vac$date))
vac$date = NULL

dir.create(path = "./pngs_bystate", showWarnings = FALSE, recursive = TRUE)

#by_state = by_state[1:358,] #WARNING: remove this line later! only to remove bottom lines with no data

#For every row (represents a day in the year) draw plot and save it
temp = lapply(1:nrow(by_state), function(idx){
  data = by_state[1:idx,, drop = FALSE]
  png(filename = paste0("./pngs_bystate/", rownames(data)[idx], ".png"), width = 8, height = 5.5, units = "in", bg = "white", res = 100)
  lo = layout(mat = matrix(data = c(1, 1, 2, 2, 3, 4, 5, 6), nrow = 4, byrow = TRUE), widths = c(3, 1), heights = c(1.5, 2.5, 5, 5))
  par(bg = "#ffffff", family = "mono")
  
  par(mar = c(1, 1, 1, 2)+0.1)
  plot(x = NA, ylim = c(0, 1), xlim = c(0, nrow(by_state)), axes = FALSE, frame.plot = FALSE, xlab = NA, ylab = NA)
  berryFunctions::roundedRect(xleft = 0, ybottom = 0, xright = nrow(by_state), ytop = 0.5, col = "#f1f2f6", rounding = 0.2, border = FALSE)
  berryFunctions::roundedRect(xleft = 0, ybottom = 0, xright = idx, ytop = 0.5, col = "#a4b0be", rounding = 0.2, border = FALSE)
  axis(side = 3, at = xaxt[seq(1, length(xaxt), 2)], labels = names(xaxt[seq(1, length(xaxt), 2)]), las = 1, col = "#34495e", col.axis = "#34495e", line = -2, cex.axis = 0.75, tick = FALSE)
  title(main = "Timeline", font.main = 1)
  #text(x = xaxt, y = rep(c(0.8, 1), length(xaxt)), labels = names(xaxt), xpd = TRUE, col = "#34495e")
  
  if(nrow(data) > 7){
    d7_avg = colSums(tail(data, 7))/de_pop[names(data)]
    #Cut cases per 100K into 7 categories bsed on ranges defined by RKI ((0,5] (5,25] (25,50] (50,100] (100,250] (250,500] (500,1e+03])
    d7_avg_lvl = cut(x = d7_avg, breaks = c(0, 5, 25, 50, 100, 250, 500, 1000), labels = paste0("L", 1:7))
    rki_risk_cols = c("#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#BD0026", "#800026")
    names(rki_risk_cols) = paste0("L", 1:7)
    
    par(mar = c(0, 1, 2, 2)+0.1)
    plot(NA, xlim = c(0, 16), ylim = c(0, 1), axes = FALSE, xlab = NA, ylab = NA)
    rect(xleft = 0:15, ybottom = 0.25, xright = 1:16, ytop = 0.75, col = rki_risk_cols[d7_avg_lvl], border = "white")
    text(x = seq(0.5, 15.5, 1), y = 0.95, labels = names(d7_avg), xpd = TRUE)
    text(x = seq(0.5, 15.5, 1), y = 0.5, labels = round(d7_avg), col = "black")
    title(main = "cases/100K inhabitants in the last 7 days", font.main = 1, sub = "Federal states")
  }else{
    plot.new()
  }
  
  par(mar = c(0, 4, 2, 0)+0.1)
  bardata = t(data)
  if(max(colSums(bardata)) < 10){
    plot(NA, xlim = c(0, idx), ylim = c(0, 10), axes = FALSE, frame.plot = FALSE, xlab = NA, ylab = NA)
  }else{
    plot(NA, xlim = c(0, idx), ylim = c(0, max(colSums(bardata))), axes = FALSE, frame.plot = FALSE, xlab = NA, ylab = NA)  
  }
  
  temp = lapply(1:ncol(bardata), function(i){
    x = bardata[,i]
    rect(xleft = i-1, ybottom = c(0, cumsum(x)[1:(length(x)-1)]), xright = i-0.1,
         ytop = cumsum(x), col = bundesland_colors[names(x)], border = NA, lwd = 0)
  })
  axis(side = 2, at = pretty(c(0, max(colSums(bardata)))), las = 2, col = "#34495e", col.axis = "#34495e", lwd = 0)
  title(main = "cases per day", adj = 0, font.main = 1, line = 1)
  title(main = paste0("total cases: ", sum(colSums(data)), " [", round((sum(colSums(data))/sum(de_pop_raw))*100, 2), "% of population]"), adj = 0, font.main = 1, line = 0)
  
  par(mar = c(2, 2, 2, 2)+0.1)
  if(sum(colSums(x = data)) > 0){
    barplot(colSums(data)/de_pop[names(data)], col = bundesland_colors[names(data)], horiz = TRUE, las = 2, border = NA, col.axis = "#34495e")
  }else{
    barplot(colSums(data)/de_pop[names(data)], col = bundesland_colors[names(data)], horiz = TRUE, las = 2, xlim = c(0, 0.01), border = NA, col.axis = "#34495e")
  }
  title(main = "total cases/100K", font.main = 1, adj = 0, line = 0.5)
  
  par(mar = c(5, 4, 2, 0)+0.1)
  if(max(vac[1:idx,]) > 0){
    plot(NA, xlim = c(0, idx), ylim = c(0, max(vac[1:idx,])), frame.plot = FALSE, xlab = NA, ylab = NA, axes = FALSE)  
  }else{
    plot(NA, xlim = c(0, idx), ylim = c(0, 1), frame.plot = FALSE, xlab = NA, ylab = NA, axes = FALSE)
  }
  
  points(vac[1:idx,"dosen_erst_kumulativ"], type = 'h', col = '#d35400') 
  points(vac[1:idx,"dosen_zweit_kumulativ"], type = 'h', col = '#c0392b')
  axis(side = 1, at = xaxt, labels = names(xaxt), las = 2, col = "#34495e", col.axis = "#34495e")
  axis(side = 2, at = pretty(c(0, max(vac[1:idx,]))), labels = pretty(c(0, max(vac[1:idx,])))/100000, las = 2, col = "#34495e", col.axis = "#34495e", lwd = 0)
  mtext(text = "Doses (x100K)", side = 2, line = 2.5, cex = 0.5) 
  legend(x = "center", legend = c("single", "double"), fill = c("#d35400", "#c0392b"), bty = "n", border = NA, xpd = TRUE)
  #text(x = 1, y = 0, labels = paste0("Single: ", sd_mil), col = "#d35400", adj = 0, cex = 0.75)
  #text(x = 1, y = -1, labels = paste0("Double: ", dd_mil), col = "#c0392b", adj = 0, cex = 0.75, xpd = TRUE)

  vac_prop = unlist(vac_company[idx,])
  if(sum(vac_prop) > 0){
    par(mar = c(1, 1, 1, 1)+0.1)
    pie(
      vac_prop,
      labels = unlist(data.table::tstrsplit(
        x = names(vac_prop),
        spli = "_",
        keep = 2
      )),
      col = c("#1abc9c", "#16a085", "#f1c40f", "#f39c12"),
      border = NA, cex = 0.75
    )
  }
  
  single_dose_r = vac[idx, "percent_single_dose"] * 2.5 #2.5 is 100%
  double_dose_r = vac[idx, "percent_double_dose"] * 2.5 
  par(fig = c(0.1,0.3, 0.25, 0.32), new = TRUE, mar = c(0, 2, 1, 0))
  plot(NA, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, frame.plot = FALSE, xlab = NA, ylab = NA)
  rect(
    xleft = c(0, 0),
    ybottom = 0.2,
    xright = c(1, vac[idx, "percent_single_dose"], vac[idx, "percent_double_dose"]),
    ytop = 0.8,
    col = c("gray70", "#d35400", "#c0392b"), border = c("gray70", "#d35400", "#c0392b")
  )
  axis(side = 1, at = seq(0, 1, 0.5), labels = c(0, 50, 100), line = -1, tick = FALSE, col = "#34495e", col.axis = "#34495e")
  # plotrix::draw.circle(x = 5, y = 5, radius = 2.5, col = "#bdc3c7", border = "#bdc3c7")
  # plotrix::draw.circle(x = 5, y = 5, radius = single_dose_r, col = "#d35400", border = "#d35400")
  # plotrix::draw.circle(x = 5, y = 5, radius = double_dose_r, col = "#c0392b", border = "#c0392b")
  
  title(main = paste0("%vaccinated: ", round(vac[idx, "percent_single_dose"] * 100, 2)), adj = 0, xpd = TRUE, cex.main = 0.8)
  
  dev.off()
})

system(command = "convert -loop 0 -delay 3 pngs_bystate/*.png RKI_covid19.gif")
