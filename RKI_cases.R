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

# Get the csv file from here https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0/data
# Its updated everyday. Columns are as descibred as above
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

#Lets count data from 2020-01-01 to 2021-01-01
date_lvls = seq.Date(as.Date("2020/01/01"), as.Date("2021/01/01"), 1)
rki[,reporting_date := factor(reporting_date, levels = as.character.Date(date_lvls), ordered = TRUE)]

#Counts by federal states
#by_state = data.table::as.data.table(table(rki$reporting_date, rki$BundeslandCode))
by_state = rki[,sum(AnzahlFall), .(reporting_date, BundeslandCode)]
by_state[,reporting_date := factor(reporting_date, levels = as.character.Date(date_lvls), ordered = TRUE)]
colnames(by_state) = c("reporting_date", "BundeslandCode", "ncases")
by_state = data.table::dcast(data = by_state, reporting_date ~ BundeslandCode, fill = 0, drop = FALSE, value.var = 'ncases')
data.table::setDF(x = by_state, as.character(by_state$reporting_date))
by_state$reporting_date = NULL

#Counts by age groups
age_group_cols = c("#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#08519C", "#08306B", "gray70")
male_by_age = rki[Geschlecht %in% "M",sum(AnzahlFall), .(reporting_date, Altersgruppe)]
colnames(male_by_age) = c("reporting_date", "Age_group", "ncases")
male_by_age[,reporting_date := factor(reporting_date, levels = as.character.Date(date_lvls), ordered = TRUE)]
male_by_age = data.table::dcast(male_by_age, reporting_date ~ Age_group, fill = 0, drop = FALSE, value.var = 'ncases')
data.table::setDF(x = male_by_age, rownames = as.character(male_by_age$reporting_date))
male_by_age$reporting_date = NULL

female_by_age = rki[Geschlecht %in% "W",sum(AnzahlFall), .(reporting_date, Altersgruppe)]
colnames(female_by_age) = c("reporting_date", "Age_group", "ncases")
female_by_age[,reporting_date := factor(reporting_date, levels = as.character.Date(date_lvls), ordered = TRUE)]
female_by_age = data.table::dcast(female_by_age, reporting_date ~ Age_group, fill = 0, drop = FALSE, value.var = 'ncases')
data.table::setDF(x = female_by_age, rownames = as.character(female_by_age$reporting_date))
female_by_age$reporting_date = NULL

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

dir.create(path = "./pngs_bystate", showWarnings = FALSE, recursive = TRUE)

by_state = by_state[1:358,] #WARNING: remove this line later! only to remove bottom lines with no data

#For every row (represents a day in the year) draw plot and save it
temp = lapply(1:nrow(by_state), function(idx){
  data = by_state[1:idx,, drop = FALSE]
  png(filename = paste0("./pngs_bystate/", rownames(data)[idx], ".png"), width = 8, height = 5.5, units = "in", bg = "white", res = 100)
  lo = layout(mat = matrix(data = c(1, 1, 2, 2, 3, 4, 5, 6), nrow = 4, byrow = TRUE), widths = c(3, 1), heights = c(1, 1, 3, 5))
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
  
  age_data_m = male_by_age[1:idx,, drop = FALSE]
  age_data_f = female_by_age[1:idx,, drop = FALSE]
  age_data = rbind(colSums(age_data_m), colSums(age_data_f))
  par(mar = c(2, 2, 2, 0)+0.1)
  b = barplot(age_data, col = c("#0984e3", "#74b9ff"), axes = FALSE, names.arg = rep(NA, ncol(age_data)), border = NA, col.axis = "#34495e")
  text(x = b, y = 0, labels = c("0-4", "5-14", "15-34", "35-59", "60-79", "80+", "NA"), srt = 0, 
       adj = 0, xpd = TRUE, pos = 1, srt = 45, offset = 1, col = "#34495e", col.axis = "#34495e")
  #axis(side = 2, at = range(c(0,max(colSums(age_data))), n = 3), las = 2, tick = FALSE, line = -1)
  axis(side = 2, at = pretty(c(0,max(colSums(age_data)))), las = 2, tick = FALSE, line = -1, col = "#34495e", col.axis = "#34495e")
  title(main = "Age group", font.main = 1, adj = 0)
  legend(x = "topright", legend = c("M", "F"), fill = c("#0984e3", "#74b9ff"), bty = "n",border = NA)
  
  par(mar = c(5, 4, 2, 2)+0.1)
  plot(NA, xlim = c(0, nrow(data)), ylim = c(0, max(colSums(data))+10), frame.plot = FALSE, xlab = NA, ylab = NA, axes = FALSE)
  
  tmp = lapply(1:ncol(data), function(colidx){
    points(1:nrow(data), cumsum(data[,colidx]), type = 'l', col = bundesland_colors[colidx])
    text(x = idx, y = max(cumsum(data[,colidx])), labels = colnames(data)[colidx], pos = 4, xpd = TRUE, col = bundesland_colors[colidx])
  })
  
  axis(side = 1, at = xaxt, labels = names(xaxt), las = 2, col = "#34495e", col.axis = "#34495e")
  axis(side = 2, at = pretty(c(0, max(colSums(data)))), las = 2, col = "#34495e", col.axis = "#34495e", lwd = 0)
  title(main = "cases cumulative [Bundesländer]", adj = 0, font.main = 1, line = 0.5)
  
  par(mar = c(5, 2, 2, 2)+0.1)
  if(sum(colSums(x = data)) > 0){
    barplot(colSums(data)/de_pop[names(data)], col = bundesland_colors[names(data)], horiz = TRUE, las = 2, border = NA, col.axis = "#34495e")
  }else{
    barplot(colSums(data)/de_pop[names(data)], col = bundesland_colors[names(data)], horiz = TRUE, las = 2, xlim = c(0, 0.01), border = NA, col.axis = "#34495e")
  }
  title(main = "cases/100K", font.main = 1, adj = 0, line = 0.5)
  
  death_rate = sum(colSums(deaths[1:idx,]))
  if(death_rate > 0.1){
    death_rate = round(sum(colSums(deaths[1:idx,]))/(sum(colSums(data)))*100, 2)
    death_rate_lvl = as.character(cut(x = death_rate, breaks = seq(0, 5.25, 0.25), labels = levels(death_rate_lvls)))
    par(fig = c(0.05,0.25, 0.37, 0.42), new = TRUE, mar = c(0, 2, 1, 0))
    b = barplot(height = death_rate, xlim = c(0, 6.5), axes = FALSE, col = death_rate_colors[death_rate_lvl], border = NA, horiz = TRUE, col.axis = "#34495e")
    text(x = death_rate, y = b, labels = paste0(death_rate, "%"), pos = 4, xpd = TRUE)
    axis(side = 1, at = seq(0, 5, 1), line = -1, tick = FALSE, col = "#34495e", col.axis = "#34495e")
    title(main = "Mortality rate", adj = 0, font.main = 3, line = 0.3)  
  }
  
  dev.off()
})

system(command = "convert -loop 0 -delay 3 pngs_bystate/*.png RKI_covid19.gif")

