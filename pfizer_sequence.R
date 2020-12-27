#!/usr/bin/env Rscript
# Source code: https://github.com/PoisonAlien/COVID_rXiv
#
# MIT License
# Copyright (c) 2020 Anand Mayakonda <anandmt3@gmail.com>
#
# Code for visualizing mrna11889 COVID19 vaccine sequence 
# Inspired by OG post: https://www.reddit.com/r/dataisbeautiful/comments/kkmavd/oc_biontechpfizer_vaccine_sequence_visualized/

fasta_file = "mrna11889_seq.txt"
linelen= 50
seqlen = 4284

nrows = ceiling(seqlen/linelen)

#Some random colors
#iupac_col_codes = c("#27ae60", "#e74c3c", "#8e44ad", "#3498db")
iupac_col_codes = c("#3C3B6E", "#B22234", "red", "#FFCE00")
names(iupac_col_codes) = c("A", "U", "G", "C")

isgz = summary(object = file(fasta_file))$class == "gzfile"
if(isgz){
  con = gzfile(fasta_file, "r")
}else{
  con = file(fasta_file, "r")
}

faseq = list()

while(TRUE){
  line = readLines(con, n = 1, skipNul = TRUE)
  if(length(line) == 0){
    break
  }
  faseq = c(faseq, line)
}
close.connection(con)

par(mar = c(0, 0, 2, 0), bg = "black", family = "mono") #bg = "black"
plot(NA, xlim = c(0, linelen+1), ylim = c(0, nrows), axes = TRUE, xlab = NA, ylab = NA)
random_radii = runif(n = seqlen, min = 0.1, max = 2) #seq(0, 0.2, length.out = 25)
lapply(1:length(faseq), function(fa){
  x = faseq[[fa]]
  cols = unlist(lapply(1:nchar(x), function(idx) {
    iupac_col_codes[substr(x = x, idx, idx)]
  }))
  symbols(
    x = 1:nchar(x),
    y = rep(fa, nchar(x)),
    circles = sample(
      x = random_radii,
      size = nchar(x),
      replace = TRUE
    ),
    bg = cols,
    fg = cols,
    inches = 0.01,
    add = TRUE
  )
})

title(main = "BioNTech/Pfizer mrna11889 vaccine sequence", adj = 0, col.main = "white")

