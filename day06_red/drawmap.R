
library(data.table)
library(sf)
library(reldist)

# Prepare coordinates of major towns
wpl <- fread("bagger/data/woonplaatsen.csv")
wpl <- wpl[, .(naam, x, y)]
sel <- c("Amsterdam", "Rotterdam", "'s-Gravenhage", "Utrecht", "Eindhoven", 
  "Groningen", "Zwolle", "Maastricht", "'s-Hertogenbosch", "Arnhem", 
  "Leeuwarden", "Assen", "Middelburg", "Breda", "Nijmegen", "Enschede")
# Corresponing positions of labels 
# 1 = down, 2 = left, 3 = top, 4 = right
pos <- c(4,1,3,4,4,
  4,4,4,4,4,
  3,4,4,4,4,2)
w <- wpl[naam %in% sel]
w <- w[!duplicated(naam)]
w$pos <- pos[match(w$naam, sel)]


# Read data on dwellings
dta <- fread("bagger/data/verblijfsobjecten.csv", 
  select = c(1, 2, 3, 7, 8, 10, 11))

dta[, begin_geldigheid := 
    as.Date(substr(as.character(begin_geldigheid), 1, 8), "%Y%m%d")]
dta[, eind_geldigheid := 
    as.Date(substr(as.character(eind_geldigheid), 1, 8), "%Y%m%d")]

# Remove records that are not valid on reference data
refdate <- as.Date("2021-09-08")
dta <- dta[(!is.na(begin_geldigheid) & (begin_geldigheid < refdate)) &
  (is.na(eind_geldigheid) | (eind_geldigheid > refdate))]
# We still have duplicates; remove those as well
dta <- dta[!duplicated(id, fromLast = TRUE), ]
# Missing values in oppervlakte
dta <- dta[oppervlakte < 999999]
# Remove unrealistic dwelling areas
# dta <- dta[oppervlakte >= 10 & oppervlakte < 2000]
# Truncate unrealistic dwelling areas
dta[oppervlakte > 2000, oppervlakte:= 2000]
dta[oppervlakte < 10, oppervlakte:= 10]

# Data already uses metres; create grid by rounding data
gridsize <- 5000
dta[, gridx := round(x/gridsize)*gridsize]
dta[, gridy := round(y/gridsize)*gridsize]

# The gini function from reldist is a bit picky regarding its
# input; wrapper to make sure it gets what it wants
gini <- function(x) {
  x <- as.integer(x)
  if (length(x) == 0) return(0)
  reldist::gini(x)
}

# Aggregate
opp <- dta[gebruiksdoel == "woonfunctie", .(
    opp = mean(oppervlakte), 
    oppmed = as.integer(median(oppervlakte)),
    gini = gini(oppervlakte),
    n = .N
  ), by = .(gridx, gridy)]

# Remove outliers/incorrect coordinates
opp <- opp[gridx > 100 & gridx < 3E5]
opp <- opp[!(gridx<0.2E5 & gridy>400000)]
# Filter cells with small numbers
opp <- opp[n > 50]

# Colour scale
pal <- hcl.colors(20, "Peach")
pal <- rev(pal)


# Divide median area into categories using quantiles
q1 <- quantile(opp$oppmed, probs = seq(0, 1, length.out = length(pal)+1))
q1 <- round(q1)
opp[, col := as.integer(cut(oppmed, breaks = q1, right = FALSE, 
  include.lowest = TRUE))]
# Labels for categories
lab1 <- sprintf("%d-%d m2", head(q1,-1), tail(q1, -1))
lab1[length(lab1)] <- sprintf(">%d", q1[length(q1)-1])


# Divide gini into categories
q2 <- c(0, 0.1, 0.2, 0.21, 0.22, 0.23, 0.24, 0.25, 0.26, 027, 0.28, 0.29, 0.30,
  0.31, 0.32, 0.33, 0.34, 0.35, 0.40, 0.60, 1)
#q2 <- quantile(opp$gini, probs = seq(0, 1, length.out = length(pal)+1))
opp[, colgini := as.integer(cut(gini, breaks = q2, right = FALSE, 
  include.lowest = TRUE))]
lab2 <- sprintf("%4.2f-%4.2f", head(q2,-1), tail(q2, -1))


# =============================================================================
# Start plot
png("dwelling_area.png", width = 10, height = 7, res = 300, units = "in")
par(mar = c(0, 0, 0, 0), oma = c(0 ,0 ,0 ,0)+0.1, mfrow = c(1,2), 
  bg = "#EEEEEE")

# Median area 
plot(opp$gridx, opp$gridy, type = 'n', asp = 1, xaxt = "n", yaxt = "n", 
  bty = "n", xlab="", ylab="")
rect(opp$gridx, opp$gridy, opp$gridx+gridsize, opp$gridy+gridsize, 
  border = NA, col = pal[opp$col])
points(w$x, w$y, pch = 20)
text(w$x, w$y, labels = w$naam, pos = w$pos, cex = 0.8)
mtext("Median dwelling area (m2)", side = 3, line = -1.2, adj = 0, font = 2)
legend("bottom", ncol = 5, legend = lab1, fill = pal, bty = 'n', cex = 0.7,
  border = NA)

# Gini
plot(opp$gridx, opp$gridy, type = 'n', asp = 1, xaxt = "n", yaxt = "n", 
  bty = "n", xlab="", ylab="")
rect(opp$gridx, opp$gridy, opp$gridx+gridsize, opp$gridy+gridsize,
  border = NA, col = pal[opp$colgini])
points(w$x, w$y, pch = 20)
text(w$x, w$y, labels = w$naam, pos = w$pos, cex = 0.8)
mtext("Inequality of dwelling area (Gini coefficient dwelling area)", 
  side = 3, line = -1.2, adj = 0, font = 2)
legend("bottom", ncol = 5, legend = lab2, fill = pal, bty = 'n', cex = 0.7, 
  border = NA)


# For testing: plot borders
# mp <- st_read("maps/gm_2021.shp")
# plot(mp[, "geometry"], add = TRUE, border = "gray")
# pv <- st_read("maps/pv_2021.shp")
# plot(pv[, "geometry"], add = TRUE, border = "black")

dev.off()


