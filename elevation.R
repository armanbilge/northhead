#!/usr/bin/env Rscript

df <- read.csv("elevation.csv")
distance <- tail(df$horizontal, n = 1) - df$horizontal
elevation <- tail(df$vertical, n = 1) - df$vertical

pdf(file = "elevation.pdf", 7, 2.8)

plot(distance, elevation, type = "o", asp = 1, las = 1, bty = "n",
     xlim = rev(range(distance)),
     ylim = c(0,3),
     yaxp = c(0,3,3),
     xlab = "Distance from the low tide mark (m)",
     ylab = "Elevation above low tide (m)")

f <- function(x) {
    x.max <- tail(distance[x < distance], n = 1)
    x.min <- distance[x >= distance][1]
    y.max <- tail(elevation[x < distance], n = 1)
    y.min <- elevation[x >= distance][1]
    if (is.na(x.min)) x.min <- 0
    if (is.na(y.min)) y.min <- 0
    print(c(x.max, x.min, y.max, y.min))
    return((y.max - y.min) / (x.max - x.min) * (x - x.min) + y.min)
}

zones <- read.csv("zones.csv")
par(xpd = NA)
for (i in 1:nrow(zones)) {
    name <- levels(zones$name)[zones$name[i]]
    max <- tail(df$horizontal, n = 1) 
    coord <- max - zones$coordinate[i] 
    segments(coord, -0.2, y1 = f(coord), lty = 44)
    x <- max - (zones$coordinate[i] + c(0, zones$coordinate)[i]) / 2
    y <- f(coord) + 0.6 
    text(x, y, labels = name, srt = 45, pos = 4, offset = -0.5)
} 
