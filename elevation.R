#!/usr/bin/env Rscript

df <- read.csv("elevation.csv")
distance <- tail(df$horizontal, n = 1) - df$horizontal
elevation <- tail(df$vertical, n = 1) - df$vertical

pdf(file = "elevation.pdf", 7, 2.8)

plot(distance, elevation, type = "o", asp = 1, las = 1,
     xlim = rev(range(distance)),
     ylim = c(0,3),
     yaxp = c(0,3,3),
     xlab = "Distance from the low tide mark (m)",
     ylab = "Elevation above low tide (m)")

y <- function(x) {
    x.max <- tail(distance[x <= distance], n = 1)
    x.min <- distance[x > distance][1]
    y.max <- tail(elevation[x <= distance], n = 1)
    y.min <- elevation[x > distance][1]
    print(c(x.max, x.min, y.max, y.min))
    return((y.max - y.min) / (x.max - x.min) * (x - x.min) + y.min)
}
print(elevation)
print(distance)
zones <- read.csv("zones.csv")
for(i in 1:nrow(zones)) {
    name <- zones$name[i]
    coord <- tail(df$horizontal, n = 1) - zones$coordinate[i] 
    print(y(coord))
    segments(coord, -0.2, y1 = y(coord))
} 
