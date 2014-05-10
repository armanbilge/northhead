#!/usr/bin/env Rscript

df = read.csv("elevation.csv")
distance = tail(df$horizontal, n = 1) - df$horizontal
elevation = tail(df$vertical, n = 1) - df$vertical

pdf(file = "elevation.pdf", 7, 2.8)

plot(distance, elevation, type = "o", asp = 1, las = 1,
     xlim = rev(range(distance)),
     ylim = c(0,3),
     yaxp = c(0,3,3),
     xlab = "Distance from the low tide mark (m)",
     ylab = "Elevation above low tide (m)")
