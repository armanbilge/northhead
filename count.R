#!/usr/bin/env Rscript

zone.names <- read.csv("zones.csv")$name
species <- read.csv("species.txt", header = F)
zone <- list(read.csv("zone1.csv"),
             read.csv("zone2.csv"),
             read.csv("zone3.csv"),
             read.csv("zone4.csv"),
             read.csv("zone5.csv"))

count <- c(nrow(zone[[1]]),
           nrow(zone[[2]]),
           nrow(zone[[3]]),
           nrow(zone[[4]]),
           nrow(zone[[5]]))

r <- cor(count, 1:5, method = "spearman")
print(r) 
n <- 30 # TODO verify value of n
T <- r * sqrt((n-2)/(1-r^2))
print(T)

pdf("count.pdf", 8, 5)
barplot(count,
        names.arg = zone.names,
        ylim = c(0, 20),
        xlab = "Zone",
        ylab = "Total No. of Species")

