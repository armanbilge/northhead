#!/usr/bin/env Rscript

zone.names <- read.csv("zones.csv")$name
species <- t(read.csv("species.txt", header = F))
zone <- list(read.csv("zone1.csv"),
             read.csv("zone2.csv"),
             read.csv("zone3.csv"),
             read.csv("zone4.csv"),
             read.csv("zone5.csv"))

species.avg <- rep(list(c()), length(species))
species.type <- c()

for (z in zone) {
    for (i in 1:nrow(z)) {
        species.type[z$species[i]] <- z$type[i]
        if (z$type[i] == 'n') a <- 4
        else a <- 1
        z[i,3:8] <- z[i,3:8] * a 
        x <- species.avg[[z$species[i]]]
        species.avg[[z$species[i]]] <- c(x, unlist(z[i,3:8]))
    }
}

for (i in 1:length(species.avg))
    species.avg[[i]] <- mean(species.avg[[i]])

count.thresh <- 4
percent.thresh <- 0.01
dominant.species <- c()
for (i in 1:length(species.avg)) {
    x <- species.avg[[i]]
    if (!is.null(x) && !is.na(x)
      && ((0 < x && x < 1 && x >= percent.thresh)
      || x >= count.thresh))
        dominant.species <- c(dominant.species, i)
}

dominant.species.names <- species[dominant.species]

se <- function(x) sd(x)/sqrt(length(x))
pdf("presence.pdf", 7, 5)

for (z in zone) {
    count.means <- c()
    count.stderr <- c()
    count.names <- c()
    percent.means <- c()
    percent.stderr <- c()
    percent.names <- c()
    for (i in dominant.species) {
        if (i %in% z$species) {
            j <- match(i, z$species)
            m <- mean(unlist(z[j,3:8]))
            s <- se(unlist(z[j,3:8]))
            type <- z$type[j]
        } else {
            m <- 0
            s <- 0
            type <- species.type[i]
        }
        if (type == 'n' || type == 1) {
            count.means <- c(count.means, m)
            count.stderr <- c(count.stderr, s)
            count.names <- c(count.names, species[i])
        } else {
            percent.means <- c(percent.means, m * 100)
            percent.stderr <- c(percent.stderr, s * 100)
            percent.names <- c(percent.names, species[i])
        }
    }
    p <- par(mar = c(9, 5, 2, 5))
    count.means <- c(count.means, rep(0, length(percent.means)))
    count.stderr <- c(count.stderr, rep(0, length(percent.stderr)))
    percent.means <- c(rep(0, length(count.names)), percent.means)
    percent.stderr <- c(rep(0, length(count.names)), percent.stderr)
    bp <- barplot(count.means,
                  ylim = c(0,20),
                  col = "gray50",
                  las = 1,
                  ylab = "Mean abundace per sq. m.")
    title(xlab = "Species", line = 7, cex.lab = 1)
    means <- c(count.means[1:length(count.names)],
               rep(1000000, length(percent.names)))
    stderrs <- count.stderr
    segments(bp, means - stderrs, bp, means + stderrs, lwd = 2)
    segments(bp - 0.1, means - stderrs, bp + 0.1, means - stderrs, lwd = 2)
    segments(bp - 0.1, means + stderrs, bp + 0.1, means + stderrs, lwd = 2)
    par(new = T)
    barplot(percent.means, ylim = c(0,80), col = "gray90", axes = F)
    means <- c(rep(1000000, length(count.names)),
               percent.means[(length(count.names)+1):length(percent.means)])
    stderrs <- percent.stderr
    segments(bp, means - stderrs, bp, means + stderrs, lwd = 2)
    segments(bp - 0.1, means - stderrs, bp + 0.1, means - stderrs, lwd = 2)
    segments(bp - 0.1, means + stderrs, bp + 0.1, means + stderrs, lwd = 2)
    axis(4, las = 1)
    mtext("Mean abundance (% cover)", side = 4, line = 3)
    text(bp,
         par("usr")[3],
         labels = c(count.names, percent.names),
         srt = 45,
         adj = c(1.1,1.1),
         xpd = T,
         cex = 1,
         font = 3)
    par(p)
}

pdf("species.pdf")

for (i in dominant.species) {
    count.means <- c()
    count.stderrs <- c()
    for (z in zone) {
        if (i %in% z$species) {
            j <- match(i, z$species)
            m <- mean(unlist(z[j,3:8]))
            s <- se(unlist(z[j,3:8]))
        } else {
            m <- 0
            s <- 0
        } 
        count.means <- c(count.means, m)
        count.stderrs <- c(count.stderrs, s)
    }
    bp <- barplot(count.means, names.arg = zone.names, main = i, ylab = z$type[j]) 
    means <- count.means
    stderrs <- count.stderrs
    segments(bp, means - stderrs, bp, means + stderrs, lwd = 2)
    segments(bp - 0.1, means - stderrs, bp + 0.1, means - stderrs, lwd = 2)
    segments(bp - 0.1, means + stderrs, bp + 0.1, means + stderrs, lwd = 2)
}
