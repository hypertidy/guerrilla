
#install.packages("readr")
#install.packages("palr")
library(palr)

#pr <- pretty(tpal$breaks)
library(readr)
library(raster)
x <- read_csv("etc.csv")
x$rawtime <- as.numeric(x$END_DATE)
par(mfrow = c(2, 1))
rng <- as.matrix(x[, c("rawtime", "dbar")])
rng[,2] <- -rng[,2]
r0 <- raster(extent(rng), ncol = length(unique(x$END_DATE)), nrow = length(unique(x$dbar)))
r1 <- rasterize(rng, r0, field = x$temp_vals)
## smooth it a bit
r <- focal(r1, matrix(1, 5, 3), fun = median, na.rm = TRUE)

par(mfrow = c(2, 1))
sstp <- sstPal(palette = TRUE)
prb <- pretty(x$temp_vals, n = 7)
ind <- findInterval(prb, sstp$breaks)
plot(r, asp = NA, axes = FALSE, col = sstp$cols, breaks = sstp$breaks, legend = FALSE)
plot(r, asp = NA, axes = FALSE, col = sstp$cols[ind], breaks = prb, legend.only = TRUE)
# axis(2) #why tf is this not working?
title("Temperature curtain", ylab = "depth", xlab = "time")
box()
