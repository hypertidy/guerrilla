# Q: should I use resample, reprojection, or aggregate?

#It depends on the data, unfortunately. All of your concerns are important, but whether and where they matter will come back to what your data describe. 

#Here's an example, showing that we can maintain simple statistical quantities with careful handling, but
#what will be appropriate depends mostly on your unstated purposes. 

#When the reprojection is happening between different coordinate system is another subtlety here, 
#and it really matters whether your method needs to be integrative and quantitative or just representative. 



library(raster)
## download a polar sea ice concentration data set 
## ~ 100Kb, use last year so uri is permanent
u <- "ftp://sidads.colorado.edu/pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/final-gsfc/south/daily/2014/nt_20140915_f17_v01_s.bin"
f <- basename(u)
if (!file.exists(f)) download.file(u, f, mode = "wb")

ice <- raster("nt_20150501_f17_nrt_s.bin")
ice0 <- ice
## set all missing values to 0
ice0[is.na(ice0)] <- 0

#plot(ice)


g4 <- raster(ice); res(g4) <- res(ice) * 4


## reprojection (default is bilinear)
rp <- projectRaster(ice, g4)

## aggregation
ags <- aggregate(ice, fact = 4, fun = sum)
agm <- aggregate(ice, fact = 4, fun = mean)

## resample (default is bilinear)
rs <- resample(ice, g4)



 ## only aggregate by sum is the same
cellStats(ice, sum)
#[1] 1307671
cellStats(rp, sum)
#[1] 82657.5
cellStats(ag, sum)
#[1] 1307671
cellStats(rs, sum)
#[1] 89769.85
 
 
## now aggregate is different, because of missing values giving
## different sample sizes for "mean"
cellStats(ice, mean)
#[1] 15.78303
cellStats(rp, mean)
#[1] 15.91404
## different
cellStats(agm, mean)
#[1] 16.67377
cellStats(rs,  mean)
#[1] 16.90581

## aggregate is same after downscaling if all pixels accounted for
cellStats(ice0, mean)
#[1] 12.46446
cellStats(aggregate(ice0, fact = 4, fun = mean), mean)
#[1] 12.46446

