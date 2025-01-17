source('~/R/clean.r')
library(sp)
library(raster)
library(neonUtilities)
library(neonOS)
library(geoNEON)
# devtools::install_github('NEONScience/NEON-geolocation/geoNEON')
library(geoNEON)
options(stringsAsFactors=F)
# install.packages('hemispheR') 
#Use this if the R version is not available for your R version
# devtools::install_git("https://gitlab.com/fchianucci/hemispheR")
library(hemispheR)


# wd <- '~/Current Projects/SpecSchool/NEON_LAI/'
# setwd(wd)

dpid <- "DP1.10017.001"
site <- "MLBS"
date <- '2022-10'
date <- '2022-11'

LAI <- loadByProduct(dpID = dpid,
                     site = site,
                     startdate = date,
                     enddate = date2,
                     check.size = F)

urls <- grep('overstory',LAI$dhp_perimagefile$imageFileUrl, value = T)
nfile <- length(urls)
dir.create(date)
setwd(date)



for (i in seq(nfile)) {
    iurl <- urls[i]
    name <- unlist(strsplit(iurl, '/'))[10]
    
    download.file(destfile = name, method ='auto', url=iurl, quiet = F)
    
    # LAI calculation from hemispheR package 
    
    image <- name 
    display = T
    img<-import_fisheye(iurl,
                        channel = 'B',
                        circ.mask=list(xc=80,yc=60,rc=80), #xcenter, ycenter, radius
                        circular=F,
                        gamma=1,
                        stretch=T,
                        display=display,
                        message=T)
    
    
    # Once imported, the function performs image classification using a single automated thresholding 
    # from the auto_thresh() functionality of the autothresholdr package:
    img.bw<-binarize_fisheye(img,
                             method='Otsu',
                             zonal=F,
                             manual=NULL,
                             display=display,
                             export=F)
    
    
    # The gapfrac_fisheye() function retrieve the angular distribution from classified fisheye images, 
    # considering the fisheye lens correction, as:
    gap.frac<-gapfrac_fisheye(
        img.bw,
        maxVZA = 90,
        lens = "equidistant",
        startVZA = 0,
        endVZA = 80,
        nrings = 7,
        nseg = 8,
        display=display,
        message = F)
    
    # calculate LAI
    canopy<- canopy_fisheye(gap.frac)
    canopy
    # LAI = 4 using high-res
    # LAI = 3 using jpeg
    
    # remove the downloaded files
    # comment out if you want to keep the photos
    file.remove(name)
    
    # Data frame creation (currently could be better to include geolocation coordinates)
    if (i == 1) {LAI.vals <- canopy$L} else {LAI.vals <- c(LAI.vals, canopy$L)}
    
}

