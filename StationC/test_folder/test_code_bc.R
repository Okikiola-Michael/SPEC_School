libs <- list('terra', 'tidyverse', 'ggplot2', 'corrplot')
invisible(lapply(libs, library, character.only = T))

# Set wd
wd <- '~/Current Projects/SpecSchool/SPEC_School/StationC/'
setwd(wd)

# load the data
plots <- vect('data/Kamoske_etal_2022_data/Kamoske_etal_2022_data/neon_plots_mlbs.shp')
agb <- rast('data/MLBS_agbEcoregion_20m.tif')
MLBS.data <- read.csv('data/Kamoske_etal_2022_data/Kamoske_etal_2022_data/all_metrics_20200803.csv') %>%
    filter(siteID == 'MLBS')

# display the data
plot(agb)
plot(plots, add = T)

# extract the agb points
agb.points <- terra::extract(agb, plots, fun=mean, ID = F)
# add the plotID names
agb.points <- cbind.data.frame(plotID = plots$id, agb.points)
names(agb.points)[2] <- 'AGB'

# merge the data
MLBS.data <- merge(MLBS.data, agb.points, by = 'plotID')
summary(MLBS.data)


# Relationship between diversity and AGB ----------------------------------
# quick linear model
summary(lm(AGB ~ diversity_shannon, data = MLBS.data)) # p = 0.132
cor(MLBS.data$AGB, MLBS.data$diversity_shannon) # rho = 0.272

# spatial autocorrelation correction attempts
summary(lm(AGB ~ diversity_shannon + latitude + longitude, data = MLBS.data)) # p = 0.072
MLBS.data$AGBresid <- resid(loess(AGB ~ latitude + longitude, data = MLBS.data))
summary(lm(AGBresid ~ diversity_shannon, data = MLBS.data)) # p = 0.4

# plotting the relationship
ggplot(MLBS.data) +
    geom_point(aes(x = diversity_shannon, y = AGB), size = 4, shape = 1, stroke = 1) +
    geom_abline(slope = 37.84, intercept = 125.09, linewidth = 1, color = 'red') +
    annotate('text', x = 1.7, y = 200, label = 'p = 0.132', size = 6, color = 'red') +
    theme_bw(base_size = 20) +
    labs(x = 'Shannons Diversity', y = 'AGB')



# Relationship between AGB and other vars ---------------------------------
AGB.data <- MLBS.data %>% select(AGB,
                                 eastness_mean, northness_mean, 
                                 slope_mean, tpi_mean, tri_mean, dsm_mean,
                                 latitude, longitude,
                                 PRI_mean, NDVI_mean, lai_mean)

c.agb <- corrplot(cor(AGB.data))
c.agb$corr[1,]

#or 

c.agb <- ggcorrplot(cor(AGB.data), lab = T) #makes more sense when you zoom the plot
c.agb

# Relationship between diversity and other vars ---------------------------
Div.data <- MLBS.data %>% select(diversity_shannon,
                                 eastness_mean, northness_mean, slope_mean, tpi_mean, tri_mean, dsm_mean,
                                 latitude, longitude,
                                 PRI_mean, NDVI_mean, lai_mean)

c.div <- corrplot(cor(Div.data))
c.div$corr[1,]

c.div <- ggcorrplot(cor(Div.data), lab = T) #makes more sense when you zoom the plot
c.div


# Selecting the best model to predict AGB

best_model <- regsubsets(AGB ~ chm_mean + 
                     convexHull + dtm_mean + lai_mean +  
                    NDVI_mean + NIR_mean + pc1_mean + 
                    pc2_mean + porosity_ratio_mean + 
                    PRI_mean + RVSI_mean + slope_mean + 
                    SWIR1_mean + SWIR2_mean + slope_mean + 
                    tpi_mean + tri_mean + dsm_mean, nvmax = 1, 
                   nbest = 7, data = MLBS.data)

mod_summ = with(summary(best_model), data.frame(rsq, adjr2, cp, rss, bic, outmat))
View(mod_summ)

# The best predictor is the mean canopy height metrics with R-squared value of 93%
cor(AGB, Predict)
lm1 <- lm(AGB ~ chm_mean, data = MLBS.data)
summary(lm1)

MLBS.data$Pred <- fitted(lm1) #predicted values
MLBS.data$Resid2 <- residuals(lm1) #residuals from the model


# Diagnostic tests and plots

plot1 <- ggplot(MLBS.data, aes(x =  Pred, y = Resid2)) + 
  geom_point(cex = 2, pch = 1, color ="red") + 
  theme_bw() +
  labs(x = "Fitted Values", y = "Residuals") + 
  geom_hline(yintercept = 0, col = "blue") +
  theme(plot.title = element_text(hjust = 0.5))


plot2 = ggplot(MLBS.data, aes(x = AGB, y = Pred)) + 
  geom_point(cex = 2, pch = 1, color ="red") + theme_bw() +
  labs(x = "Observed AGB", y = "Fitted AGB") + 
  geom_abline(intercept = 0, slope = 1, col = "blue") +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange( plot1, plot2, ncol =2)




