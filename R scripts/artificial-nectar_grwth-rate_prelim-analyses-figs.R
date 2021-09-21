#Artificial nectar growth rate trial
#preliminary figure and analyses


#load libraries
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(growthrates)
library(growthcurver)

#load data
angr <- read_csv("data files/artificial-nectar_grwth-rate-data_cl.csv")
View(angr)

angr_key <- read_csv("data files/artificial-nectar_grwth-rate_key.csv")
View(angr_key)


theme_set(theme_classic())


#Plotting growth curves for yeast strains--------------------

#plotting growth curves, faceted by strain and yeast extract treatment, colored by replicate
gr_plot <- ggplot(angr, aes(x=dec.time, y=OD, color=rep, group=rep))
gr_plot + geom_point(
) + geom_line(
) + facet_wrap(strain ~ trt)



#remove blanks, as they did not have growth
angr_nb <- subset(angr, strain!="blank")


#plotting growth curves, faceted by strain and yeast extract treatment, colored by replicate--no blanks
gr_plot <- ggplot(angr_nb, aes(x=dec.time, y=OD, color=rep, group=rep))
gr_plot + geom_point(
) + geom_line(
) + facet_wrap(strain ~ trt)


#take log of OD
angr$log_od <- log(angr$OD)
angr_nb$log_od <- log(angr_nb$OD)

#plotting growth curves, faceted by strain and yeast extract treatment, colored by replicate
gr_log_plot <- ggplot(angr_nb, aes(x=dec.time, y=log_od, color=rep, group=rep))
gr_log_plot + geom_point(
) + geom_line(
) + facet_wrap(strain ~ trt)




#Using growthcurver package to analyze growth rate data-----------------

#convert data to wide format, with well names as column names

angrw <- angr %>% pivot_wider(id_cols = dec.time, names_from = well_id, values_from = OD)


#fit curve to data from a single well
#package seems to have trouble with data that are more quadratic than sigmoidal--doesn't match the 
#downward curve of the data after hitting the maximum
b2_fit <- SummarizeGrowth(angrw$dec.time, angrw$B2)
b2_fit
plot(b2_fit)


#looking at plate as a whole, using default "min" background correction method, outputting plots of
#growth curves
plate_fit <- SummarizeGrowthByPlate(angrw, plot_fit = TRUE, 
                                    plot_file = "gc_plots.pdf")

View(plate_fit)


#rename "sample" in plate_fit to be "well_id", to match angr_key for adding strain and treatment info
plate_fit <- rename(plate_fit, well_id = sample)

#add strain and treatment data to model output by using inner_join
plate_fit <- inner_join(plate_fit, angr_key)

#separate out strain, treatment, and replicate into their own columns
plate_fit <- plate_fit %>% separate(strain_trt_rep_id, 
                                    into = c("strain", "trt", "rep"), sep = "_", remove = FALSE)




#Using growthrates package to analyze growth rate data---------------

#split data into single treatment and strain for initial understanding of package
splitd <- multisplit(angr, c("strain", "trt", "rep"))
dat1 <- splitd[[1]]


#trying to find the maximum growth rate with easylinear function
lin_fit1 <- fit_easylinear(dat1$dec.time, dat1$OD)
summary(lin_fit1)


#plot fit with base r
par(mfrow = c(1, 2))
plot(lin_fit1, log = "y")
plot(lin_fit1)


