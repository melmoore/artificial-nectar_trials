#Artificial Nectar growth rate experiment
#cleaning script


#load libraries
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)



#load data
angr <- read_csv("data files/artificial-nectar_grwth-rate-data_raw.csv")
View(angr)

angr_key <- read_csv("data files/artificial-nectar_grwth-rate_key.csv")
View(angr_key)


theme_set(theme_classic())

#Matching well IDs to strain, treatment and replicate IDs---------------

#Plot temp column to see if there are anomalies
temp_plot <- ggplot(angr, aes(x=Time, y=Temp_600))
temp_plot + geom_point()

#transform data to long rather than wide

angr <- angr %>% pivot_longer(-c(Time, Temp_600), names_to = "well_id", values_to = "OD")
View(test)

#merge plate data with key meta data using inner_join
angr <- inner_join(angr, angr_key)
View(angr)


#separate out strain, treatment, and replicate into their own columns
angr <- angr %>% separate(strain_trt_rep_id, into = c("strain", "trt", "rep"), sep = "_", remove = FALSE)

#add "rep_" to values in column "rep"
angr$rep <- sub("^", "rep_", angr$rep)


#rename columns to be lower case
angr <- angr %>% rename(time = Time, temp_600 = Temp_600)


#convert times to decimal time for better analysis and plotting-------------
dec.time<-function(x) {
  x<-as.character(x)
  sapply(strsplit(x,":"),function(x){
    x <- as.numeric(x)
    y<-x[1]+x[2]/60
    
  })
}


angr$dec.time <- dec.time(angr$time)


#save to csv file

write.csv(angr, "artificial-nectar_grwth-rate-data_cl.csv", row.names = FALSE)
