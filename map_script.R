library(tmap)
library(sf)
library(dplyr)
library(spData)
library(ggplot2)
library(USAboundaries)
library(USAboundariesData)
library(viridis)

codf <- read.csv("covid.csv") #data from OHA

str(codf) #check it out

or_map <- us_counties(resolution = "high", states = c("Oregon")) #state map w/ counties

orderedm <- or_map[order(or_map$name),] #order by county name for merge purposes

or_data<-merge(orderedm, codf, by.x="name", by.y="County")

str(or_data)

orcases <- ggplot(or_data) + geom_sf(aes(fill = Cases)) + scale_fill_viridis_c(option = "magma") +
  theme_classic() + theme(axis.line.y = element_blank(), 
                          axis.line.x = element_blank(), 
                          axis.text.x = element_blank(), 
                          axis.ticks = element_blank(),
                          axis.text.y = element_blank()) +
  ggtitle("COVID-19 Cases in Oregon (5/10/2021)")

dev.off()
jpeg(filename = "OR_case_map.jpeg",res=300,height=7, width=7, units = "in")
orcases
dev.off()
