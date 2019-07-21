## Scrapbook File ##

## To Run App
#setwd("C:/Users/Anthony/OneDrive/Documents/STAT 6220 Assignment 10")
#library(shiny)
#runApp("Anthony_R_Shiny_App")

Burroughs <- read.csv("C:/Users/Anthony/OneDrive/Documents/STAT 6220 Assignment 10/Anthony_R_Shiny_App/STAT 6220 Burroughs_2017-18 Combined.csv", header = TRUE)
Burroughs$Complete.Labs <- relevel(Burroughs$Complete.Labs, ref = "Yes")
names(Burroughs)[names(Burroughs)=="Complete.Labs"] <- "Complete_Labs"
Burroughs$within90 <- relevel(Burroughs$within90, ref = "Yes")
names(Burroughs)[names(Burroughs)=="within90"] <- "Within90"
Burroughs$Year <- as.factor(Burroughs$Year)

## Get data into Aggregated Form
library(dplyr)
within90_table <- count(Burroughs, Year, Within90)
Labs <- count(Burroughs, Year, Complete_Labs)

## Load ggplot for various graphs
library(ggplot2)

## Fisher's Exact Test with Mid-P Value Correction
library(exact2x2)
