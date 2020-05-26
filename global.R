#######################################
## DCP-UHC tool: Making Fair Choices ##
##              BCEPS                ##
#######################################

####################
## required packages
####################
library(shiny)
library(shinythemes)
library(rhandsontable)
library(dplyr)
library(readxl)
library(ggplot2)
library(gridExtra)
library(grid)
library(DT)
library(kableExtra)
library(tidyverse)
library(DT)
library(purrr)
library(rlang)
#library(forcats)


##########################
## get required functions
##########################
source("functions_costs.R")


#################################
## load required data into memory
#################################

## data frame (Basics)------------

df.lic3 <- read.csv("Unit_cost_LIC_select_0422.csv")
df.lic3$target <- 0.8

df.lmic <- read.csv("Unit_cost_LMIC_select_0422.csv")
df.lmic$target <- 0.8



LIC_total_pop <- as.numeric(896507437.8)
LMIC_total_pop <- as.numeric(2671721030)

## data frame (Multiplier)------------
lic <- read_xlsx("LGH Costing_final.xlsx", sheet = "LIC Input")
lmic <- read_xlsx("LGH Costing_final.xlsx", sheet = "LMIC Input")
more.info <- read_xlsx("LGH Costing_final.xlsx", sheet = "More info")

result_table_article <- read.csv("Result_0422.csv")

LIC_GNI_2015 <- as.numeric(794.4947062)
LMIC_GNI_2015 <- as.numeric(2211.473603)

# Data for unit cost adjustment
## GNI
GNI <- read.csv("GNI_WB_USD_atlas_add3.csv")
GNI[GNI$Country == "LIC", c("X2015")] <- 794.4947062
GNI[GNI$Country == "LMIC", c("2015")] <- 2211.473603

## Others
cpi <- read.csv("CPI_WB_2018_added.csv") #Revised
regional_inflation <- read_xls("Regional_inflation_20200212.xls", sheet = "Data")
class <- read_xls("CLASS.xls")

exchange <- read_xlsx("Exchange_rate.xlsx")
regional_inflation[,c(5:64)] <- (regional_inflation[,c(5:64)]/100)+1

all_tradable <- read_xlsx("alltradeable.xlsx")
all_nontradable <- read_xlsx("Nontradeable.xlsx")
