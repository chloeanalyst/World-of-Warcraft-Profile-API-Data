#------------------------------------------------------------------------------- Load required packages.

library(httr)
library(jsonlite)
library(dplyr)
library(tidyverse)

#------------------------------------------------------------------------------- Load function.

source("Function.R")

#------------------------------------------------------------------------------- User Inputs.


access_token <- "&access_token=YOUR TOKEN"

realm <- "YOUR REALM"

raiders <- list('USERNAME',
                'USERNAME',
                'USERNAME',
                'USERNAME',
                'USERNAME',
                'USERNAME')

rbg.stats(raiders, realm, access_token)
