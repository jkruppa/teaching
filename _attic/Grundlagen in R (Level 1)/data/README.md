```r
## ------------------------------------------------------------
## by J.Kruppa on Friday, July 30, 2021 (08:42)
library(tidyverse)
library(readxl)
library(haven)

## Wo ist was? Und wo bin ich?

getwd()

dir()

## txt 

read_delim("dummy_data_ws.txt", delim = " ")

read_delim("dummy_data_tab.txt", delim = "\t")


## csv 

read_csv("dummy_data.csv")

read_csv2("dummy_data_semicolon.csv")

## xlsx

read_excel("dummy_data.xlsx", sheet = 1)

## rds

read_rds("dummy_data.rds")

## sav (SPSS)

read_sav("dummy_data.sav")


## dta (Stata)

read_dta("dummy_data.dta")

## SAS

read_sas("dummy_data.sas")


## xlsx (all sheets / alle Tabellenblätter)
## Source: https://stackoverflow.com/a/12945838 (Jeromy Anglim) modified by J. Kruppa

library(readxl)    
read_excel_allsheets <- function(filename) {
    sheets <- readxl::excel_sheets(filename)
    x <- lapply(sheets, function(x) readxl::read_excel(filename, sheet = x))
    return(x)
}

foo <- read_excel_allsheets("dummy_data_sheets.xlsx")

bar <- foo[[1]]
foo[[2]]
foo[[3]]
```
