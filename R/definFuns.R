
#library(mosaic)
#library(gt)

# .onLoad <- function(libname, pkgname) {
#   library(tidyverse)
#   library(broom)
#   library(gt)
#   library(readxl)
#   library(mosaic)
#   library(DataEditR)
#   library(missMethods)
#   library(REAT)
#   library(gtsummary)
#   library(ggplot2)
#   library(reshape2)
# }

mad <- aggregatingFunction1(stats::mad)

gini <- aggregatingFunction1(REAT::gini)

confint <- aggregatingFunction1(stats::confint)

nice <- function(tab,rowVar="",colVar="",title="",sn="")
{
  if(is.matrix(tab))
  {
    tab <- as.data.frame.matrix(tab,fix.empty.names=F)
    tab <- cbind(rownames(tab),tab)
    names(tab)[1] <- "S"
  }
  else tab <- as.data.frame(tab)

  gt(tab,rowname_col = "S") %>%
    tab_stubhead(label = rowVar) %>%
    tab_spanner(label = colVar, columns = names(tab)) %>%
    tab_header(title=title) %>%
    tab_source_note(source_note = sn)
}

niceT <- function(tab,rowVar="",colVar="",title="",sn="")
{
  nam <- names(tab)
  tab <- tab %>%
    mutate(!!nam[3]:=unlist(.data[[nam[3]]])) %>%
    pivot_wider(names_from=nam[2],values_from = nam[3],
                values_fill = NaN)
  kat <- as.character(tab[[1]])
  names(tab)[1] <- "S"
  gt(tab,rowname_col = "S") %>%
    tab_stubhead(label = rowVar) %>%
    tab_spanner(label = colVar, columns = names(tab))  %>%
    tab_header(title=title) %>%
    tab_source_note(source_note = sn)
}
