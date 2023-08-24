
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

analyzeOrd <- function(data,cat,val,lev,tit="")
{
  newDat <- data %>%
    select(cat={{cat}},val={{val}}) %>%
    mutate(factor(cat,levels=lev,ordered = T)) %>%
    group_by(cat) %>%
    summarise(Median = median(val,na.rm = T),
              Q25=quantile(val,prob=0.25,na.rm = T,type=3),
              Q75=quantile(val,prob=0.75,na.rm = T,type=3))
  colnames(newDat)[1] <- cat
  nice(newDat,title=tit)

}
