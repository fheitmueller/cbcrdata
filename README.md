Minimum tax threshold analysis
================
Frederik Heitmüller
10 June 2021

This document contains code to reproduce the graph contained in the
blogpost [“Are G7 countries doing developing countries a favour when
levying global minimum
taxes?”](http://globtaxgov.weblog.leidenuniv.nl/2021/06/10/are-g7-countries-doing-developing-countries-a-favour-when-levying-global-minimum-taxes/)

To reproduce the analysis, download the folder and execute the RMarkdown
file
[“R/minimum\_tax\_threshold.Rmd”](https://github.com/fheitmueller/cbcrdata/blob/main/R/minimum_tax_threshold.Rmd).
You need a recent version of [R](https://www.r-project.org/) and
[RStudio](https://www.rstudio.com/).

The following R packages are used in the analysis:

``` r
installed.packages()[names(sessionInfo()$otherPkgs), "Version"]
```

    ## countrycode         WDI      ggpubr      scales    magrittr        here 
    ##     "1.2.0"     "2.7.4"     "0.4.0"     "1.1.1"     "2.0.1"     "1.0.1" 
    ##     forcats     stringr       dplyr       purrr       readr       tidyr 
    ##     "0.5.1"     "1.4.0"     "1.0.6"     "0.3.4"     "1.4.0"     "1.1.3" 
    ##      tibble     ggplot2   tidyverse 
    ##     "3.1.2"     "3.3.3"     "1.3.1"

The data source for the graph is aggregated country by country report
data for the year 2016, downloaded from the [OECD
website](https://stats.oecd.org/Index.aspx?DataSetCode=CBCR_TABLEI) on 9
November 2020.

In a first step, data is cleaned, information on country income groups
from the World Bank is added, and effective tax rates are calculated
using the country by country reporting variables “Tax Accrued” and
“Profit”.

``` r
#reading in data
cbcr <- read_csv(here("data_raw/CBCR_TABLEI.csv"))

# filtering out irrelevant variables
cbcr %<>% filter(CBC %in% c("PROFIT", "TAX_ACCRUED", "SUBGROUPS_COUNT"))

# adding data on the World Bank income group classification of the investment-receiving jurisdictions, 
world_bank <- WDI(start=2016, end=2016, extra=TRUE)
cbcr %<>% left_join(world_bank[c("iso3c", "income")], by=c("JUR"="iso3c"))

cbcr %<>% select(-Variable)
cbcr %<>% pivot_wider(names_from= "CBC", values_from = "Value")

# removing entries with either missing profit or tax accrued values.
cbcr %<>% filter(!is.na(PROFIT)&!is.na(TAX_ACCRUED))

## keeping only sub-groups with positive profits and calculating effective tax rates
cbcr_etr <- cbcr %>% filter(Grouping =="Sub-Groups with positive profits") %>% mutate(effective_rate = TAX_ACCRUED / PROFIT)
```

In a next step, a boxplot graph is produced to show the distribution of
effective tax rates by MNE groups from different home countries within a
given host country. For the interpretation of boxplot graphs, see for
example [this
post](https://www.dummies.com/education/math/statistics/what-a-boxplot-can-tell-you-about-a-statistical-data-set/).
It is important to note that the distribution is not the true
distribution, since every home country only reports an aggregate figure
for all MNE subgroups present in a given host jurisdiction. The graph
therefore assumes that the effective tax rate is the same for all MNE
subgroups from a given home jurisdiction. Moreover, India does not
report the number of MNE subgroups on which its aggregate is based. For
this group, it is assumed to be 1, which likely understates the true
number.

In reality, the distribution of effective tax rates is thus likely to be
bigger than displayed in this graph. For the purposes of this graph this
is not a problem, though, because its purpose is to show mainly that
certain MNE groups have effective tax rates lower than 15% in some low
and lower middle income countries. This graph likely underestimates the
number.

``` r
# filter out subsidiaries in the home country (since these would not be affected by a minimum tax)

cbcr_etr %<>% filter(JUR!=COU)

# filter out subgroups with ETRs below 0 and above 100%

cbcr_etr %<>% filter(effective_rate>=0&effective_rate<1)

# India does not report subgroup counts, the number is in that case assumed to be 1 for all jurisdictions ()
cbcr_etr$SUBGROUPS_COUNT %<>% replace_na(1)

# Repeat the home-host pair rows by the number of subgroups for each home-host datapoint. Each row now represents one MNE subgroup. Attention this assumes that each subgroup from a given home jurisdiction has the same ETR, which likely not the case. The distribution is thus artificially narrowed. Moreover, subgroups from India are likely to be underrepresented in the data. This needs to be considered when interpreting the data.
cbcr_etr <- cbcr_etr[rep(seq(nrow(cbcr_etr)), cbcr_etr$SUBGROUPS_COUNT),]

# calculate median effective tax rate for each host country 

cbcr_etr %<>% group_by(JUR) %>% mutate(average_ETR = median(effective_rate, na.rm=TRUE)) 

## adding short country names for better readability in the graph
cbcr_etr %<>% mutate(short_name = countrycode(JUR, "iso3c", "cldr.short.en"))

## make a plot ordered by median ETR

plot_parameters <- function(data, countrygroup){
  ggplot(filter(data, income %in% countrygroup), aes(x=reorder(short_name, average_ETR)))+
    geom_boxplot(aes(y=effective_rate))+
    theme_pubclean()+
    theme(axis.text.x=element_text(angle=90), axis.title.x=element_blank())+
    geom_hline(aes(yintercept=0.15), color="red")+
    geom_hline(aes(yintercept=0.21), color="red", linetype="dashed")+
    scale_y_continuous(labels = scales::percent)+
    labs(y="effective tax rate",
      title = "Distribution of MNE subgroup ETRs",
      subtitle= ifelse(length(countrygroup)==2, paste(countrygroup[1], "and", countrygroup[2], "host countries"), paste(countrygroup, "host countries")),
      caption= "Data from OECD CbCR Aggregate Totals by Jurisdiction, 2016, sub-groups with positive profits.\n ETR = Tax Accrued / Profit. Negative tax rates and rates above 100% have been removed.\n  One point in the distribution is one MNE subgroup, assuming the same ETR for subgroups from the same home country.\n Red line = 15%, dashed red line = 21%")
}


plot_parameters(cbcr_etr, c("Low income", "Lower middle income"))
```

![](minimum_tax_threshold_files/figure-gfm/etrs%20by%20jurisdiction%20pairs-1.png)<!-- -->

``` r
ggsave("distribution_plot.png", dpi=1500)
```
