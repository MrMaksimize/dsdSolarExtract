library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(plotly)

kw_regex = '((\\d|\\.|\\s))*(?=(kw))'

#dsd <- read.csv("http://datasd-prod.s3.amazonaws.com/permits_issued_ytd_datasd.csv", stringsAsFactors = FALSE)
dsd <- read.csv("http://datasd-prod.s3.amazonaws.com/dsd/permits_issued_2015_datasd.csv", stringsAsFactors = FALSE)
dsd <- dplyr::filter(dsd, approval_type_id == 293) %>%
    select(approval_id, 
           issue_date, 
           complete_cancel_date,
           status,
           latitude, 
           longitude, 
           permit_holder, 
           scope) %>%
    mutate(scope = str_to_lower(scope)) %>%
    mutate(scope = str_replace(scope, ",", "")) %>%
    mutate(kw = as.numeric(str_trim(str_extract(scope, kw_regex)))) %>%
    mutate(issue_date = ymd_hms(issue_date)) %>%
    mutate(issue_month = month(issue_date, label=TRUE)) %>%
    dplyr::filter(issue_date < ymd_hms('2016-01-01:00:00:00'))

dsd[is.na(dsd$kw), "kw"] <- 0

permits_by_mon <- dsd %>% group_by(issue_month) %>%
       count(issue_month)

g <- ggplot(permits_by_mon, aes(x=issue_month, y=n)) +
    geom_bar(stat="identity", fill="orange") +
    labs(title="Solar Permits Issued By Month", x = "Month", y = "Number of Permits") +
    theme(panel.background = element_rect(fill="#434343"), 
          panel.grid = element_blank())

permits_by_mon_plot <- ggplotly(g)


plotly_POST(permits_by_mon_plot, filename = "solar_permits_issued_16", sharing = 'public')



write.csv(dsd, "~/Dropbox/MasterSync/DATA/PROD/DSD/issued_solar_2015.csv", row.names = FALSE)
