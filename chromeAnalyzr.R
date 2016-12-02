library("dplyr")
library("urltools")
library("magrittr")
library("ggplot2")
library("RSQLite")

# create a connection object. 
con <- dbConnect(RSQLite::SQLite(), dbname="/path/to/chrome/History")

# get a list of all tables
# alltables = dbListTables(con)

#query <- 'SELECT urls.url, urls.visit_count FROM urls, visits WHERE urls.id = visits.url;'
query <- 'SELECT datetime(((visits.visit_time/1000000)-11644473600),"unixepoch") as datetime, urls.url, urls.visit_count,
urls.title FROM urls, visits WHERE urls.id = visits.url;'
# get the url table as a data.frame object.
History.data <- dbGetQuery(con, query)
# storing the date values as a date object
# it's a good idea to store date and time values as date object, not as strings or numbers. 
# manipulating date as strings can be difficult. 
# also date and time objects includes functions for extracting character representation of date object.

History.data$datetime = as.POSIXct(History.data$datetime)

#Function that extracts domain from the url hence the urltools !
extract.domain <- function(url){
  df <- suffix_extract(domain(url))
  if (!(is.na(df$domain) && is.na(df$suffix))){
    paste(df$domain, df$suffix, sep='.')
  } else {
    return(url)
  }
}


#add a new column by applying (lapplying actually) extract.domain function to url column to extract the domain names 
History.data$domain <- 
  lapply(History.data$url, extract.domain) %>% 
  unlist() %>%
  factor()


#top ten domain
Top.domains <- group_by(History.data, domain) %>%
  summarise(visitcount = n()) %>% 
  arrange(desc(visitcount)) %>%
  head(., 10)

Top.urls <- group_by(History.data, url) %>%
  summarise(visitcount = n()) %>% 
  arrange(desc(visitcount)) %>%
  head(., 10)

# Date stats
Date.stats <- 
  mutate(History.data, date = strftime(datetime, '%b %d')) %>%
  count(., date)
  
#group_by(date) %>%
#summarise(visitcount = n())

#Day of week stats
Weekday.stats <-
  mutate(History.data, weekday = strftime(datetime, '%a')) %>%
  count(., weekday)

# Hour 
Hr.stats <-
  mutate(History.data, hr = strftime(datetime, '%I %p')) %>%
  count(., hr)

#Day of the month stats (day as a number)
Day.stats <- 
  mutate(History.data, day = as.integer(strftime(datetime, '%d'))) %>%
  count(., day)

#Months stats
Month.stats <-
  mutate(History.data, month = strftime(datetime, '%b')) %>%
  count(., month)


#Let us c!

# A lil bit of customization never kills nobody
# Customized barPlot function :D

barPlot <- function(data, height, names.arg, color='#278DBC', title = '', xlab='', ylab='Visit Count', sortAsc=F, sortDesc=F, annotate=F){
  ann <- geom_text(aes(label = height), vjust = -0.25, size=3)
  if( !sortAsc && !sortDesc ){
    gg <- ggplot(data=data, aes(x=names.arg, y=height))
  } else {
    if( sortAsc ){
      gg <- ggplot(data=data, aes(x=reorder(names.arg,height), y=height)) 
    } else {
      gg <- ggplot(data=data, aes(x=reorder(names.arg,-height), y=height))
    } 
  }
  
  if (annotate){
    gg <- gg + ann
  }
  
  plot <- gg + 
    labs(x=x, y=y, title=title) + 
    geom_bar(stat = 'identity', fill = color) +
    xlab(xlab) + ylab(ylab) + 
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_line(size=.05, color="grey" ),
      panel.background = element_rect(fill='white', colour='white'),
      axis.ticks = element_blank()
    )
  
  return(plot)
}



with(Top.domains, barPlot(Top.domains, visitcount, domain, title='Top domains')) + 
  theme(
    axis.text.x = element_text(angle = 60, hjust=1)
  )

with(Hr.stats, barPlot(Hr.stats, n, hr, title='Time of Day')) + 
  theme(
    axis.text.x = element_text(angle = 60, hjust=1)
  )

with(Month.stats, barPlot(Month.stats, n, month, sortAsc = T, title='Month')) 
# a few tweak for daily stats. 
with(Day.stats, barPlot(Day.stats, n, as.integer(day), title='Day of Month')) + 
  scale_x_continuous(breaks = seq(1, 31, 2))

with(Weekday.stats, barPlot(Weekday.stats, n, weekday, title='Day of Week'))
