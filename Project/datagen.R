library(tidyverse)

files <- list.files("./airdata",full.names = T)

airdata <- data_frame()

for(i in 1:length(files)){
  dat <- read.csv(files[i]) %>%
    filter(Sample.Duration == "24 HOUR") %>%
    select(State.Code, County.Code, Site.Num, Parameter.Code,  
           Parameter.Name, Year, Units.of.Measure,
           Arithmetic.Mean, State.Name, County.Name,
           Latitude, Longitude, Year) %>%
    mutate(FIPS=paste0(State.Code,"-",County.Code))
  dat <- dat[!duplicated(dat[c("State.Code", "County.Code", "Site.Num", "Parameter.Name")]),]
  airdata <- rbind(airdata,dat)
}

write.csv(airdata,"airdata.csv",row.names = F)