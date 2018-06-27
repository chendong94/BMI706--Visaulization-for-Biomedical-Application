library(tidyverse)

files <- list.files("./airdata",full.names = T)

airdata <- data_frame()

for(i in 1:length(files)){
  dat <- read.csv(files[i]) %>%
    filter(Sample.Duration == "24 HOUR") %>%
    select(State.Code, County.Code, Site.Num, POC, 
           Parameter.Name, Year, Units.of.Measure,
           Arithmetic.Mean, State.Name, County.Name,
           Latitude, Longitude, Year) %>%
    mutate(FIPS=paste0(State.Code,"-",County.Code))
  dat <- dat[!duplicated(dat[c("State.Code", "County.Code", "Site.Num", "Parameter.Name", "POC")]),]
  dat <- dat %>% group_by(State.Code, County.Code, Site.Num, Parameter.Name, Year, 
                          Units.of.Measure, State.Name, County.Name, FIPS, add = T) %>% 
    summarise(Mean = mean(Arithmetic.Mean))
  airdata <- rbind(airdata,dat)
}

write.csv(airdata,"airdata_24h.csv",row.names = F)