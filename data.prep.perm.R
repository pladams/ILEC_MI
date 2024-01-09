library(arrow)
library(data.table)
library(tidyverse)
library(dtplyr)

ilec_arrow_src <- '/workspace/Projects/ILEC/VBT/Data/ilecdata'

dat.arrow <- arrow::open_dataset(ilec_arrow_src)

# Term No PLT

dat.arrow %>%
  filter(Observation_Year >= 2011 
         & Observation_Year <= 2017 
         & Insurance_Plan != 'Term') %>%
  select(!Observation_Year) %>%
  group_by(
    Sex,
    Smoker_Status,
    Attained_Age,
    Duration,
    Face_Amount_Band,
    Insurance_Plan,
    Number_of_Pfd_Classes,
    Preferred_Class
  ) %>%
  summarize(
    Death_Count=sum(Death_Count),
    Policies_Exposed=sum(Policies_Exposed),
    ExpDth_Cnt_VBT2015=sum(ExpDth_Cnt_VBT2015),
    Death_Claim_Amount=sum(Death_Claim_Amount),
    Amount_Exposed=sum(Amount_Exposed),
    ExpDth_Amt_VBT2015=sum(ExpDth_Amt_VBT2015)
    ) %>%
  collect() %>%
  as.data.table() -> 
  dat.perm

dat.perm[,
         `:=`(Smoker_Status=as.character(Smoker_Status),
              Number_of_Pfd_Classes=as.character(Number_of_Pfd_Classes),
              Preferred_Class=as.character(Preferred_Class))]

dat.perm[is.na(Number_of_Pfd_Classes),
         Number_of_Pfd_Classes:="U"]

dat.perm[is.na(Preferred_Class),
         Preferred_Class:="U"]

dat.perm[,UW:=paste0(Smoker_Status,"/",Preferred_Class,"/",Number_of_Pfd_Classes)]

saveRDS(dat.perm,"dat.perm.rds")
