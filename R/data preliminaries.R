library('tidyverse')
library('willsutils')
library('readxl')
library('lubridate')
library('ggrepel')

library('boot')


# import data and create some new variables
dat<-read_xlsx(file.path("data-raw","I-SPY DATA (prepared).xlsx")) %>%
  mutate(ven_duration=as.duration(hm(ven_end_time)-hm(ven_start_time)),
         post_duration=as.duration(hm(post_end_time)-hm(post_start_time)),
         PP=BPS-BPD,
         IVCci=1-(IVCi/IVCe),
         VTI=(VTI1+VTI2+VTI3)/3,
         ECHO_SV=pi*((LVOT_diam/2)^2)*VTI,
         ECHO_CO=ECHO_SV*HR/1000,
         FTc=flow_time/sqrt(cycle_time),
         eCBV=(height^(0.5))*log(weight)*MAP*ECHO_CO,
         BSA=0.007184*(height^0.725)*(weight^0.425),
         CFi=CF/BSA,
         USCOM_CI=USCOM_CO/BSA,
         USCOM_SVI=USCOM_SV/BSA,
         ECHO_CI=ECHO_CO/BSA,
         ECHO_SVI=ECHO_SV/BSA,
         LIDCO_CI=LIDCO_CO/BSA,
         LIDCO_SVI=LIDCO_SV/BSA
  )


# create data frame containing only baseline patient data
datpat<-dat %>%
        filter(stage=='pre') %>%
        select(ID,date,sex,age,height,weight,BSA,LVOT_diam,
               ven_start_time,ven_end_time,post_start_time,post_end_time,ven_duration,post_duration)

# create data frame containing pre- and post-venesection data
datlong<-dat %>%
        select(-date,-sex,-age,-height,-weight,-BSA,-LVOT_diam,
               -ven_start_time,-ven_end_time,-post_start_time,-post_end_time,-ven_duration,-post_duration)


# what does each variable name mean?
datmeta<-data.frame(measurement=names(datlong)) %>%
          mutate(measurement_long=plyr::revalue(measurement,
                        c( "BPS"="Systolic blood pressure",
                           "BPD"="Diastolic blood pressure",
                           "PP"="Pulse pressure",
                           "MAP"="Mean arterial pressue",
                           "HR"="Heart rate",
                           "IVCe"="IVC expiration",
                           "IVCi"="IVC inspiration",
                           "Tcol"="Time to collapse",
                           "CF"="Carotid flow",
                           "CFi"="Carotid flow index",
                           "USCOM_CO"="USCOM cardiac output",
                           "USCOM_SV"="USCOM stroke volume",
                           "LIDCO_SV"="LIDCO stroke volume",
                           "LIDCO_CO"="LIDCO cardiac output",
                           "LIDCO_SVV"="LIDCO stroke volume variation",
                           "LIDCO_PPV"="LIDCO pulse pressure varation",
                           
                           "USCOM_CI"="USCOM cardiac output index",
                           "USCOM_SVI"="USCOM stroke volume index",
                           "LIDCO_CI"="LIDCO cardiac output index",
                           "LIDCO_SVI"="LIDCO stroke volume index",
                           
                           "flow_time"="Flow time",
                           "cycle_time"="Cycle time",
                           "IVCci"="IVC collapsibility index",
                           "VTI"="VTI",
                           "ECHO_SV"="ECHO stroke volume",
                           "ECHO_CO"="ECHO cardiac output",
                           "ECHO_SVI"="ECHO stroke volume index",
                           "ECHO_CI"="ECHO cardiac output index",
                           "FTc"="Flow time corrected",
                           "eCBV"="eCBV"
                         )
                      ),
                 measurement_short=plyr::revalue(measurement,
                        c( "BPS"="SBP",
                           "BPD"="DBP",
                           "PP"="PP",
                           "MAP"="MAP",
                           "HR"="HR",
                           "IVCe"="IVCe",
                           "IVCi"="IVCi",
                           "Tcol"="Time to collapse",
                           "CF"="Carotid flow",
                           "CFi"="Carotid flow index",
                           "USCOM_CO"="USCOM CO",
                           "USCOM_SV"="USCOM SV",
                           "LIDCO_SV"="LIDCO SV",
                           "LIDCO_CO"="LIDCO CO",
                           "LIDCO_SVV"="LIDCO SVV",
                           "LIDCO_PPV"="LIDCO PPV",
                           
                           "USCOM_CI"="USCOM CI",
                           "USCOM_SVI"="USCOM SVI",
                           "LIDCO_SVI"="LIDCO SVI",
                           "LIDCO_CI"="LIDCO CI",
                           
                           "flow_time"="Flow time",
                           "cycle_time"="Cycle time",
                           "IVCci"="IVC CI",
                           "VTI"="VTI",
                           "ECHO_SV"="ECHO SV",
                           "ECHO_CO"="ECHO CO",
                           "ECHO_SVI"="ECHO SVI",
                           "ECHO_CI"="ECHO CI",
                           "FTc"="FTc",
                           "eCBV"="eCBV"
                         )
                      )
          )

# create data frame with a row per patient per variable
datlonger<-datlong %>%
  select(-VTI1, -VTI2, -VTI3) %>%
  gather(measurement, value, -ID, -stage) %>%
  spread(stage, value) %>%
  mutate(dif=post-pre,
         ave=(post+pre)/2,
         difnorm=ifelse(dif==0,0,dif/ave)*100
         ) %>%
  left_join(datmeta, by='measurement')



datwide<-datlonger %>%
  select(ID, measurement, dif) %>%
  spread(measurement, dif)


# create a data frame summarising each variable (nicer way to do this with tidyr but meh)
datsummary<-datlonger %>%
  group_by(measurement) %>%
  summarise(n=sum(!is.na(dif)),
            nchange=sum(!is.na(dif) & dif!=0),
            nflat=sum(dif==0,na.rm=TRUE),
            nrose=sum(dif>0,na.rm=TRUE),
            nfell=sum(dif<0,na.rm=TRUE),
            meanpre=mean(pre,na.rm=TRUE),
            meanpost=mean(post,na.rm=TRUE),
            corr=cor(pre,post,use='complete.obs'),
            meandif=mean(dif,na.rm=TRUE),
            meandifnorm=mean(difnorm,na.rm=TRUE),
            sddif=sd(dif,na.rm=TRUE),
            minval=min(c(pre,post),na.rm=TRUE),
            maxval=max(c(pre,post),na.rm=TRUE),
            minvalmean=min((pre+post)/2,na.rm=TRUE),
            maxvalmean=max((pre+post)/2,na.rm=TRUE),
            minvaldif=min(dif,na.rm=TRUE),
            maxvaldif=max(dif,na.rm=TRUE),
            cv=sddif/meandif,
            cvinv=1/cv,
            lo_naive=log(nrose/nfell),
            lo=log((nrose+(nflat/2))/(nfell+(nflat/2))),
            directionLO = case_when(sign(lo)==1 ~ "+",
                                    sign(lo)==-1 ~ "-",
                                    sign(lo)==0 ~ ".",
                                   TRUE ~ NA_character_),
             directionCV = case_when(sign(cv)==1 ~ "+",
                                     sign(cv)==-1 ~ "-",
                                     sign(cv)==0 ~ ".",
                                  TRUE ~ NA_character_),

             direction=paste0("LO ",directionLO,", CV ",directionCV),
             direction=case_when(directionLO=="-" ~ "Lower after venesection",
                                 directionLO=="+" ~ "Higher after venesection",
                                    TRUE ~ "No change")
  )  %>%
  left_join(datmeta, by='measurement')




# datwide<-patdat %>%
#   left_join( datlong %>% filter(stage=='pre') %>% select(-stage) %>% setNames(c(names(.)[1], paste0(names(.)[-1],"_pre"))) ) %>%
#   left_join( datlong %>% filter(stage=='post') %>% select(-stage) %>% setNames(c(names(.)[1], paste0(names(.)[-1],"_pst"))) ) %>%
#   left_join( datlong %>% group_by(ID) %>% select(-stage) %>%
#                summarise_each(funs(last(.)-first(.))) %>% setNames(c(names(.)[1], paste0(names(.)[-1],"_dif"))) ) %>%
#   left_join( datlong %>% group_by(ID) %>% select(-stage) %>%
#                summarise_each(funs(mean(.,na.rm=FALSE))) %>% setNames(c(names(.)[1], paste0(names(.)[-1],"_ave"))) ) %>%
#   left_join( datlong %>% group_by(ID) %>% select(-stage) %>%
#                summarise_each(funs(100*(last(.)-first(.))/mean(.,na.rm=FALSE))) %>% setNames(c(names(.)[1], paste0(names(.)[-1],"_difnorm"))) )
#



