




datwide<-datlonger %>%
  select(ID, measurement, dif) %>%
  spread(measurement, dif)

bssummary<-datwide %>%
  summarise_all(
    .funs=funs(
            n=sum(!is.na(.)),
            nchange=sum(!is.na(.) & .!=0),
            nflat=sum(.==0),
            nrose=sum(.>0),
            nfell=sum(.<0),
            meandif=mean(.),
            sddif=sd(.)
          ), na.rm=TRUE
  ) %>%
      mutate(
            cv=sddif/meandif,
            cvinv=1/cv,
            lo=log((nrose+(nflat/2))/(nfell+(nflat/2)))
            )  %>%
  left_join(datmeta, by='measurement')






boots <- datpat %>% 
  select(ID) %>%
  modelr::bootstrap(10, id="bssamp") %>%
  mutate(
    measurements = map(strap, ~left_join(as_data_frame(.x), datlonger, by="ID")),
    summarised = map(measurements, 
                     ~summarise(group_by(.x, measurement),
                                n=sum(!is.na(dif)),
                                nflat=sum(dif==0,na.rm=TRUE),
                                nrose=sum(dif>0,na.rm=TRUE),
                                nfell=sum(dif<0,na.rm=TRUE),
                                meanpre=mean(pre,na.rm=TRUE),
                                meanpost=mean(post,na.rm=TRUE),
                                corr=cor(pre,post,use='complete.obs'),
                                meandif=mean(dif,na.rm=TRUE),
                                meandifnorm=mean(difnorm,na.rm=TRUE),
                                sddif=sd(dif,na.rm=TRUE),
                                cvinv=meandif/sddif,
                                lo=log((nrose+(nflat/2))/(nfell+(nflat/2))),
                                )
                    )
    summarised2 = map(strap,
  )

test <- boots %>% unnest(summarised)



mutate(models = map(strap, ~lm(y ~ x, data = .x) ),
       results = map(models, broom::tidy))

stats <- map(boots$strap, )







#############################
#bootstrapping
#############################


bootfunc<-function(data, indices){
  #take single measurement only
  
  bootdata<-data[indices,]
  bootsum<-bootdata %>%
    summarise(n=sum(!is.na(dif)),
              nflat=sum(dif==0,na.rm=TRUE),
              nrose=sum(dif>0,na.rm=TRUE),
              nfell=sum(dif<0,na.rm=TRUE),
              corr=cor(pre,post,use='complete.obs'),
              meandif=mean(dif,na.rm=TRUE),
              meandifnorm=mean(difnorm,na.rm=TRUE),
              sddif=sd(dif,na.rm=TRUE),
              cv=sddif/meandif,
              cvinv=1/cv,
              lo=log((nrose+(nflat/2))/(nfell+(nflat/2)))
    ) %>%
    select(corr,meandif,meandifnorm,sddif,cv,cvinv,lo) %>%
    gather('statistic','value')
  
  statistics<- setNames(bootsum %>% pull(value), bootsum %>% pull(statistic))
  
  return(statistics)
}

###########################################
#bootstrap and get 0.025 0.975 quantiles
############################################

bootprocess<-function(measurement){
  
  measurement_i<-measurement
  dat_i<-datlonger %>% filter(measurement==measurement_i) %>%  filter(!is.na(ave))
  
  n_i<-nrow(dat_i)
  boot_i<-boot(data=dat_i, statistic=bootfunc, R=10000)
  stack_i<-boot_i$t
  colnames(stack_i) <- names(bootfunc(dat_i,1:n_i))
  stack_i<-as.data.frame(stack_i) %>% mutate(measurement=measurement_i)
  
  return(stack_i)
}

boot_list <- sapply(as.character(sumdat$measurement), bootprocess, simplify=FALSE, USE.NAMES=TRUE)
stack_boot <- as.data.frame(do.call(rbind, boot_list))

saveRDS(stack_boot, file='data/stack_boot1.rds')
stack_boot <- readRDS(file='data/stack_boot1.rds')


sumdatLimits<-stack_boot %>%
  gather(statistic, value, -measurement) %>%
  group_by(measurement,statistic) %>%
  summarise(LL=quantile(value,p=0.025,names=FALSE),
            UL=quantile(value,p=0.975,names=FALSE)
  ) %>%
  gather(limit,value,-measurement,-statistic) %>%
  unite('temp',statistic,limit, sep = "_") %>%
  spread(temp, value)


#merge with summary statistics###########
sumdatAll<-left_join(datsummary,sumdatLimits) %>%
  mutate(
    meandif_LoALL=meandif+(sqrt(n)*(meandif_LL-meandif)),
    meandif_LoAUL=meandif+(sqrt(n)*(meandif_UL-meandif)),
    meandifnorm_LoALL=meandifnorm+(sqrt(n)*(meandifnorm_LL-meandifnorm)),
    meandifnorm_LoAUL=meandifnorm+(sqrt(n)*(meandifnorm_UL-meandifnorm)),
    
    roundval=round(-log(abs(sddif),10),0)+3,
    # corr_CI=paste0(sprintf("%.3f", corr), " (", sprintf("%.3f", corr_LL), ", ", sprintf("%.3f", corr_UL), ")"),
    # cv_CI=paste0(sprintf("%.2f", cv), " (", sprintf("%.2f", cv_LL), ", ", sprintf("%.2f", cv_UL), ")"),
    # cvinv_CI=paste0(sprintf("%.3f", cvinv), " (", sprintf("%.3f", cvinv_LL), ", ", sprintf("%.3f", cvinv_UL), ")"),
    # lo_CI=paste0(sprintf("%.3f", lo), " (", sprintf("%.3f", lo_LL), ", ", sprintf("%.3f", lo_UL), ")"),
    # meandif_CI=paste0(sprintf(paste0("%.",roundval,"f"), meandif), " (", sprintf(paste0("%.",roundval,"f"), meandif_LL), ", ", sprintf(paste0("%.",roundval,"f"), meandif_UL), ")"),
    # meandifnorm_CI=paste0(sprintf("%.1f", meandifnorm), "% (", sprintf("%.1f", meandifnorm_LL), ", ", sprintf("%.1f", meandifnorm_UL), ")")
    
    roundval=round(-log(abs(sddif),10),0)+3,
    corr_CI=print_est2bracket(corr, corr_LL, corr_UL, 3),
    cv_CI=print_est2bracket(cv, cv_LL, cv_UL, 2),
    cvinv_CI=print_est2bracket(cvinv, cvinv_LL, cvinv_UL, 3),
    lo_CI=print_est2bracket(lo, lo_LL, lo_UL, 3),
    meandif_CI=print_est2bracket(meandif, meandif_LL, meandif_UL, roundval, 1),
    meandifnorm_CI=print_est2bracket(meandifnorm, meandifnorm_LL, meandifnorm_UL, 1)
    
  )


###########################################
#make table
############################################

tableout<-sumdatAll %>%
  mutate(meanpre=round(meanpre,roundval),
         meanpost=round(meanpost,roundval),
         meandifnorm=paste0(round(meandifnorm,1), "%"),
         LoAabs=paste0("(",sprintf(paste0("%.",roundval,"f"), meandif_LoALL),",  ",sprintf(paste0("%.",roundval,"f"), meandif_LoAUL),")"),
         LoArel=paste0("(",sprintf("%.1f", meandifnorm_LoALL),",  ",sprintf("%.1f", meandifnorm_LoAUL),")")
  ) %>%
  select('Measurement'=measurement_short,
         'N (excl. missing)'=n,
         'n increased'=nrose,
         'n decreased'=nfell,
         'n no change'=nflat,
         'pre-venesection mean'=meanpre,
         'post-venesection mean'=meanpost,
         'bias (absolute)'=meandif_CI,
         'bias (relative)'=meandifnorm_CI,
         'Correlation'=corr_CI,
         'Coefficient of variation (CV)'=cv_CI,
         'inverse CV'=cvinv_CI,
         'log-odds'=lo_CI,
         'limits of agreement (absolute)'=LoAabs,
         'limits of agreement % (relative)'=LoArel
  )

tableoutlong <- tableout %>%
  gather('statistic','value',-Measurement) %>%
  arrange(Measurement) %>%
  mutate(text=paste0(statistic,":  ",value))





write.csv(tableout, "output/tables/summary.csv", row.names=FALSE, na="")
write.csv(tableoutlong, "output/tables/summary long.csv", row.names=FALSE, na="")
