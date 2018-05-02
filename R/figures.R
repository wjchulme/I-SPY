
##################################################################
# make main plot comparing measures of consistencyacross variables
##################################################################

plot_cor<-ggplot(datlonger %>% filter(measurement=='PP'),aes(x=pre,y=post))+
  geom_point(size=0.7)+
  geom_abline(aes(intercept=0,slope=1), linetype='dotted',size=0.5, colour='grey')+
  #annotate("text", x = Inf, y = -Inf, label =label_i, vjust=0, hjust=1, colour='maroon',size=rel(2), alpha=0.6)+
  #labs(title=paste0(measurement_name,"   Correlation"), x='pre venesection', y='post venesection')+
  #coord_cartesian(xlim=lims_i,ylim=lims_i)+
  theme_bw(base_size=8)+theme(aspect.ratio=1)

plot_cor


cbbPalette <- c( "#0072B2", "#D55E00")
plot_abs_lo_v_cvinv<-
  datsummary %>%
  filter(measurement %ni% c("flow_time", "cycle_time", "VTI", "BPD", "BPS", "PP", "CF", "eCBV", "MAP", "HR", "IVCe", "IVCi",
                            "ECHO_SV", "ECHO_CO", "USCOM_SV", "USCOM_CO", "LIDCO_SV", "LIDCO_CO")) %>%
  ggplot(aes(x=abs(1/cv),y=abs(lo),colour=direction))+
  geom_point()+
  geom_text_repel(aes(label=measurement_short), show.legend = FALSE)+
  labs(x="Consistency of magnitude", y="Consistency of direction")+
  #coord_cartesian(xlim=c(0,Inf),ylim=c(0,Inf))+
  xlim(0,1.19)+ylim(0,2.51)+
  theme_bw(base_size = 16)+
  scale_colour_manual(name=NULL, values=cbbPalette)+
  theme(legend.justification=c(1,0), legend.position=c(1,0))#, legend.title = element_blank())
plot_abs_lo_v_cvinv


ggsave('output/figures/plot_abs_lo_v_cvinv.png',plot=plot_abs_lo_v_cvinv, width=20, height=20, unit='cm')


# how does it look psotive and negative?
plot_lo_v_cvinv<-ggplot(datsummary, aes(x=1/cv,y=lo))+
  geom_point()+
  geom_vline(xintercept=0)+
  geom_hline(yintercept=0)+
  geom_text(aes(label=measurement_short),hjust=0,vjust=0)+
  labs(x="inverse of coefficient of variation", y="log-odds")+
  theme_bw()







###########################################
# Bland-Altman plots
############################################

plotloop<-function(measurement_i){
  
  dat_i<-filter(datlonger,measurement==measurement_i) %>% filter(!is.na(ave))
  
  
  plotwidth<-7.5
  summary_i<-sumdatAll %>% filter(measurement==measurement_i)
  lims_i<-with(summary_i,c(minval,maxval))
  
  plotheightabs<-(plotwidth)*((summary_i$maxvaldif-summary_i$minvaldif)/(summary_i$maxvalmean-summary_i$minvalmean))
  measurement_name<-datsummary %>% filter(measurement==measurement_i) %>% pull(measurement_short) %>% as.character()
  nudge_abs=with(dat_i,(max(dif)-min(dif))/100)
  nudge_rel=with(dat_i,(max(difnorm)-min(difnorm))/100)
  
  
  # toprint_i<-t(rbind(summary_i,CIlimits_i)) %>% as.data.frame() %>% setNames(c("mean","LL","UL")) %>%
  #   rownames_to_column(var="stat") %>%
  #   mutate(CI= paste0(sprintf("%.3f", round(mean,roundval_i)), " (", sprintf("%.3f", round(LL,roundval_i)), ", ", sprintf("%.3f", round(UL,roundval_i)), ")"),
  #          LoALL=mean+(sqrt(n_i)*(LL-mean)),
  #          LoAUL=mean+(sqrt(n_i)*(UL-mean))
  #          #LoACI= paste0(sprintf("%.2f", round(mean,roundval_i)), " (", sprintf("%.2f", round(LoALL,roundval_i)), ", ", sprintf("%.2f", round(LoAUL,roundval_i)), ")")
  #   )
  
  # label_i<-paste0("r = ",toprint_i %>% filter(stat=='corr') %>% pull(CI),"\n ",
  #               "1/CV = ",toprint_i %>% filter(stat=='cvinv') %>% pull(CI),"\n ",
  #               "log-odds = ",toprint_i %>% filter(stat=='lo') %>% pull(CI),"\n ",
  #               "bias = ",toprint_i %>% filter(stat=='meandif') %>% pull(CI),"\n "
  #               )
  
  
  plot_cor<-ggplot(dat_i,aes(x=pre,y=post))+
    geom_point(size=0.7)+
    geom_abline(aes(intercept=0,slope=1), linetype='dotted',size=0.5, colour='grey')+
    #annotate("text", x = Inf, y = -Inf, label =label_i, vjust=0, hjust=1, colour='maroon',size=rel(2), alpha=0.6)+
    labs(title=paste0(measurement_name,"   Correlation"), x='pre venesection', y='post venesection')+
    coord_cartesian(xlim=lims_i,ylim=lims_i)+
    theme_bw(base_size=8)+theme(aspect.ratio=1)
  
  
  plot_ba_abs<-ggplot(dat_i,aes(x=ave,y=dif))+
    geom_point(size=0.5)+
    geom_abline(aes(intercept=0,slope=0),linetype='dotted',size=0.5, colour='grey')+
    geom_abline(data=summary_i, aes(intercept=meandif,slope=0),linetype='dashed', colour='red')+
    geom_abline(data=summary_i, aes(intercept=meandif_LoALL,slope=0),linetype='dashed', colour='red')+
    geom_abline(data=summary_i, aes(intercept=meandif_LoAUL,slope=0),linetype='dashed', colour='red')+
    
    coord_cartesian(ylim=with(dat_i,c(min(dif,mean(dif)-2.1*sd(dif)),max(dif,mean(dif)+2.1*sd(dif)))))+
    labs(title=paste0(measurement_name,"   Bland-Altman plot (absolute)"),  x="average", y="absolute difference")+
    
    geom_text(data=summary_i, aes(x=Inf,y=meandif+nudge_abs, label=sprintf(paste0("%.",roundval,"f"), meandif)), colour='red', hjust=1,vjust=0, size=rel(2.5))+
    geom_text(data=summary_i, aes(x=Inf,y=meandif_LoALL+nudge_abs, label=sprintf(paste0("%.",roundval,"f"), meandif_LoALL)), colour='red', hjust=1,vjust=0, size=rel(2.5))+
    geom_text(data=summary_i, aes(x=Inf,y=meandif_LoAUL+nudge_abs, label=sprintf(paste0("%.",roundval,"f"), meandif_LoAUL)), colour='red', hjust=1,vjust=0, size=rel(2.5))+
    theme_bw(base_size=8)+#theme(aspect.ratio=1)
    coord_fixed()
  
  
  
  
  
  plot_ba_rel<-ggplot(dat_i,aes(x=ave,y=difnorm))+
    geom_point(size=0.5)+
    geom_abline(aes(intercept=0,slope=0),linetype='dotted',size=0.5, colour='grey')+
    geom_abline(data=summary_i, aes(intercept=meandifnorm,slope=0),linetype='dashed', colour='red')+
    geom_abline(data=summary_i, aes(intercept=meandifnorm_LoALL,slope=0),linetype='dashed', colour='red')+
    geom_abline(data=summary_i, aes(intercept=meandifnorm_LoAUL,slope=0),linetype='dashed', colour='red')+
    
    coord_cartesian(ylim=with(dat_i,c(min(difnorm,mean(difnorm)-2.1*sd(difnorm)),max(difnorm,mean(difnorm)+2.1*sd(difnorm)))))+
    labs(title=paste0(measurement_name,"   Bland-Altman plot (relative)"), x="average", y="proportional difference (%)")+
    
    geom_text(data=summary_i, aes(x=Inf,y=meandifnorm+nudge_rel, label=sprintf("%.1f%%", meandifnorm)), colour='red', hjust=1,vjust=0, size=rel(2.5))+
    geom_text(data=summary_i, aes(x=Inf,y=meandifnorm_LoALL+nudge_rel, label=sprintf("%.1f%%", meandifnorm_LoALL)), colour='red', hjust=1,vjust=0, size=rel(2.5))+
    geom_text(data=summary_i, aes(x=Inf,y=meandifnorm_LoAUL+nudge_rel, label=sprintf("%.1f%%", meandifnorm_LoAUL)), colour='red', hjust=1,vjust=0, size=rel(2.5))+
    
    theme_bw(base_size=8)+theme(aspect.ratio=1)
  
  
  
  ggsave(file.path("output/figures",paste0(measurement_name,' plot_cor.png')),plot=plot_cor,width=plotwidth,height=plotwidth, unit='cm')
  ggsave(file.path("output/figures",paste0(measurement_name,' plot_ba_abs.png')),plot=plot_ba_abs,width=plotwidth,height=plotwidth, unit='cm')
  ggsave(file.path("output/figures",paste0(measurement_name,' plot_ba_rel.png')),plot=plot_ba_rel,width=plotwidth,height=plotwidth, unit='cm')
  
  print(c(plotwidth,plotheightabs))
  
  rm(measurement_i, nudge_rel, nudge_abs, lims_i, dat_i, summary_i)
  rm(plot_cor, plot_ba_abs, plot_ba_rel )
  
}

for (name in as.character(datsummary$measurement)){
  plotloop(name)
}


