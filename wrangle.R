#if(getwd()=="/home/ubuntu"){setwd("/home/ubuntu/rivercreeper")}
#
# plotly config options
#https://www.rdocumentation.org/packages/plotly/versions/2.0.16/topics/config

#options(rsconnect.max.bundle.size=1*10^9)

library(plyr)
library(shiny);library(ggvis)
library(dplyr);#library(leaflet)
#library(leaflet.extras);
library(data.table)

library(janitor);library(lubridate)
#library(readxl);
library(tidyverse);#library(devtools)
library(ggplot2);library(reshape2)
library(quantmod)
library(scales)
#library(TTR)
library(plotly)
library(DT)
library(janitor)
library(lme4)

library(leaflet)
library(leaflet.extras)

##################


#sites<-fread("nla2012_wide_siteinfo_08232016.txt" ,colClasses=colclasses_sites) %>% as.data.frame()

sites0<-fread("nla2012_wide_siteinfo_08232016.txt") %>% as.data.frame()
keyvars0<-fread("nla12_keyvariables_data.txt") %>% as.data.frame()
profiles0<-fread("nla2012_wide_profile_08232016.txt") %>% as.data.frame()
algaltoxins0<-fread("nla2012_algaltoxins_08192016.txt") %>% as.data.frame()
waterchem0<-fread("nla2012_waterchem_wide.txt") %>% as.data.frame()

chla0<-fread("nla2012_chla_wide.txt") %>% as.data.frame()
#secchi0<-fread("nla2012_secchi_08232016.txt") %>% as.data.frame()
secchi0<-read.csv("nla2012_secchi_08232016.txt") %>% as.data.frame()
benth0<-fread("nla2012_wide_benthic_08232016.txt") %>% as.data.frame()
watershed0<-fread("nla2012_wide_watershed.txt") %>% as.data.frame()

zoopmets0<-fread("nla2012_zoopmets_08192016.txt") %>% as.data.frame()
zoopcts0<-fread("nla2012_zoopcnt_04032014.txt") %>% as.data.frame()
benthmets0<-fread("nla2012_bentmet.txt") %>% as.data.frame()
phytocts0<-fread("nla2012_wide_phytoplankton_count_02122014.txt") %>% as.data.frame()

sites<-clean_names(sites0)
keyvars<-clean_names(keyvars0)
profiles<-clean_names(profiles0)
algaltoxins<-clean_names(algaltoxins0)
waterchem<-clean_names(waterchem0)

chla<-clean_names(chla0)
secchi<-clean_names(secchi0)
benth<-clean_names(benth0)
watershed<-clean_names(watershed0)

zoopmets<-clean_names(zoopmets0)
zoopcts<-clean_names(zoopcts0)
benthmets<-clean_names(benthmets0)
phytocts<-clean_names(phytocts0)


keyvars_unitsprep<-names(keyvars)[grep("units",names(keyvars))]
keyvars_unitsprep2<-keyvars %>% select(keyvars_unitsprep) %>% unique()
names(keyvars_unitsprep2)<-substr(names(keyvars_unitsprep2),1,-6+nchar(names(keyvars_unitsprep2)))
keyvars_unitsprep3<-keyvars_unitsprep2 %>% t() %>% as.data.frame()
keyvars_unitsprep3$variable<-row.names(keyvars_unitsprep3)
keyvars_unitsprep3$units<-as.character(keyvars_unitsprep3[,1])
if(sum(is.na(keyvars_unitsprep3$units)>0)){
  keyvars_unitsprep3$units[which(is.na(keyvars_unitsprep3$units)==TRUE)]<-as.character(keyvars_unitsprep3[,2])[which(is.na(keyvars_unitsprep3$units)==TRUE)]
}
if(sum(is.na(keyvars_unitsprep3$units)>0)){
  keyvars_unitsprep3$units[which(is.na(keyvars_unitsprep3$units)==TRUE)]<-as.character(keyvars_unitsprep3[,3])[which(is.na(keyvars_unitsprep3$units)==TRUE)]
}
if(sum(is.na(keyvars_unitsprep3$units)>0)){
  keyvars_unitsprep3$units[which(is.na(keyvars_unitsprep3$units)==TRUE)]<-as.character(keyvars_unitsprep3[,4])[which(is.na(keyvars_unitsprep3$units)==TRUE)]
}
keyvars_units<-keyvars_unitsprep3 %>% select(variable,units) %>% data.frame(row.names=NULL)

waterchem_unitsprep<-names(waterchem)[grep("units",names(waterchem))]
waterchem_unitsprep2<-waterchem %>% select(waterchem_unitsprep) %>% unique()
names(waterchem_unitsprep2)<-substr(names(waterchem_unitsprep2),1,-6+nchar(names(waterchem_unitsprep2)))
waterchem_unitsprep3<-waterchem_unitsprep2 %>% t() %>% as.data.frame()
waterchem_unitsprep3$variable<-row.names(waterchem_unitsprep3)
waterchem_unitsprep3$units<-as.character(waterchem_unitsprep3[,1])
if(sum(is.na(waterchem_unitsprep3$units), as.numeric(waterchem_unitsprep3$units=="")) >0){
  waterchem_unitsprep3$units[which(is.na(waterchem_unitsprep3$units)==TRUE | waterchem_unitsprep3$units=="")]<-as.character(waterchem_unitsprep3[,2])[which(is.na(waterchem_unitsprep3$units)==TRUE | waterchem_unitsprep3$units=="")]
}
if(sum(is.na(waterchem_unitsprep3$units), as.numeric(waterchem_unitsprep3$units=="")) >0){
  waterchem_unitsprep3$units[which(is.na(waterchem_unitsprep3$units)==TRUE | waterchem_unitsprep3$units=="")]<-as.character(waterchem_unitsprep3[,3])[which(is.na(waterchem_unitsprep3$units)==TRUE | waterchem_unitsprep3$units=="")]
}
if(sum(is.na(waterchem_unitsprep3$units), as.numeric(waterchem_unitsprep3$units=="")) >0){
  waterchem_unitsprep3$units[which(is.na(waterchem_unitsprep3$units)==TRUE | waterchem_unitsprep3$units=="")]<-as.character(waterchem_unitsprep3[,4])[which(is.na(waterchem_unitsprep3$units)==TRUE | waterchem_unitsprep3$units=="")]
}
if(sum(is.na(waterchem_unitsprep3$units), as.numeric(waterchem_unitsprep3$units=="")) >0){
  waterchem_unitsprep3$units[which(is.na(waterchem_unitsprep3$units)==TRUE | waterchem_unitsprep3$units=="")]<-as.character(waterchem_unitsprep3[,5])[which(is.na(waterchem_unitsprep3$units)==TRUE | waterchem_unitsprep3$units=="")]
}
if(sum(is.na(waterchem_unitsprep3$units), as.numeric(waterchem_unitsprep3$units=="")) >0){
  waterchem_unitsprep3$units[which(is.na(waterchem_unitsprep3$units)==TRUE | waterchem_unitsprep3$units=="")]<-as.character(waterchem_unitsprep3[,6])[which(is.na(waterchem_unitsprep3$units)==TRUE | waterchem_unitsprep3$units=="")]
}
if(sum(is.na(waterchem_unitsprep3$units), as.numeric(waterchem_unitsprep3$units=="")) >0){
  waterchem_unitsprep3$units[which(is.na(waterchem_unitsprep3$units)==TRUE | waterchem_unitsprep3$units=="")]<-as.character(waterchem_unitsprep3[,7])[which(is.na(waterchem_unitsprep3$units)==TRUE | waterchem_unitsprep3$units=="")]
}
if(sum(is.na(waterchem_unitsprep3$units), as.numeric(waterchem_unitsprep3$units=="")) >0){
  waterchem_unitsprep3$units[which(is.na(waterchem_unitsprep3$units)==TRUE | waterchem_unitsprep3$units=="")]<-as.character(waterchem_unitsprep3[,8])[which(is.na(waterchem_unitsprep3$units)==TRUE | waterchem_unitsprep3$units=="")]
}
waterchem_units<-waterchem_unitsprep3 %>% select(variable,units) %>% data.frame(row.names=NULL)

units<-rbind(keyvars_units,waterchem_units)

#secondvis0<-keyvars %>% filter(visit_no>1) %>% select(uid) %>% unique()
#secondvis<-sites %>% select(uid %in% secondvis0)

profiles$temperature[which(profiles$temperature>50)]<-NA
#waterchem$sulfate_result[which(waterchem$sulfate_result>10000)]<-NA
maxdepths<-profiles[-which(is.na(profiles$index_site_depth)==TRUE),] %>% select(site_id,depth,index_site_depth) %>% group_by(site_id) %>% dplyr::summarize(depthmax=max(index_site_depth,na.rm=TRUE)) %>% as.data.frame()
maxdepths<-na.omit(maxdepths)
#maxdepths<-profiles %>% select(site_id,depth) %>% group_by(site_id) %>% dplyr::summarize(maxdepth=max(depth,na.rm=TRUE)) %>% as.data.frame()
#maxdepths<-
#  profiles %>% select(site_id,depth,index_site_depth) %>% group_by(site_id) %>% dplyr::summarize(maxdepth=max(depth,na.rm=TRUE),
#                                                                                maxdepth2=max(index_site_depth,na.rm=TRUE)) %>% as.data.frame()


sites_trim0<-sites %>% select(site_id,uid,date=date_col,name=eval_name,area_ha,elevation,lat=lat_dd83,lon=lon_dd83,manmade_natural=lake_origin,lake_origin12,state,aggr_eco3_2015,aggr_eco9_2015,epa_reg,regionL1=fw_eco3,region=fw_eco9,eco_bio,cntyname)
sites_trim0<-sites_trim0 %>% arrange(site_id,date) %>% as.data.frame()

# drop the subset of sites and dates that were revisits, keep first visit only
sites_trim<-sites_trim0[which(duplicated(sites_trim0$site_id)==FALSE),]
sites_trim$name[which(sites_trim$name=="")]<-"unnamed"

sites_trim$name<-paste(sites_trim$name," (",substr(sites_trim$site_id,-2+nchar(sites_trim$site_id),nchar(sites_trim$site_id)),")",sep="")

#sites_trim$name<-paste(sites_trim$name,round(sites_trim$lat,digits=3))

#sites_trim %>% select(site_id,name,state) %>% group_by(name,state) %>%
#  dplyr::summarize(name=pas)

#sites_trim[which(duplicated(sites_trim$name)==FALSE),]

#sites_trim0 %>% select(site_id,date) %>% group_by(site_id) %>%
#  dplyr::summarize(mindate=min(date,na.rm=TRUE)) %>% as.data.frame()

#sites_trim0 %>% select(site_id,uid) %>% group_by(site_id) %>%
#  dplyr::summarize(mindate=min(uid,na.rm=TRUE)) %>% as.data.frame()

#                              AGGR_ECO3_2015,AGGR_ECO9_2015)
#firsts<-sites_trim %>% dplyr::summarize(firsts=first(value))


# no conductivity in profiles (?)


#keyvars
#keyvars_names<-c()
keyvars_names<-sort(names(keyvars)[grep("result",names(keyvars))])
keyvars_trim<-melt(keyvars %>% select(site_id,uid,keyvars_names),id.vars=c("site_id","uid")) %>% na.omit()
keyvars<-merge(sites_trim,keyvars_trim,by=c("site_id","uid"))
keyvars$depth<-""
keyvars<-keyvars %>% select(site_id,depth,variable,value)
keyvars$variable<-as.character(keyvars$variable)
keyvars$variable<-substr(keyvars$variable,1,-7+nchar(keyvars$variable))
keyvars<-keyvars %>% filter(variable %in% c("atrazine","methylmercury","totalhg"))

################################

profiles_trim<-melt(profiles %>% select(site_id,depth,temperature,oxygen,ph,conductivity),id.vars=c("site_id","depth")) %>% na.omit()
#names(profiles_trim)[which(names(profiles_trim)=="temperature")]<-"Water temperature (degrees C)"

#daterr$variable0<-daterr$variable
#daterr$variable[which(daterr$variable=="oxygen")]<-"Oxygen (mg/L)"
#daterr$variable[which(daterr$variable=="temperature")]<-"Water temperature (degrees C)"
#daterr$variable[which(daterr$variable=="ph")]<-"pH"



profiles_trim_summ<-profiles_trim %>% group_by(site_id,depth,variable) %>% 
  dplyr::summarize(value=mean(value,na.rm=TRUE)) %>% as.data.frame()


profiles_top2m<-profiles_trim %>% filter(depth<=2) %>% group_by(site_id) %>%
  dplyr::summarize(temperature=mean(value[which(variable=="temperature")],na.rm=TRUE),
                   oxygen=mean(value[which(variable=="oxygen")],na.rm=TRUE),
                   ph=mean(value[which(variable=="ph")],na.rm=TRUE)) %>% mutate(depth="top2m") %>% melt(id.vars=c("site_id","depth"))
profiles_2to4m<-profiles_trim %>% filter(depth>2 & depth<=4) %>% group_by(site_id) %>%
  dplyr::summarize(temperature=mean(value[which(variable=="temperature")],na.rm=TRUE),
                   oxygen=mean(value[which(variable=="oxygen")],na.rm=TRUE),
                   ph=mean(value[which(variable=="ph")],na.rm=TRUE)) %>% mutate(depth="2to4m") %>% melt(id.vars=c("site_id","depth"))
profiles_below4m<-profiles_trim %>% filter(depth>4) %>% group_by(site_id) %>%
  dplyr::summarize(temperature=mean(value[which(variable=="temperature")],na.rm=TRUE),
                   oxygen=mean(value[which(variable=="oxygen")],na.rm=TRUE),
                   ph=mean(value[which(variable=="ph")],na.rm=TRUE)) %>% mutate(depth="below4m") %>% melt(id.vars=c("site_id","depth"))
profiles_below5m<-profiles_trim %>% filter(depth>5) %>% group_by(site_id) %>%
  dplyr::summarize(temperature=mean(value[which(variable=="temperature")],na.rm=TRUE),
                   oxygen=mean(value[which(variable=="oxygen")],na.rm=TRUE),
                   ph=mean(value[which(variable=="ph")],na.rm=TRUE)) %>% mutate(depth="below5m") %>% melt(id.vars=c("site_id","depth"))
profiles<-rbind(profiles_top2m,profiles_2to4m,profiles_below4m,profiles_below5m)

profiles_trim_summ$variable<-as.character(profiles_trim_summ$variable)
profiles_trim_summ$variable[which(profiles_trim_summ$variable=="temperature")]<-"temperature (C)"
profiles_trim_summ$variable[which(profiles_trim_summ$variable=="oxygen")]<-"oxygen (mg/L)"
profiles_trim_summ$variable[which(profiles_trim_summ$variable=="ph")]<-"ph"
profiles_trim_summ$variable[which(profiles_trim_summ$variable=="conductivity")]<-"conductivity (uS)"
profiles_trim_summ$variable<-as.factor(profiles_trim_summ$variable)

#profiles<-profiles(merge(profiles,maxdepths,by="site_id"))

#algaltoxins0<-fread("nla2012_algaltoxins_08192016.txt") %>% as.data.frame()
#algaltoxins1<-clean_names(algaltoxins0)
#algaltoxins1$depth<-""
algaltoxins$depth<-""
algaltoxins<-melt(algaltoxins %>% select(site_id,depth,micl_result,micx_result),id.vars=c("site_id","depth")) %>% na.omit()
algaltoxins<-algaltoxins

waterchem_names<-sort(names(waterchem)[grep("result",names(waterchem))])
waterchem_units<-sort(names(waterchem)[grep("units",names(waterchem))])
waterchem_trim<-melt(waterchem %>% select(uid,waterchem_names),id.vars=c("uid")) %>% na.omit()
waterchem_units_trim<-melt(waterchem %>% select(uid,waterchem_units),id.vars=c("uid")) %>% na.omit()
#check waterchem units
waterchem_units_trim %>% select(variable,value) %>% unique()

waterchem_trim$variable<-substr(waterchem_trim$variable,1,-7+nchar(as.character(waterchem_trim$variable)))
waterchem<-waterchem_trim
waterchem<-merge(sites_trim,waterchem,by="uid")
waterchem$depth<-""
waterchem<-waterchem %>% select(site_id,depth,variable,value)

#profiles<-merge(sites_trim,profiles_trim,by="site_id")

chla$variable<-"chla"
chla_trim<-chla %>% select(uid,variable,value=chlx_result)
chla<-merge(sites_trim,chla_trim,by="uid")
chla$depth<-""
chla<-chla %>% select(site_id,depth,variable,value)

secchi$variable<-"secchi"
secchi_trim<-secchi %>% select(uid,variable,value=secchi)
secchi<-merge(sites_trim,secchi_trim,by="uid")
secchi$depth<-""
secchi<-secchi %>% select(site_id,depth,variable,value)

###############################

#benth
#benth %>% filter(target_taxon=="DREISSENA") %>% as.data.frame()

###############################

watershed_names<-c("damcnt_bsn","damden_bsn","elevmean_bsn","elevmax_bsn",
                   "houseden_bsn","houseden_1000",
                   "nadp_totaln_bsn","nadp_no3_bsn","nadp_nh4_bsn",
                   "nlcd2006_11area_bsn","nlcd2006_12area_bsn",
                   "nlcd2006_22area_bsn","nlcd2006_23area_bsn","nlcd2006_24area_bsn",
                   "nlcd2006_41area_bsn","nlcd2006_42area_bsn","nlcd2006_43area_bsn",
                   "nlcd2006_82area_bsn",
                   "nlcd2006_90area_bsn","nlcd2006_95area_bsn"
)
watershed_names<-c("damcnt_bsn","damden_bsn","elevmean_bsn","elevmax_bsn",
                   "houseden_bsn","houseden_1000",
                   "nadp_totaln_bsn","nadp_no3_bsn","nadp_nh4_bsn",
                   "nlcd2006_11pct_bsn","nlcd2006_12pct_bsn",
                   "nlcd2006_22pct_bsn","nlcd2006_23pct_bsn","nlcd2006_24pct_bsn",
                   "nlcd2006_41pct_bsn","nlcd2006_42pct_bsn","nlcd2006_43pct_bsn",
                   "nlcd2006_82pct_bsn",
                   "nlcd2006_90pct_bsn","nlcd2006_95pct_bsn"
)
watershed<-watershed %>% select(site_id,watershed_names) %>% as.data.frame()
watershed$nlcd2006_22_23_24pct_bsn<- watershed$nlcd2006_22pct_bsn + watershed$nlcd2006_23pct_bsn + watershed$nlcd2006_24pct_bsn
watershed$nlcd2006_41_42_43pct_bsn<- watershed$nlcd2006_41pct_bsn + watershed$nlcd2006_42pct_bsn + watershed$nlcd2006_43pct_bsn
watershed$nlcd2006_90_95pct_bsn<- watershed$nlcd2006_90pct_bsn + watershed$nlcd2006_95pct_bsn 
watershed_names<-c(watershed_names,
                   "nlcd2006_22_23_24pct_bsn","nlcd2006_41_42_43pct_bsn","nlcd2006_90_95pct_bsn")

watershed_trim<-melt(watershed %>% select(site_id,watershed_names),id.vars=c("site_id")) %>% na.omit()
watershed_trim$depth<-""

watershed<-merge(sites_trim,watershed_trim,by="site_id")
watershed<-watershed %>% select(site_id,depth,variable,value) %>% arrange(site_id,variable)
watershed$variable<-as.character(watershed$variable)
watershed$variable[which(watershed$variable=="nlcd2006_11pct_bsn")]<-"nlcd2006_openwater_pct_bsn"
watershed$variable[which(watershed$variable=="nlcd2006_22_23_24pct_bsn")]<-"nlcd2006_developed_pct_bsn"
watershed$variable[which(watershed$variable=="nlcd2006_41_42_43pct_bsn")]<-"nlcd2006_forest_pct_bsn"
watershed$variable[which(watershed$variable=="nlcd2006_82pct_bsn")]<-"nlcd2006_cropland_pct_bsn"
watershed$variable[which(watershed$variable=="nlcd2006_90_95pct_bsn")]<-"nlcd2006_wetland_pct_bsn"
watershed<-watershed %>% filter(!variable %in% c("nlcd2006_12pct_bsn",
                                                 "nlcd2006_22pct_bsn","nlcd2006_23pct_bsn","nlcd2006_24pct_bsn",
                                                 "nlcd2006_41pct_bsn","nlcd2006_42pct_bsn","nlcd2006_43pct_bsn",
                                                 "nlcd2006_90pct_bsn","nlcd2006_95pct_bsn"))

#melt(chla %>% select(uid,waterchem_names),id.vars=c("uid")) %>% na.omit()

####################
#zoops, benthic macroinverts
zoopmets_names<-names(zoopmets)[8:216]
zoopmets_trim<-melt(zoopmets %>% select(site_id,zoopmets_names),id.vars=c("site_id")) %>% na.omit()
zoopmets_trim$depth<-""
zoopmets_trim$variable<-paste("zp_",zoopmets_trim$variable,sep="")
zoopmets<-zoopmets_trim %>% select(site_id,depth,variable,value) %>% as.data.frame()

zoopcts_names<-c("density","biomass")
zoopcts_trim<-melt(zoopcts %>% select(site_id,zoopcts_names,target_taxon),id.vars=c("site_id","target_taxon")) %>% na.omit()
zoopcts_trim$depth<-""
zoopcts_trim$variable<-paste("z_zoop_",zoopcts_trim$variable,"_",zoopcts_trim$target_taxon,sep="")
zoopcts<-zoopcts_trim %>% select(site_id,depth,variable,value) %>% as.data.frame()

benthmets_names<-names(benthmets)[9:175]
benthmets_names<-benthmets_names[-grep("tl",benthmets_names)]
benthmets_names<-benthmets_names[-grep("tol",benthmets_names)]
benthmets_trim<-melt(benthmets %>% select(site_id,benthmets_names),id.vars=c("site_id")) %>% na.omit()
benthmets_trim$depth<-""
benthmets_trim$variable<-paste("z_benth_",benthmets_trim$variable,sep="")
benthmets<-benthmets_trim %>% select(site_id,depth,variable,value) %>% as.data.frame()


phytocts_names<-c("density","biovolume")
phytocts$target_taxon2<-paste(phytocts$target_taxon,"_",phytocts$PHYLUM,"_",
                              phytocts$CLASS,"_",phytocts$ORDER,"_",phytocts$FAMILY,
                              phytocts$GENUS,"_",phytocts$SPECIES,"_",sep="")
phytocts_trim<-melt(phytocts %>% select(site_id,phytocts_names,target_taxon),id.vars=c("site_id","target_taxon")) %>% na.omit()
phytocts_trim$depth<-""
phytocts_trim$variable<-paste("z_phyto_",phytocts_trim$variable,"_",phytocts_trim$target_taxon,sep="")
phytocts<-phytocts_trim %>% select(site_id,depth,variable,value) %>% as.data.frame()

phytocts_sitect<-phytocts %>% select(site_id,variable,value) %>% group_by(variable) %>% dplyr::summarize(ct=length(value)) %>% as.data.frame()
phytocts_sitect_use<-phytocts_sitect[which(phytocts_sitect$ct>200),]$variable
phytocts<-phytocts %>% filter(variable %in% phytocts_sitect_use) %>% as.data.frame()

#############################
# remove duplicated
zoopmets<-zoopmets[which(duplicated(zoopmets)==FALSE),]
benthmets<-benthmets[which(duplicated(benthmets)==FALSE),]
phytocts<-phytocts[which(duplicated(phytocts)==FALSE),]
zoopcts<-zoopcts[which(duplicated(zoopcts)==FALSE),]

#zoopmets<-zoopmets %>% filter(variable=="zp_asplan300_ptax")
#zoopmets<-zoopmets %>% filter(variable=="zp_asplan300_ptax")
zoopmets_use<-unique(zoopmets$variable)[-grep("300|nat",unique(zoopmets$variable))]
zoopmets_use<-c(zoopmets_use,"zp_totl300_bio","zp_zocn300_bio","zp_zofn_bio")
#TOTL300_BIO
#ZOCN300_BIO
#ZOFN_BIO

#zoopcts_sum<-zoopcts %>% group_by(site_id) %>% dplyr::summarize(value=sum(value))
#zoopcts_sum$variable<-"zoop_density_total"
#zoopcts_sum$depth<-""
#zoopcts_sum<-zoopcts_sum %>% select(site_id,depth,variable,value)
#zoopcts<-rbind(zoopcts,zoopcts_sum)

#zoopmets_use<-zoopmets_use[1:10] # worked
#zoopmets_use<-zoopmets_use[1:20] # worked
#zoopmets_use<-zoopmets_use[1:30] #worked with 30, on 3 Oct after splitting data into other repo

#benthmets_use<-unique(benthmets$variable)[-grep("300|nat",unique(benthpmets$variable))]
benthmets_use<-unique(benthmets$variable)
#benthmets_use<-benthmets_use[1:30] #worked with 30, on 3 Oct after splitting data into other repo

phytocts_use<-unique(phytocts$variable)
#phytocts_use<-phytocts_use[1:30] #worked with 30, on 3 Oct after splitting data into other repo

zoopmets<- zoopmets %>% filter(variable %in% zoopmets_use)
benthmets<- benthmets %>% filter(variable %in% benthmets_use)
phytocts<- phytocts %>% filter(variable %in% phytocts_use)


##########################

daterr0<-rbind(profiles,algaltoxins,waterchem,
               chla,secchi,keyvars,
               watershed,zoopmets,benthmets,
               phytocts,zoopcts)#,benthmets,phytocts)#,zoopcts)
daterr0$variable<-paste(daterr0$variable,daterr0$depth,sep="")
daterr0<-merge(daterr0,maxdepths,by="site_id")

#sites

daterr1<-merge(sites_trim,daterr0,by="site_id")


daterr_areas0<-merge(sites_trim,daterr0,by="site_id")
daterr_areas0$depth<-NA
daterr_areas0$variable<-"area_ha"
daterr_areas0$value<-NA
daterr_areas<-unique(daterr_areas0)
daterr_areas$value<-daterr_areas$area_ha

daterr_elevs0<-merge(sites_trim,daterr0,by="site_id")
daterr_elevs0$depth<-NA
daterr_elevs0$variable<-"elevation"
daterr_elevs0$value<-NA
daterr_elevs<-unique(daterr_elevs0)
daterr_elevs$value<-daterr_elevs$elevation

daterr_lats0<-merge(sites_trim,daterr0,by="site_id")
daterr_lats0$depth<-NA
daterr_lats0$variable<-"latitude "
daterr_lats0$value<-NA
daterr_lats<-unique(daterr_lats0)
daterr_lats$value<-daterr_lats$lat

daterr_maxdepths0<-merge(sites_trim,daterr0,by="site_id")
daterr_maxdepths0$variable<-"depthmax"
daterr_maxdepths0$value<-daterr_maxdepths0$depthmax
daterr_maxdepths<-unique(daterr_maxdepths0)
daterr_maxdepths$value<-daterr_maxdepths$depthmax
daterr_maxdepths$depth<-daterr_maxdepths$value
daterr_maxdepths<-unique(daterr_maxdepths)

daterr1<-rbind(daterr1,daterr_areas,daterr_elevs,daterr_lats,daterr_maxdepths)

#daterr1$area_group<-"10000+ ha"
#daterr1$area_group[which(daterr1$area_ha<10000)]<-"1000-9999 ha"
daterr1$area_group<-">1000ha"
daterr1$area_group[which(daterr1$area_ha<1000)]<-"100-999ha"
daterr1$area_group[which(daterr1$area_ha<100)]<-"10-99ha"
daterr1$area_group[which(daterr1$area_ha<10)]<-"<10ha"
#daterr1$area_group<-factor(unique(daterr1$area_group),levels=c("<10 ha", "10-99 ha", "100-999 ha", ">1000 ha"))
daterr1$area_group<-factor(daterr1$area_group,levels=c("<10ha", "10-99ha", "100-999ha", ">1000ha"))


daterr1$depth_group<-"d >10m"
daterr1$depth_group[which(daterr1$depthmax<10)]<-"c 5-9.99m"
daterr1$depth_group[which(daterr1$depthmax<5)]<-"b 2-4.99m"
daterr1$depth_group[which(daterr1$depthmax<2)]<-"a <2m"
#daterr1$depth_group<-factor(unique(daterr1$depth_group),levels=c("<2 m", "2-4.99 m", "5-9.99 m", ">10 m"))
#daterr1$depth_group<-factor(daterr1$depth_group,levels=c("<2m", "2-4.99m", "5-9.99m", ">10m"))
#daterr1$depth_group<-factor(daterr1$depth_group,levels=c("a <2m", "b 2-4.99m", "c 5-9.99m", "d >10m"))



daterr1<-daterr1 %>% select(-area_ha,-elevation, -depthmax)  


#daterr1
#daterr_all<-daterr1

#daterr_all$state<-"all"
#daterr_all$fw_eco3<-"all"
#daterr_all$fw_eco9<-"all"
#daterr_all$aggr_eco3_2015<-"all"
#daterr_all$aggr_eco9_2015<-"all"

daterr1<-merge(daterr1,units,by="variable",all=TRUE)

daterr1$units[which(daterr1$var=="area_ha")]<-"ha"
daterr1$units[which(daterr1$var=="chla")]<-"ug/L"
daterr1$units[which(daterr1$var=="damcnt_bsn")]<-"number"
daterr1$units[which(daterr1$var=="damden_bsn")]<-"number/km2"
daterr1$units[which(daterr1$var=="depthmax")]<-"m"
daterr1$units[which(daterr1$var=="elevation")]<-"m"
daterr1$units[which(daterr1$var=="elevmax_bsn")]<-"m"
daterr1$units[which(daterr1$var=="elevmean_bsn")]<-"m"
daterr1$units[which(daterr1$var=="houseden_1000")]<-"units/mile2"
daterr1$units[which(daterr1$var=="houseden_bsn")]<-"units/mile2"
daterr1$units[which(daterr1$var=="latitude ")]<-"degrees N"
daterr1$units[which(daterr1$var=="micl_result")]<-"ug/L"
daterr1$units[which(daterr1$var=="micx_result")]<-"ug/L"
#daterr1$units[which(daterr1$var=="micl_result")]<-"NA"
#daterr1$units[which(daterr1$var=="micx_result")]<-"NA"

daterr1$units[which(daterr1$var=="nadp_nh4_bsn")]<-"kg/km2"
daterr1$units[which(daterr1$var=="nadp_no3_bsn")]<-"kg/km2"
daterr1$units[which(daterr1$var=="nadp_totaln_bsn")]<-"kg/km2"
daterr1$units[grep("pct_bsn",daterr1$var)]<-"%"
daterr1$units[which(daterr1$var=="oxygentop2m")]<-"mg/L"
daterr1$units[which(daterr1$var=="oxygen2to4m")]<-"mg/L"
daterr1$units[which(daterr1$var=="oxygenbelow4m")]<-"mg/L"
daterr1$units[which(daterr1$var=="oxygenbelow5m")]<-"mg/L"
daterr1$units[which(daterr1$var=="ph")]<-"ph"
daterr1$units[which(daterr1$var=="phtop2m")]<-"ph units"
daterr1$units[which(daterr1$var=="ph2to4m")]<-"ph units"
daterr1$units[which(daterr1$var=="phbelow4m")]<-"ph units"
daterr1$units[which(daterr1$var=="phbelow5m")]<-"ph units"
daterr1$units[which(daterr1$var=="secchi")]<-"m"
daterr1$units[which(daterr1$var=="temperaturetop2m")]<-"C"
daterr1$units[which(daterr1$var=="temperature2to4m")]<-"C"
daterr1$units[which(daterr1$var=="temperaturebelow4m")]<-"C"
daterr1$units[which(daterr1$var=="temperaturebelow5m")]<-"C"

daterr1$units[grep("pind",daterr1$var)]<-"% of individuals present"
daterr1$units[grep("ptax",daterr1$var)]<-"% of species present"
daterr1$units[grep("pbio",daterr1$var)]<-"% of biomass"
daterr1$units[grep("_den",daterr1$var)]<-"individuals per Liter"
daterr1$units[grep("_bio",daterr1$var)]<-"biomass units"

daterr1$units[grep("_phyto_density",daterr1$var)]<-"individuals per Liter"
daterr1$units[grep("_zoop_density",daterr1$var)]<-"individuals per Liter"
daterr1$units[grep("_phyto_biovolume",daterr1$var)]<-"biomass per Liter"

daterr1$units[grep("_ntax",daterr1$var)]<-"number of taxa present"
daterr1$units[grep("_ptax",daterr1$var)]<-"% of taxa present"

#daterr1$units[grep("zoop_density_total",daterr1$var)]<-"individuals per Liter"




daterr1$variable<-paste(daterr1$variable," (",daterr1$units,")",sep="")


#daterr1$unit<-"mg/L"

daterr<-daterr1

#daterr<-rbind(daterr_all,daterr1)
daterr$group<-daterr$state


daterr<-unique(daterr)
daterr<-daterr %>% arrange(variable)


do<-0
if(do==1){

vars<-unique(daterr$variable)
#vars<-c("area_ha",unique(daterr$variable))
states<-c("all",sort(unique(daterr$state)))
#fw_eco3s<-c("all",unique(daterr$fw_eco3))
#fw_eco9s<-c("all",unique(daterr$fw_eco9))
regionL1s<-c("all",sort(unique(daterr$regionL1)))
regions<-c("all",sort(unique(daterr$region)))
#lake_origins<-c("all",unique(daterr$lake_origin))
manmade_naturals<-c("all",unique(daterr$manmade_natural))
depth_groups<-c("all",sort(unique(daterr$depth_group),decreasing=TRUE))
aggr_eco3_2015s<-unique(daterr$aggr_eco3_2015)
aggr_eco9_2015s<-unique(daterr$aggr_eco9_2015)


#daterr$variable0<-daterr$variable
#daterr$variable[which(daterr$variable=="oxygen")]<-"Oxygen (mg/L)"
#daterr$variable[which(daterr$variable=="temperature")]<-"Water temperature (degrees C)"
#daterr$variable[which(daterr$variable=="ph")]<-"pH"

#categoricals<-c("none","state","fw_eco3","fw_eco9","lake_origin")#,"lake_origin12")
#categoricals<-c("none","state","regionL1","regionL2","lake_origin")#,"lake_origin12")
#categoricals<-c("none","state","regionL1","regionL2","manmade_natural")#,"lake_origin12")
categoricals<-c("none","state","region","manmade_natural","area_group","depth_group")#,"lake_origin12")


daterr_wide<-daterr %>% select(-depth,-units) %>% pivot_wider(names_from = variable, values_from = value, values_fn=mean) %>% as.data.frame()
#daterr_wide<-merge(daterr %>% select(site_id,area_ha,elevation,lat) %>% unique(),daterr_wide0,by="site_id")
daterr_wide$X<-daterr_wide$'temperaturetop2m (C)'
daterr_wide$Y<-daterr_wide$'oxygentop2m (mg/L)'
#daterr_wide$X<-daterr_wide$temperaturetop1m
#daterr_wide$Y<-daterr_wide$oxygentop1m
#daterr_wide$group<-daterr_wide$lake_origin
daterr_wide$group<-daterr_wide$manmade_natural
# map paramaters
zoomstart<-9
#pointradius<-5000
pointradius<-2000
}

daterr_profiles0<-daterr %>% select(-variable,-value,-depth,-units) %>% unique()
daterr_profiles<-merge(daterr_profiles0,profiles_trim_summ,by="site_id")
daterr_profiles$group<-daterr_profiles$manmade_natural
daterr_profiles$variable<-as.character(daterr_profiles$variable)
daterr_profiles$namestate<-paste(daterr_profiles$name,daterr_profiles$state)
daterr_profiles_start<-daterr_profiles %>% arrange(name,variable,depth) %>% as.data.frame()
vars_profiles<-unique(daterr_profiles$variable)
namestate_profiles<-unique(daterr_profiles$namestate)

write.csv(daterr,"daterr.csv")
write.csv(sites_trim,"sites_trim.csv")
write.csv(daterr_profiles_start,"daterr_profiles.csv")

#Lake Delavan WI
#Castle Lake CA


#}



# random sample/subset the lakes to make a restricted sample for class

#sampling<-sample(unique(daterr$site_id),25)
do<-0
if(do==1){
  sampling<-sample(unique(daterr$site_id),50)
  
  #sampling<-sample(1:length(daterr[,1]),100)
  
  
  #daterr<-daterr[sampling,]
  daterr<-daterr %>% filter(site_id %in% sampling)
  
  daterr_wide<-daterr_wide %>% filter(site_id %in% sampling)
}

do<-0

if(do==1){
  
  ###################3
  
  melt(profiles1 %>% select(SITE_ID,DEPTH,TEMPERATURE,OXYGEN,PH),id.vars=c("SITE_ID","DEPTH"))
  
  
  merge(algaltoxins0,profiles0,by=c("SITE_ID","UID","STATE")) %>% select(SITE_ID,DEPTH,MICL_RESULT,MICX_RESULT)
  
  
  profiles_trunc<-profiles0 %>% select(SITE_ID,DEPTH, TEMPERATURE,OXYGEN,PH)
  
  
  data<-profiles
  field_list<-names(data)
  
  ######################
  
  melt(keyvars,by="SITE_ID")
  
  
  merged<-merge(sites_short,keyvars,by=c("SITE_ID"))
  
  
  
  
  
  melt(merged,id.vars=c("SITE_ID","UID","LAT_DD83","LON_DD83",EPA_REG,AGGR_ECO3_2015,AGGR_ECO9_2015))
}



