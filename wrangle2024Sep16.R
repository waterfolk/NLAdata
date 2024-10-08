rm(list=ls())

alreadyran<-0

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

library(rvest)

##################

url<-"https://www.epa.gov/national-aquatic-resource-surveys/data-national-aquatic-resource-surveys"

page <- read_html(url)
links<- page %>% html_nodes("a") %>% html_attr("href")
links_nla<-links[grep("nla",links,ignore.case=TRUE)]
links_nlacsv<-links_nla[grep(".csv",links_nla)]
links_nlatxt<-links_nla[grep(".txt",links_nla)]

#urlpart<-"https://www.epa.gov/"
urlpart<-""
urls<-paste(urlpart,links_nlacsv,sep="")

#https://www.epa.gov/sites/default/files/2013-09/nla2007_basin_landuse_metrics_20061022.csv

for(i in 1:length(urls)){
  print(i)
  urli<-urls[i]
  #  datai<-fread(urls[i])
  destfilei<-as.character(strsplit(urli,"/")[[1]])[length(strsplit(urli,"/")[[1]])]
  #  download.file(urli,destfile=destfilei)
  download.file(urli,destfile=paste("data/",destfilei,sep=""))
}

# now do .txt files, mostly metadata
urls<-paste(urlpart,links_nlatxt,sep="")
for(i in 1:length(urls)){
  print(i)
  urli<-urls[i]
  #  datai<-fread(urls[i])
  destfilei<-as.character(strsplit(urli,"/")[[1]])[length(strsplit(urli,"/")[[1]])]
  #  download.file(urli,destfile=destfilei)
  download.file(urli,destfile=paste("data/",destfilei,sep=""))
}


#####

datafiles<-list.files(path="data/")
csvs<-datafiles[grep(".csv",datafiles)]
txts<-datafiles[grep(".txt",datafiles)]
csvs2012<-csvs[grep("2012",csvs)]
csvs2017<-csvs[grep("2017",csvs)]
csvs2022<-csvs[grep("2022",csvs)]

csv_short<-gsub(".csv","",csvs)

getstarted<-function(file,dir='data/'){
  data<-fread(paste(dir,file,sep="")) %>% as.data.frame()
  # catch bad reads of column names
  if(substr(names(data)[2],1,1)=="V"){
    data<-read.csv(paste(dir,file,sep="")) %>% as.data.frame()
  }
  data
}

#data<-fread("data/nla2012_secchi_08232016.csv")
#data<-fread("data/nla2007_meando_data_20091007.csv")

for(i in 1:length(csvs)){
  print(i)
  datai<-getstarted(csvs[i])
  datai<-clean_names(datai)
  obji<-gsub(".csv","",csvs[i])
  obji<-gsub("-","_",obji)
  obji<-gsub("\\.","_",obji)
  assign(obji,datai)
}

#############################

sites2012<-nla2012_wide_siteinfo_08232016
sites2017<-nla_2017_site_information_data
sites2022<-nla22_siteinfo

sitenames2012<-sites2012 %>% filter(visit_no %in% c(1,2)) %>% select(site_id,visit_no,date_col,gnis_name,nars_name,eval_name) %>% unique()
sitenames2017<-sites2017 %>% filter(visit_no %in% c(1,2)) %>% select(site_id,visit_no,date_col,gnis_name,nars_name) %>% unique()
sitenames2022<-sites2022 %>% filter(visit_no %in% c(1,2)) %>% select(site_id,visit_no,date_col,gnis_name,nars_name) %>% unique()

sitenames_merged<-merge(sitenames2012,sitenames2017,by="site_id",suffixes=c("_2012","_2017"),all=TRUE)
sitenames_merged2<-merge(sitenames_merged,sitenames2022,by="site_id",suffixes=c("","_2022"),all=TRUE)

# eval_name
sites2012_trim0<-sites2012 %>% select(site_id,uid,nars_name,gnis_name,date=date_col,visit_no,area_ha,elevation,lat=lat_dd83,lon=lon_dd83,manmade_natural=lake_origin,state,ag_eco3=aggr_eco3_2015,epa_reg,region=fw_eco9,cntyname)
sites2012_trim0<-sites2012_trim0 %>% arrange(site_id,date) %>% as.data.frame()
sites2012_trim0$year<-"2012"

sites2017_trim0<-sites2017 %>% select(site_id,uid,nars_name,gnis_name,date=date_col,visit_no,area_ha,elevation,lat=lat_dd83,lon=lon_dd83,manmade_natural=lake_orgn,state=pstl_code,ag_eco3,epa_reg,region=ag_eco9,cntyname)
sites2017_trim0<-sites2017_trim0 %>% arrange(site_id,date) %>% as.data.frame()
sites2017_trim0$year<-"2017"

sites2022_trim0<-sites2022 %>% select(site_id,uid,nars_name,gnis_name,date=date_col,visit_no,area_ha,elevation,lat=lat_dd83,lon=lon_dd83,manmade_natural=lake_orgn,state=pstl_code,ag_eco3,epa_reg,region=ag_eco9,cntyname)
sites2022_trim0<-sites2022_trim0 %>% arrange(site_id,date) %>% as.data.frame()
sites2022_trim0$year<-"2022"

#sitedateyears_rbind<-rbind(sites2012_trim0,sites2017_trim0)

sitedateyears_rbind<-rbind(sites2012_trim0,sites2017_trim0,sites2022_trim0)

#sitedateyears_rbind<-rbind(sites2012_trim0,sites2017_trim0)
sitedateyears_rbind$epa_reg[which(sitedateyears_rbind$epa_reg=="Region_1")]<-"Region_01"
sitedateyears_rbind$epa_reg[which(sitedateyears_rbind$epa_reg=="Region_2")]<-"Region_02"
sitedateyears_rbind$epa_reg[which(sitedateyears_rbind$epa_reg=="Region_3")]<-"Region_03"
sitedateyears_rbind$epa_reg[which(sitedateyears_rbind$epa_reg=="Region_4")]<-"Region_04"
sitedateyears_rbind$epa_reg[which(sitedateyears_rbind$epa_reg=="Region_5")]<-"Region_05"
sitedateyears_rbind$epa_reg[which(sitedateyears_rbind$epa_reg=="Region_6")]<-"Region_06"
sitedateyears_rbind$epa_reg[which(sitedateyears_rbind$epa_reg=="Region_7")]<-"Region_07"
sitedateyears_rbind$epa_reg[which(sitedateyears_rbind$epa_reg=="Region_8")]<-"Region_08"
sitedateyears_rbind$epa_reg[which(sitedateyears_rbind$epa_reg=="Region_9")]<-"Region_09"

# drop the subset of sites and dates that were revisits, keep first visit only
sitedateyears_trim<-sitedateyears_rbind %>% filter(visit_no==1)
sites_trim<-sitedateyears_trim %>% select(-uid,-date,-visit_no,-year) %>% unique()
#sites_trim<-sitedateyears_rbind[which(duplicated(sitedateyears_rbind$site_id)==FALSE),]
#sites_trim$name[which(sites_trim$name=="")]<-"unnamed"


###############################

nla_2017_secchi_data$result<-0.5*(nla_2017_secchi_data$disappears+nla_2017_secchi_data$reappears)
nla_2017_secchi_data$units<-"m"
nla_2017_secchi_data$var<-"Secchi depth"
#nla_2017_secchi_data<-nla_2017_secchi_data %>%
#  dplyr::summarize(result=0.5*(disappears+reappears))


nla22_secchi$result<-0.5*(nla22_secchi$disappears+nla22_secchi$reappears)
nla22_secchi$units<-"m"
nla22_secchi$var<-"Secchi depth"

nla2012_chla_wide$site_id<-""
nla2012_chla_wide$date_col<-""
nla2012_waterchem_wide$site_id<-""
nla2012_waterchem_wide$date_col<-""

names(nla22_waterchem_wide)[which(names(nla22_waterchem_wide)=="chla_result_units")]<-
  "chla_units"


nla2012_atrazine_08192016$var<-"Atrazine"
nla2012_secchi_08232016$var<-"Secchi depth"

nla2022_profile_wide$date_col<-format(mdy(nla2022_profile_wide$date_col), "%d-%b-%y")
#nla_2017_profile_data$date_col<-dmy(nla_2017_profile_data$date_col)
#nla2012_wide_profile_08232016$date_col<-dmy(nla2012_wide_profile_08232016$date_col)
nla2022_profile_wide$date_col<-format(dmy(nla2022_profile_wide$date_col), "%m/%d/%Y")
nla_2017_profile_data$date_col<-format(dmy(nla_2017_profile_data$date_col), "%m/%d/%Y")
nla2012_wide_profile_08232016$date_col<-format(dmy(nla2012_wide_profile_08232016$date_col), "%m/%d/%Y")
names(nla2012_secchi_08232016)[which(names(nla2012_secchi_08232016)=="secchi")]<-"result"

if(alreadyran==0){
  #names(nla2012_bentmet)[9:length(names(nla2012_bentmet))]<-
  #  paste("bentmet_",names(nla2012_bentmet)[9:length(names(nla2012_bentmet))],sep="")
  
  #names(nla_2017_benthic_metrics_data)[12:length(names(nla_2017_benthic_metrics_data))]<-
  #  paste("bentmet_",names(nla_2017_benthic_metrics_data)[12:length(names(nla_2017_benthic_metrics_data))],sep="")
  
  nla2012_bentmet_resultcols<-paste(names(nla2012_bentmet)[9:length(names(nla2012_bentmet))],
                                    collapse=",",sep="")
  nla2012_bentmet_resultcols<-paste(nla2012_bentmet_resultcols,collapse=",",sep="")
  nla2012_waterchem_wide_resultcols<-names(nla2012_waterchem_wide)[grep("result",names(nla2012_waterchem_wide))]
  nla2012_waterchem_wide_resultcols<-paste(nla2012_waterchem_wide_resultcols,collapse=",",sep="")
  
  nla2022_waterchem_wide_resultcols<-names(nla22_waterchem_wide)[grep("result",names(nla22_waterchem_wide))]
  nla2022_waterchem_wide_resultcols<-paste(nla2022_waterchem_wide_resultcols,collapse=",",sep="")
  
  #names(nla_2017_zooplankton_metrics_data_updated_12092021)[16:length(names(nla_2017_zooplankton_metrics_data_updated_12092021))]<-
  #  paste("bentmet_",names(nla_2017_zooplankton_metrics_data_updated_12092021)[16:length(names(nla_2017_zooplankton_metrics_data_updated_12092021))],sep="")
  
  nla_2017_benthic_metrics_data_resultcols<-paste(names(nla_2017_benthic_metrics_data)[16:length(names(nla_2017_benthic_metrics_data))],
                                                  collapse=",",sep="")
  nla_2017_benthic_metrics_data_resultcols<-paste(nla_2017_benthic_metrics_data_resultcols,collapse=",",sep="")
  
  
  nla_2017_zooplankton_metrics_data_updated_12092021_resultcols<-paste(names(nla_2017_zooplankton_metrics_data_updated_12092021)[16:length(names(nla_2017_zooplankton_metrics_data_updated_12092021))],
                                                                       collapse=",",sep="")
  nla_2017_zooplankton_metrics_data_updated_12092021_resultcols<-paste(nla_2017_zooplankton_metrics_data_updated_12092021_resultcols,collapse=",",sep="")
  
  #names(nla_2017_zooplankton_metrics_data_updated_12092021)
  
}

alreadyran<-1

nla_2012_zooplankton_count_data_updated_12092021$units<-"num indiv"
nla2012_algaltoxins_08192016$units<-"ppb"
nla2012_bentcond_08232016$units<-"%"
nla2012_bentcond_08232016$units<-"% of count, num indiv, or num taxa"
nla2012_bentmet$units<-"% of count, num indiv, or num taxa"
nla2012_chla_wide$units<-"ug/L"
nla2012_secchi_08232016$units<-"m"
nla2012_topsedhg_08192016$units<-"ng/g"
#nla2012_isotopes_wide$units<-""
#nla2012_waterchem_wide$units<-""
#nla2012_wide_phytoplankton_count_02122014$units<-""

ls()[grep("nla_2012|nla2012",ls())]
ls()[grep("nla_2017|nla2017",ls())]


nla2012_wide_watershed

datakey <- tribble(
  ~obj, ~year, ~varcols,~resultcols,
  ~unitcols,~refcols,~othercols,
  "nla_2012_zooplankton_count_data_updated_12092021","2012","count,count_300,biomass,biomass_300","count,count_300,biomass,biomass_300",
  "units", "uid", "site_id,date_col",
  
  "nla2012_algaltoxins_08192016","2012","micl_result,micx_result","micl_result,micx_result",
  "units", "uid", "site_id,date_col",
  "nla2012_atrazine_08192016","2012","var","atrazine_result",
  "atrazine_units", "uid", "site_id,date_col",
  #  "nla2012_bentcond_08232016","2012","mmi_bent_nla12,comp_pt,divs_pt,feed_pt,habt_pt,rich_pt,tolr_pt","mmi_bent_nla12,comp_pt,divs_pt,feed_pt,habt_pt,rich_pt,tolr_pt",
  #      "units", "uid", "site_id,date_col",
  "nla2012_bentmet","2012",nla2012_bentmet_resultcols,nla2012_bentmet_resultcols,
  "units", "uid", "site_id,date_col",
  "nla2012_chla_wide","2012","chll_result,chlx_result","chll_result,chlx_result",
  "units", "uid", "site_id,date_col",
  "nla2012_isotopes_wide","2012","d18o_h2o,d_d_h2o,e_i,rt","d18o_h2o,d_d_h2o,e_i,rt",
  "units", "uid", "site_id,date_col",
  "nla2012_secchi_08232016","2012","var","result",
  "units", "uid", "site_id,date_col",
  "nla2012_topsedhg_08192016","2012","methylmercury_result,totalhg_result","methylmercury_result,totalhg_result",
  "units", "uid", "site_id,date_col",
  "nla2012_waterchem_wide","2012",nla2012_waterchem_wide_resultcols,nla2012_waterchem_wide_resultcols,
  "units", "uid", "site_id,date_col",
  "nla2012_wide_phytoplankton_count_02122014","2012","abundance,biovolume,density","abundance,biovolume,density",
  "units", "uid", "site_id,date_col",
  "nla2012_wide_profile_08232016","2012","temperature,ph,oxygen,conductivity","temperature,ph,oxygen,conductivity",
  "units", "uid", "site_id,date_col",
  #  "nla2012_wide_watershed","2012","count,count_300,biomass,biomass_300","count,count_300,biomass,biomass_300",
  #      "units", "uid", "site_id,date_col"#,
  #) %>% as.data.frame()
  
  "nla_2017_algal_toxin_data","2017","analyte","result",
  "result_units","uid","site_id,date_col",
  "nla_2017_atrazine_data_0","2017","analyte","result",
  "result_units","uid","site_id,date_col",
  #  "nla_2017_benthic_count_data","2017","var","result",
  #      "units","uid","site_id,date_col",
  "nla_2017_benthic_metrics_data","2017",nla_2017_benthic_metrics_data_resultcols,nla_2017_benthic_metrics_data_resultcols,
  "units","uid","site_id,date_col",
  "nla_2017_e_coli_data","2017","e_coli_result,total_coliforms_result","e_coli_result,total_coliforms_result",
  "units","uid","site_id,date_col",
  "nla_2017_phytoplankton_count_data","2017","abundance,biovolume,density","abundance,biovolume,density",
  "units", "uid", "site_id,date_col",
  "nla_2017_profile_data","2017","temperature,ph,oxygen,conductivity","temperature,ph,oxygen,conductivity",
  "units", "uid", "site_id,date_col",
  "nla_2017_secchi_data","2017","var","result",
  "units","uid","site_id,date_col",
  "nla_2017_sediment_chemistry_data","2017","analyte","result",
  "units","uid","site_id,date_col",
  "nla_2017_water_chemistry_chla_data","2017","analyte","result",
  "result_units","uid","site_id,date_col",
  #  "nla_2017_zooplankton_count_data","2017","var","result",
  #      "result_units","uid","site_id,date_col",
  "nla_2017_zooplankton_metrics_data_updated_12092021","2017",nla_2017_zooplankton_metrics_data_updated_12092021_resultcols,nla_2017_zooplankton_metrics_data_updated_12092021_resultcols,
  "units","uid","site_id,date_col",
  "nla22_waterchem_wide","2022",nla2022_waterchem_wide_resultcols,nla2022_waterchem_wide_resultcols,
  "units", "uid", "site_id,date_col",
  "nla22_secchi","2022","var","result",
  "units","uid","site_id,date_col",
  "nla2022_profile_wide","2022","temperature,ph,oxygen,conductivity","temperature,ph,oxygen,conductivity",
  "units", "uid", "site_id,date_col"
) %>% as.data.frame()


datakey_singleresultcol<-datakey[-grep("\\,",datakey$resultcols),]
datakey_multiresultcol<-datakey[grep("\\,",datakey$resultcols),]

key<-datakey_singleresultcol
for (i in 1:length(datakey_singleresultcol[,1])){
  print(i)
  keyi<-key[i,]
  varcolsi<-strsplit(keyi$varcols[[1]],",")[[1]]
  resultcolsi<-strsplit(keyi$resultcols[[1]],",")[[1]]
  refcolsi<-strsplit(keyi$refcols[[1]],",")[[1]]
  othercolsi<-strsplit(keyi$othercols[[1]],",")[[1]]
  unitcolsi<-strsplit(keyi$unitcols[[1]],",")[[1]]
  numunitcolsi<-length(unitcolsi)
  obji<-keyi$obj
  datai<-get(obji)
  
  datai$depth<-""
  datai$year<-keyi$year
  datai$file<-obji
  
  datai$units<-datai[which(names(datai)==unitcolsi)][[1]]
  
  dataformati<-datai %>% select(year,file,refcolsi,othercolsi,depth,units,varcolsi,resultcolsi)  
  
  names(dataformati)[which(names(dataformati)=="analyte")]<-"var"
  

  names(dataformati)[grep("_result",names(dataformati))]<-"result"
  dataformati$var<-gsub("_result","",dataformati$var)

  dataformati<- dataformati %>% select(year,file,uid,site_id,date_col,units,var,depth,result)
  
  objformati<-paste("format_",obji,sep="")
  assign(objformati,data.frame(dataformati))
}

key<-datakey_multiresultcol
for (i in 1:length(key[,1])){
  print(i)
  keyi<-key[i,]
  varcolsi<-strsplit(keyi$varcols[[1]],",")[[1]]
  resultcolsi<-strsplit(keyi$resultcols[[1]],",")[[1]]
  refcolsi<-strsplit(keyi$refcols[[1]],",")[[1]]
  othercolsi<-strsplit(keyi$othercols[[1]],",")[[1]]
  unitcolsi<-strsplit(keyi$unitcols[[1]],",")[[1]]
  numunitcolsi<-length(unitcolsi)
  obji<-keyi$obj
  datai<-get(obji)
  
  if(length(grep("profile",obji))==0){
    datai$depth<-""
  }
  
  datai$year<-keyi$year
  datai$file<-obji
  
  if(!"units" %in% names(datai)){
    datai$units<-""
  }
  
#  if(obji=="nla2012_wide_phytoplankton_count_02122014"){
#    datai$r
#  }
  
  
  #  if(length(unitcolsi)==0){
  #    unitcolsi="units"
  #  }
  #  if(unitcolsi=="units" & length(grep("units",names(datai)))!=0){
  #    datai$units<-""
  #  }
  
  #  datai$units<-datai %>% select(unitcolsi) %>% c()
  
  dataformati<-datai %>% select(year,file,refcolsi,othercolsi,depth,unitcolsi,varcolsi,resultcolsi) %>%
    pivot_longer(names_to="var",cols=resultcolsi,values_to="result")#,unitcolsi)
  
  dataformati$var<-gsub("_result","",dataformati$var)
  
  dataformati<- dataformati %>% select(year,file,uid,site_id,date_col,units,var,depth,result)
  
  #  names(dataformati)<-c("year","file","uid","site_id","date_col","units","var","depth","result")
  objformati<-paste("format_",obji,sep="")
  assign(objformati,data.frame(dataformati))
}

########## Begin re-wrangling of stubborn data objects ############

#nla2012_waterchem_wide

result_names<-names(nla2012_waterchem_wide)[grep("result",names(nla2012_waterchem_wide))]
units_names<-names(nla2012_waterchem_wide)[grep("units",names(nla2012_waterchem_wide))]

year<-c()
file<-c()
uid<-c()
site_id<-c()
date_col<-c()
units<-c()
var<-c()
depth<-c()
result<-c()

datastubborn<-nla2012_waterchem_wide
for(i in 1:length(result_names)){
#for(i in 1:2){
  uidi<-datastubborn$uid
  site_idi<-datastubborn$site_id
  date_coli<-datastubborn$date_col
  unitsi<-datastubborn[,which(names(datastubborn)==units_names[i])]
  unitsi<-sort(unitsi,decreasing=TRUE)[1]
  unitsi<-rep(unitsi,length(uidi))
  vari<-gsub("_units","",units_names)[i]
  vari<-rep(vari,length(uidi))
  depthi<-""
  resulti<-datastubborn[,which(names(datastubborn)==result_names[i])]
  uid<-c(uid,uidi)
  site_id<-c(site_id,site_idi)
  date_col<-c(date_col,date_coli)
  units<-c(units,unitsi)
  var<-c(var,vari)
  depth<-c(depth,depthi)
  result<-c(result,resulti)
}
stubborndf<-data.frame(year="2012",file="nla2012_waterchem_wide",
                       uid,site_id,date_col,units,
                       var,depth,result)

uidsiteidkey<-merge(nla2012_wide_siteinfo_08232016 %>% select(uid,site_id),stubborndf %>% select(uid),by="uid") %>% unique()

merged<-merge(uidsiteidkey,stubborndf %>% select(-site_id),by=c("uid"),all=TRUE)
dataformati<-merged %>% select(year,file,uid,site_id,date_col,units,var,depth,result) %>% arrange(var,site_id)
objformati<-paste("format_","nla2012_waterchem_wide",sep="")
assign(objformati,data.frame(dataformati))

##################################################

result_names<-names(nla2012_chla_wide)[grep("result",names(nla2012_chla_wide))]
units_names<-names(nla2012_chla_wide)[grep("units",names(nla2012_chla_wide))]

year<-c()
file<-c()
uid<-c()
site_id<-c()
date_col<-c()
units<-c()
var<-c()
depth<-c()
result<-c()

datastubborn<-nla2012_chla_wide
for(i in 1:length(result_names)){
  #for(i in 1:2){
  uidi<-datastubborn$uid
  site_idi<-datastubborn$site_id
  date_coli<-datastubborn$date_col
  unitsi<-datastubborn[,which(names(datastubborn)==units_names[i])]
  unitsi<-sort(unitsi,decreasing=TRUE)[1]
  unitsi<-rep(unitsi,length(uidi))
  vari<-gsub("_units","",units_names)[i]
  vari<-rep(vari,length(uidi))
  depthi<-""
  resulti<-datastubborn[,which(names(datastubborn)==result_names[i])]
  uid<-c(uid,uidi)
  site_id<-c(site_id,site_idi)
  date_col<-c(date_col,date_coli)
  units<-c(units,unitsi)
  var<-c(var,vari)
  depth<-c(depth,depthi)
  result<-c(result,resulti)
}
stubborndf<-data.frame(year="2012",file="nla2012_chla_wide",
                       uid,site_id,date_col,units,
                       var,depth,result)

uidsiteidkey<-merge(nla2012_wide_siteinfo_08232016 %>% select(uid,site_id,date_col),stubborndf %>% select(uid),by="uid") %>% unique()

merged<-merge(uidsiteidkey,stubborndf %>% select(-site_id,-date_col),by=c("uid"),all=TRUE)
dataformati<-merged %>% select(year,file,uid,site_id,date_col,units,var,depth,result) %>% arrange(var,site_id)
objformati<-paste("format_","nla2012_chla_wide",sep="")
assign(objformati,data.frame(dataformati))

########################################################################################
#nla2022_waterchem_wide

result_names<-names(nla22_waterchem_wide)[grep("result",names(nla22_waterchem_wide))]
units_names<-names(nla22_waterchem_wide)[grep("units",names(nla22_waterchem_wide))]
units_names<-units_names[-which(units_names=="chla_extract_units")]

year<-c()
file<-c()
uid<-c()
site_id<-c()
date_col<-c()
units<-c()
var<-c()
depth<-c()
result<-c()

datastubborn<-nla22_waterchem_wide
for(i in 1:length(result_names)){
  #for(i in 1:2){
  uidi<-datastubborn$uid
  site_idi<-datastubborn$site_id
  date_coli<-datastubborn$date_col
  unitsi<-datastubborn[,which(names(datastubborn)==units_names[i])]
  unitsi<-sort(unitsi,decreasing=TRUE)[1]
  unitsi<-rep(unitsi,length(uidi))
  vari<-gsub("_units","",units_names)[i]
  vari<-rep(vari,length(uidi))
  depthi<-""
  resulti<-datastubborn[,which(names(datastubborn)==result_names[i])]
  uid<-c(uid,uidi)
  site_id<-c(site_id,site_idi)
  date_col<-c(date_col,date_coli)
  units<-c(units,unitsi)
  var<-c(var,vari)
  depth<-c(depth,depthi)
  result<-c(result,resulti)
  print(vari[1])
  print(resulti[1])
}
stubborndf<-data.frame(year="2022",file="nla22_waterchem_wide",
                       uid,site_id,date_col,units,
                       var,depth,result)

uidsiteidkey<-merge(nla22_siteinfo %>% select(uid,site_id),stubborndf %>% select(uid),by="uid") %>% unique()

merged<-merge(uidsiteidkey,stubborndf %>% select(-site_id),by=c("uid"),all=TRUE)
dataformati<-merged %>% select(year,file,uid,site_id,date_col,units,var,depth,result) %>% arrange(var,site_id)
objformati<-paste("format_","nla22_waterchem_wide",sep="")
assign(objformati,data.frame(dataformati))

##################################################

result_names<-names(nla2012_topsedhg_08192016)[grep("result",names(nla2012_topsedhg_08192016))]
units_names<-names(nla2012_topsedhg_08192016)[grep("units",names(nla2012_topsedhg_08192016))]

year<-c()
file<-c()
uid<-c()
site_id<-c()
date_col<-c()
units<-c()
var<-c()
depth<-c()
result<-c()

datastubborn<-nla2012_topsedhg_08192016
for(i in 1:length(result_names)){
  uidi<-datastubborn$uid
  site_idi<-datastubborn$site_id
  date_coli<-datastubborn$date_col
  unitsi<-datastubborn[,which(names(datastubborn)==units_names[i])]
  unitsi<-sort(unitsi,decreasing=TRUE)[1]
  unitsi<-rep(unitsi,length(uidi))
  vari<-gsub("_units","",units_names)[i]
  vari<-rep(vari,length(uidi))
  depthi<-""
  resulti<-datastubborn[,which(names(datastubborn)==result_names[i])]
  uid<-c(uid,uidi)
  site_id<-c(site_id,site_idi)
  date_col<-c(date_col,date_coli)
  units<-c(units,unitsi)
  var<-c(var,vari)
  depth<-c(depth,depthi)
  result<-c(result,resulti)
}
stubborndf<-data.frame(year="2012",file="nla2012_topsedhg_08192016",
                       uid,site_id,date_col,units,
                       var,depth,result)

uidsiteidkey<-merge(nla2012_wide_siteinfo_08232016 %>% select(uid,site_id),stubborndf %>% select(uid),by="uid") %>% unique()

merged<-merge(uidsiteidkey,stubborndf %>% select(-site_id),by=c("uid"),all=TRUE)
dataformati<-merged %>% select(year,file,uid,site_id,date_col,units,var,depth,result) %>% arrange(var,site_id)
objformati<-paste("format_","nla2012_topsedhg_08192016",sep="")
assign(objformati,data.frame(dataformati))


##################################################

year<-c()
file<-c()
uid<-c()
site_id<-c()
date_col<-c()
units<-c()
var<-c()
depth<-c()
result<-c()

datastubborn<-nla2012_wide_phytoplankton_count_02122014
result_names<-c("abundance","biovolume","density")
resultparts<-unique(datastubborn$genus) %>% sort()

for(i in 1:length(resultparts)){
  datai<-datastubborn %>% filter(genus==resultparts[i])
  
  uidi<-datai$uid
  site_idi<-datai$site_id
  date_coli<-datai$date_col
#  vari<-gsub("_units","",units_names)[i]
#  vari<-rep(vari,length(uidi))
  depthi<-""
  resulti<-datai[,which(names(datai) %in% result_names)]
  resultparti<-datai$genus
  
  dfi<-data.frame(site_idi,uidi,date_coli,depthi,resultparti,resulti)
  
  dfi<-dfi %>% pivot_longer(names_to="var",cols=c(names(resulti)),values_to="result")
  dfi$vari<-paste(dfi$var,dfi$resultparti)
  
  uid<-c(uid,dfi$uidi)
  site_id<-c(site_id,dfi$site_idi)
  date_col<-c(date_col,dfi$date_coli)
  units<-c(units,rep("",length(dfi$uidi)))
  var<-c(var,dfi$vari)
  depth<-c(depth,dfi$depthi)
  result<-c(result,dfi$result)
}
stubborndf<-data.frame(year="2012",file="nla2012_wide_phytoplankton_count_02122014",
                       uid,site_id,date_col,units,
                       var,depth,result)
stubborndf<-unique(stubborndf)
stubborndf<-stubborndf %>% group_by(year,file,uid,site_id,date_col,units,var,depth) %>%
  dplyr::summarize(result=sum(result,na.rm=TRUE)) %>% as.data.frame()

uidsiteidkey<-merge(nla2012_wide_siteinfo_08232016 %>% select(uid,site_id),stubborndf %>% select(uid),by="uid") %>% unique()

merged<-merge(uidsiteidkey,stubborndf %>% select(-site_id),by=c("uid"),all=TRUE)
dataformati<-merged %>% select(year,file,uid,site_id,date_col,units,var,depth,result) %>% arrange(var,site_id)
objformati<-paste("format_","nla2012_wide_phytoplankton_count_02122014",sep="")
assign(objformati,data.frame(dataformati))
#############################################

formatted<-ls()[grep("format_",ls())]

formatted_list <- lapply(ls()[grep("format_",ls())],
                         function(x) if (class(get(x)) == "data.frame") get(x))

longdata<-rbindlist(formatted_list) %>% as.data.frame()

longdata$result[grep("ND",longdata$result)]

longdata$result<-as.numeric(longdata$result)
longdata<-longdata %>% rename(value=result)
longdata<-longdata #%>% rename(var=var)
#as.numeric(longdata$result) %>% sort() %>% tail()


longdata$var<-tolower(longdata$var)
longdata$var<-gsub("\\/","_",longdata$var)
longdata$var<-gsub("\\-","_",longdata$var)
longdata$var<-gsub("\\(","",longdata$var)
longdata$var<-gsub("\\)","",longdata$var)
longdata$var<-gsub("_result","",longdata$var)

df<-data.frame(var=longdata$var,file=longdata$file) %>% unique() %>% arrange(var)

# data entry error? delete these?
nla_2017_water_chemistry_chla_data %>% filter(analyte=="BATCH_ID")

#which<-which(longdata$file %in% c("nla_2017_profile_data","nla2012_wide_profile_08232016") & longdata$var=="temperature" & longdata$result>50)

which<-which(longdata$file %in% c("nla2022_profile_wide","nla_2017_profile_data","nla2012_wide_profile_08232016") & longdata$var=="temperature" & longdata$result>50)
longdata[which,]

############################
#"","var","site_id","uid","date","name","lat","lon","manmade_natural","lake_origin12","state","aggr_eco3_2015","aggr_eco9_2015","epa_reg","regionL1","region","eco_bio","cntyname","depth","value","area_group","depth_group","units","group"
#"","site_id","uid","date","name","lat","lon","manmade_natural","lake_origin12","state","aggr_eco3_2015","aggr_eco9_2015","epa_reg","regionL1","region","eco_bio","cntyname","area_group","depth_group","group","depth","var","value","namestate"
#############################

# corrections
longdata$file[grep("water",longdata$file)] %>% unique()
#which<-which(longdata$file %in% c("nla_2017_profile_data","nla2012_wide_profile_08232016") & longdata$var=="temperature" & longdata$result>50)
which<-which(longdata$file %in% c("nla2022_profile_wide","nla_2017_profile_data","nla2012_wide_profile_08232016") & longdata$var=="temperature" & longdata$result>50)#longdata[which,]$result<-NA

##############################################################

longdata$file[grep("profile",longdata$file)] %>% unique()

stubborndf<-nla2012_wide_profile_08232016
# this table has missing info in the site_id column
# checking to see if we gain site_id's when we start with nla2012_wide_siteinfo_08232016
uidsiteidkey<-merge(nla2012_wide_siteinfo_08232016 %>% select(uid,site_id),stubborndf %>% select(uid),by="uid") %>% unique()
merged<-merge(uidsiteidkey,stubborndf %>% select(-site_id),by=c("uid"),all=TRUE)
length(nla2012_wide_profile_08232016[,1])
length(merged[,1])
# no site_id's gained, moving on

profiles_top1m<-longdata %>% filter(site_id!="") %>%
  filter(depth<=1, file %in% c("nla2022_profile_wide","nla_2017_profile_data","nla2012_wide_profile_08232016")) %>%
  group_by(site_id,uid,date_col,year) %>%
  dplyr::summarize(temperature=mean(value[which(var=="temperature")],na.rm=TRUE),
                   oxygen=mean(value[which(var=="oxygen")],na.rm=TRUE),
                   ph=mean(value[which(var=="ph")],na.rm=TRUE),
                   conductivity=mean(value[which(var=="conductivity")],na.rm=TRUE)) %>% mutate(depth="top1m") %>% melt(id.vars=c("site_id","uid","date_col","year","depth"))

profiles_1to2m<-longdata %>% filter(site_id!="") %>%
  filter(depth>1 & depth<=2, file %in% c("nla2022_profile_wide","nla_2017_profile_data","nla2012_wide_profile_08232016")) %>%
  group_by(site_id,uid,date_col,year) %>%
  dplyr::summarize(temperature=mean(value[which(var=="temperature")],na.rm=TRUE),
                   oxygen=mean(value[which(var=="oxygen")],na.rm=TRUE),
                   ph=mean(value[which(var=="ph")],na.rm=TRUE),
                   conductivity=mean(value[which(var=="conductivity")],na.rm=TRUE)) %>% mutate(depth="1to2m") %>% melt(id.vars=c("site_id","uid","date_col","year","depth"))

profiles_2to4m<-longdata %>% filter(site_id!="") %>%
  filter(depth>2 & depth<=4, file %in% c("nla2022_profile_wide","nla_2017_profile_data","nla2012_wide_profile_08232016")) %>%
  group_by(site_id,uid,date_col,year) %>%
  dplyr::summarize(temperature=mean(value[which(var=="temperature")],na.rm=TRUE),
                   oxygen=mean(value[which(var=="oxygen")],na.rm=TRUE),
                   ph=mean(value[which(var=="ph")],na.rm=TRUE),
                   conductivity=mean(value[which(var=="conductivity")],na.rm=TRUE)) %>% mutate(depth="2to4m") %>% melt(id.vars=c("site_id","uid","date_col","year","depth"))

profiles_below4m<-longdata %>% filter(site_id!="") %>%
  filter(depth>4, file %in% c("nla2022_profile_wide","nla_2017_profile_data","nla2012_wide_profile_08232016")) %>%
  group_by(site_id,uid,date_col,year) %>%
  dplyr::summarize(temperature=mean(value[which(var=="temperature")],na.rm=TRUE),
                   oxygen=mean(value[which(var=="dissolvedoxygen")],na.rm=TRUE),
                   ph=mean(value[which(var=="ph")],na.rm=TRUE),
                   conductivity=mean(value[which(var=="conductivity")],na.rm=TRUE)) %>% mutate(depth="below4m") %>% melt(id.vars=c("site_id","uid","date_col","year","depth"))

profiles_below5m<-longdata %>% filter(site_id!="") %>%
  filter(depth>5, file %in% c("nla2022_profile_wide","nla_2017_profile_data","nla2012_wide_profile_08232016")) %>%
  group_by(site_id,uid,date_col,year) %>%
  dplyr::summarize(temperature=mean(value[which(var=="temperature")],na.rm=TRUE),
                   oxygen=mean(value[which(var=="oxygen")],na.rm=TRUE),
                   ph=mean(value[which(var=="ph")],na.rm=TRUE),
                   conductivity=mean(value[which(var=="conductivity")],na.rm=TRUE)) %>% mutate(depth="below5m") %>% melt(id.vars=c("site_id","uid","date_col","year","depth"))

profiles_groups<-rbind(profiles_top1m,profiles_1to2m,profiles_2to4m,profiles_below4m,profiles_below5m)
profiles_groups<-profiles_groups %>% rename(var=variable)
profiles_groups$var<-paste(profiles_groups$var,profiles_groups$depth,sep="")
profiles_groups<-profiles_groups %>% select(-depth)
########################################################

###############################

watershed_names<-c("damcnt_bsn","damden_bsn","elevmean_bsn","elevmax_bsn",
                   "houseden_bsn","houseden_1000",
                   "nadp_totaln_bsn","nadp_no3_bsn","nadp_nh4_bsn",
                   "nlcd2006_11pct_bsn","nlcd2006_12pct_bsn",
                   "nlcd2006_22pct_bsn","nlcd2006_23pct_bsn","nlcd2006_24pct_bsn",
                   "nlcd2006_41pct_bsn","nlcd2006_42pct_bsn","nlcd2006_43pct_bsn",
                   "nlcd2006_82pct_bsn",
                   "nlcd2006_90pct_bsn","nlcd2006_95pct_bsn"
)


watershed<-nla2012_wide_watershed %>% select(site_id,watershed_names) %>% as.data.frame()
watershed$nlcd2006_22_23_24pct_bsn<- watershed$nlcd2006_22pct_bsn + watershed$nlcd2006_23pct_bsn + watershed$nlcd2006_24pct_bsn
watershed$nlcd2006_41_42_43pct_bsn<- watershed$nlcd2006_41pct_bsn + watershed$nlcd2006_42pct_bsn + watershed$nlcd2006_43pct_bsn
watershed$nlcd2006_90_95pct_bsn<- watershed$nlcd2006_90pct_bsn + watershed$nlcd2006_95pct_bsn
watershed_names<-c(watershed_names,
                   "nlcd2006_22_23_24pct_bsn","nlcd2006_41_42_43pct_bsn","nlcd2006_90_95pct_bsn")

watershed_trim<-melt(watershed %>% select(site_id,watershed_names),id.vars=c("site_id")) %>% na.omit()
#watershed_trim$depth<-""

watershed<-merge(sitedateyears_rbind,watershed_trim,by="site_id")
watershed$var<-as.character(watershed$variable)
watershed<-watershed %>% select(site_id,var,value) %>% arrange(site_id,var)
watershed$var[which(watershed$var=="nlcd2006_11pct_bsn")]<-"nlcd2006_openwater_pct_bsn"
watershed$var[which(watershed$var=="nlcd2006_22_23_24pct_bsn")]<-"nlcd2006_developed_pct_bsn"
watershed$var[which(watershed$var=="nlcd2006_41_42_43pct_bsn")]<-"nlcd2006_forest_pct_bsn"
watershed$var[which(watershed$var=="nlcd2006_82pct_bsn")]<-"nlcd2006_cropland_pct_bsn"
watershed$var[which(watershed$var=="nlcd2006_90_95pct_bsn")]<-"nlcd2006_wetland_pct_bsn"
watershed<-watershed %>% filter(!var %in% c("nlcd2006_12pct_bsn",
                                            "nlcd2006_22pct_bsn","nlcd2006_23pct_bsn","nlcd2006_24pct_bsn",
                                            "nlcd2006_41pct_bsn","nlcd2006_42pct_bsn","nlcd2006_43pct_bsn",
                                            "nlcd2006_90pct_bsn","nlcd2006_95pct_bsn"))
watershed<-watershed %>% unique()
watershed$year<-"2012"
#watershed<-watershed %>% select(-var)
#melt(chla %>% select(uid,waterchem_names),id.vars=c("uid")) %>% na.omit()

####################
daterr0<-longdata

daterr1<-merge(sitedateyears_rbind,daterr0,by=c("site_id","uid","year"),all=TRUE)

#daterr1<-merge(sites_trim,daterr0,by=c("site_id"),all=TRUE)

daterr1_null<-daterr1
daterr1_null$value<-""
daterr1_null$var<-""
daterr1_null$depth<-""
daterr1_null$uid<-""
daterr1_null$file<-""
daterr1_null$units<-""
daterr1_null$date_col<-""
daterr1_null<-daterr1_null %>% unique()


daterr_areas<-daterr1_null
daterr_areas$var<-"area_ha"
daterr_areas$value<-daterr_areas$area_ha

daterr_elevs<-daterr1_null
daterr_elevs$var<-"elevation"
daterr_elevs$value<-daterr_elevs$elevation

daterr_lats<-daterr1_null
daterr_lats$var<-"latitude"
daterr_lats$value<-daterr_lats$lat

#maxdepths<-longdata %>%
#  filter(file %in% c("nla_2017_profile_data","nla2012_wide_profile_08232016")) %>%
#  select(site_id,depth) %>%
#  group_by(site_id) %>%
#  dplyr::summarize(maxdepth=max(as.numeric(depth),na.rm=TRUE)) %>% as.data.frame()


#daterr_maxdepths<-merge(daterr1_null,maxdepths,by="site_id",all=TRUE)
#daterr_maxdepths$value<-daterr_maxdepths$maxdepth
#daterr_maxdepths$var<-"depthmax"
#daterr_maxdepths<-daterr_maxdepths %>% select(-maxdepth)


daterr_profiles_groups<-merge(daterr1_null %>% select(-date_col,-year,-var,-value),profiles_groups,by=c("site_id"))
daterr_profiles_groups<- daterr_profiles_groups %>% rename(uid=uid.y)
daterr_profiles_groups<- daterr_profiles_groups %>% select(-uid.x)

#daterr1<-rbind(daterr1,daterr_areas,daterr_elevs,daterr_lats,daterr_maxdepths)

#daterr1<-rbind(daterr1,daterr_areas,daterr_elevs,daterr_lats,daterr_maxdepths,daterr_profiles_groups)
daterr1<-rbind(daterr1,daterr_areas,daterr_elevs,daterr_lats,daterr_profiles_groups)


#watershed<-watershed %>% select(-variable)
#watershed$year<-""
#watershed$file<-""
#watershed$uid<-""
#watershed$date_col<-""
#watershed$unit<-""

daterr1<-rbind.fill(daterr1,watershed)

#daterr1$unit<-"mg/L"
daterr<-daterr1
#daterr<-rbind(daterr_all,daterr1)
#daterr$group<-daterr$state
daterr<-unique(daterr)


daterr_profiles0<-daterr[grep("profile",daterr$file),]
daterr_profiles1<-daterr_profiles0 %>% select(-area_ha,-elevation,-lat,-lon,-manmade_natural,-state,-ag_eco3,-epa_reg)
daterr_profiles2<-merge(sitedateyears_rbind,daterr_profiles1,by=c("site_id","uid","year"))
drop<-grep(".y",names(daterr_profiles2))

daterr_profiles3<-daterr_profiles2[,-drop]
names(daterr_profiles3)<-gsub(".x","",names(daterr_profiles3))

daterr_profiles_out<-daterr_profiles3

daterr_profiles3$depth<-as.numeric(daterr_profiles3$depth)
depthmaxes<-daterr_profiles3 %>% select(uid,depth) %>% group_by(uid) %>%
  dplyr::summarize(depthmax=max(depth,na.rm=TRUE)) %>% as.data.frame()
depthmaxes$depthmax[which(depthmaxes$depthmax<0)]<-NA

daterr_depthmaxes0<-daterr[which(duplicated(daterr$uid)==FALSE),]
daterr_depthmaxes<-merge(daterr_depthmaxes0 %>% select(-value,-var,-units,file),
                         depthmaxes,by="uid")
#daterr_depthmaxes<-merge(sitedateyears_rbind,depthmaxes,by="uid")
daterr_depthmaxes<-daterr_depthmaxes %>% rename(value=depthmax)
daterr_depthmaxes$var<-"depthmax"
daterr_depthmaxes$units<-"m"
daterr_depthmaxes$file<-"nla2012_wide_profile_08232016, nla_2017_profile_data, nla2022_profile_wide"
daterr<-rbind.fill(daterr,daterr_depthmaxes)

daterr<-daterr %>% arrange(var)
daterr_out<-daterr
daterr_out$value<-signif(daterr_out$value,digits=3)

write.csv(daterr_out,"daterr.csv")
write.csv(sitedateyears_rbind,"sites_trim.csv")
write.csv(daterr_profiles_out,"daterr_profiles.csv")
