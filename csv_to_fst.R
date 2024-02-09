library(data.table)
library(fst)

url<-"https://github.com/waterfolk/NLAdata/raw/master/daterr.csv"

daterr0<-fread(url, header =  TRUE, sep = ',' , stringsAsFactors=FALSE,
               colClasses="character")
daterr<-daterr0

write.fst(daterr,"daterr.fst")

