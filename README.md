# NLAdata

This repository contains large files that necessitate use of git LFS (large file storage). 

Use wrangle R script to scrape NLA data and metadata files from EPA website 
and download them to the local repository, 
then upload the data files as objects into R,
then compile the objects into three long form files: 
1) daterr.csv (main data)
2) daterr_profiles.csv (vertical profile data)
3) site_trim.csv (site information)

Scraping and downloading the files takes a few minutes on Powers machine.
Compiling the files into long format takes several minutes. 

Potential next steps
This script does not yet wrangle every single NLA variable (but it does wrangle a lot of them).
Because phytoplankton and invertebrate data have multiple taxonomic levels, they require
decisions about which taxonomic level to aggregate to, and until further notice the aggregation
levels occur at higher levels and/or what seemed easiest to code quickly.






 
 








