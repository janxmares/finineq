# Jan Mares 6/3/2015
# MFCR
# Based on script by johannes kutsam october 2012 johannes.kutsam@gmail.com 

# libraries plyr, zoo, data.table, reshape2, stringr  are used

#features:
#data sets and dimensions are downloaded from the bulk service from eurostat
# dimensions are correctly labeled as factors
#load dimensions into a local cache to speed up the labeling

library(data.table)
library(reshape2)
library(zoo)
library(plyr)
library(stringr)

read.eurostat=function(datasetname,LANGUAGE="en",cache=TRUE, flags=F){
     dsfname=paste(datasetname,".Rdata",sep="")
  if(file.exists(paste0('Data/',dsfname)) & cache==TRUE){
    load(file=paste0('Data/',dsfname))}
   else{
        baseurl="http://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&file=data%2F"
        fullfilename=paste(datasetname,".tsv.gz",sep="")
        temp <- paste(tempfile(),".gz",sep="")
        download.file(paste(baseurl,fullfilename,sep=""),temp)
        dataconnection <- gzfile(temp)
        d=data.table(read.delim(dataconnection))
        
        #split the dimension column into its components
        firstname=colnames(d)[1] # remove .time and count how many headings are there 
        firstname=substr(firstname,1,nchar(firstname)-nchar(".time"))
        headings=toupper(strsplit(firstname,".",fixed=TRUE)[[1]])
        headingcount=length(headings)
        setnames(d,colnames(d)[1],c("dimensions"))
        headings_split = data.table(colsplit(d[,dimensions], pattern = "\\,",names=headings))
        for(x in headings) headings_split[[x]]=as.factor(headings_split[[x]])
                
        #join dimensions with data, convert to long format,split the value from the flag
        mydata=cbind(headings_split,d[,colnames(d)[-1],with=FALSE])

        # strip flags
        if(flags){
            eurostatdata=mydata    
        }
        else{
           stripflags <- data.table(sapply(mydata[,(headingcount+1):ncol(mydata), with=F], function(x) as.numeric(str_extract(x,'[-]?[0-9][,]?[0-9]*[,]?[0-9]?.?[0-9]?'))))
           eurostatdata <- cbind(mydata[,1:headingcount, with=F], stripflags)
        }
        if (file.exists('Data')){
    		save(eurostatdata,file=paste0("Data/",dsfname))    	
        } else {
        	dir.create(file.path(getwd(), 'Data'))
        	save(eurostatdata,file=paste0("Data/",dsfname))
        }
    	}

eurostatdata
}