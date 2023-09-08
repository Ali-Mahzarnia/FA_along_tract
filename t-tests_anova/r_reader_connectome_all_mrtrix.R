
library(readxl)
library(dplyr)

path_master='/Users/ali/Desktop/Sep23/FAGNN/connecomic_t_test/MasterSheet_Experiments2021.xlsx'
data=read_xlsx(path_master, sheet = '18ABB11_readable02.22.22_BJ_Cor' )
datatemp=data%>%select(DWI,Genotype,Weight, Sex, Diet, Age_Months,  CIVMID)#subselect
#nchar(datatemp[111,1])
datatemp=na.omit(datatemp)
datatemp[nchar(datatemp$DWI)==1,]=matrix(NA,1,dim(datatemp)[2])
datatemp=na.omit(datatemp)
datatemp[substr(datatemp$DWI,1,1)!="N",]=matrix(NA,1,dim(datatemp)[2])
datatemp=na.omit(datatemp) ## ommit all na and zero character dwi and died durring

####remove 60 line
#datatemp[substr(datatemp$DWI,1,2)!="N5",]=matrix(NA,1,dim(datatemp)[2])
#datatemp=na.omit(datatemp) ## ommit all na and zero character dwi and died durring
####


datatemp$DWI=as.numeric(substr(datatemp$DWI,2,6)) # make dwi numeric
datatemp=datatemp[datatemp$Genotype!="HN",]

####plains
path_connec="/Users/ali/Desktop/Sep23/FAGNN/connecomic_t_test/connectome/"
file_list=list.files(path_connec)
plain_index = grep("plain", file_list)
sft_node_index = grep("sift_node", file_list)
sft_index = grep("sift.csv", file_list)
dst_index = grep("distances.csv", file_list)
mean_FA_index = grep("mean_FA", file_list)

file_list = file_list[plain_index]
temp_conn= read.csv( paste0(path_connec,file_list[1]) , header = F )
#temp_conn=temp_conn[,2: dim(temp_conn)[2]]
connectivity=array( NA ,dim=c(dim(temp_conn)[1],dim(temp_conn)[2],dim(datatemp)[1]))
connectivity_raw =connectivity
dim(connectivity)

notfound=0
##read connec
for (i in 1:dim(connectivity)[3]) {
  
  temp_index=which(datatemp$DWI[i]==as.numeric(substr(file_list,2,6)))
  if (length(temp_index)>0) 
  {
  temp_connec=read.csv( paste0(path_connec,file_list[temp_index]) , header = F )
  #temp_connec=temp_connec[,2:dim(temp_connec)[2]]
  #colnames(temp_connec)=NA
  connectivity_raw[,,i]=as.matrix(temp_connec)
  #temp_connec = temp_connec /sum(temp_connec[lower.tri(temp_connec, diag=FALSE)])
  #connectivity[,,i]=as.matrix(temp_connec)
  }
  else
    notfound=c(notfound, datatemp$DWI[i])
  
}

# notfound=notfound[2:length(notfound)]
# not_found_index=which( datatemp$DWI  %in%  notfound )

# datatemp=datatemp[-not_found_index,]
# connectivity=connectivity[,,-not_found_index]
# sum(is.na(connectivity))

# connectivity_raw=connectivity_raw[,,-not_found_index]
sum(is.na(connectivity_raw))

response=datatemp
save(response, file="response.rda")
save(connectivity_raw, file="connectivity_plain.rda")





###### sift
file_list=list.files(path_connec)
file_list = file_list[sft_index]
temp_conn= read.csv( paste0(path_connec,file_list[1]) , header = F )
#temp_conn=temp_conn[,2: dim(temp_conn)[2]]
connectivity=array( NA ,dim=c(dim(temp_conn)[1],dim(temp_conn)[2],dim(datatemp)[1]))
connectivity_raw =connectivity
dim(connectivity)

notfound=0
##read connec
for (i in 1:dim(connectivity)[3]) {
  
  temp_index=which(datatemp$DWI[i]==as.numeric(substr(file_list,2,6)))
  if (length(temp_index)>0) 
  {
    temp_connec=read.csv( paste0(path_connec,file_list[temp_index]) , header = F )
    #temp_connec=temp_connec[,2:dim(temp_connec)[2]]
    #colnames(temp_connec)=NA
    connectivity_raw[,,i]=as.matrix(temp_connec)
    #temp_connec = temp_connec /sum(temp_connec[lower.tri(temp_connec, diag=FALSE)])
    #connectivity[,,i]=as.matrix(temp_connec)
  }
  else
    notfound=c(notfound, datatemp$DWI[i])
  
}

#notfound=notfound[2:length(notfound)]
#not_found_index=which( datatemp$DWI  %in%  notfound )

#datatemp=datatemp[-not_found_index,]
#connectivity=connectivity[,,-not_found_index]
#sum(is.na(connectivity))

#connectivity_raw=connectivity_raw[,,-not_found_index]
sum(is.na(connectivity_raw))
dim(connectivity_raw)

#response=datatemp
#save(response, file="response.rda")
save(connectivity_raw, file="connectivity_sift.rda")




###### sift_node
file_list=list.files(path_connec)
file_list = file_list[sft_node_index]
temp_conn= read.csv( paste0(path_connec,file_list[1]) , header = F )
#temp_conn=temp_conn[,2: dim(temp_conn)[2]]
connectivity=array( NA ,dim=c(dim(temp_conn)[1],dim(temp_conn)[2],dim(datatemp)[1]))
connectivity_raw =connectivity
dim(connectivity)

notfound=0
##read connec
for (i in 1:dim(connectivity)[3]) {
  
  temp_index=which(datatemp$DWI[i]==as.numeric(substr(file_list,2,6)))
  if (length(temp_index)>0) 
  {
    temp_connec=read.csv( paste0(path_connec,file_list[temp_index]) , header = F )
    #temp_connec=temp_connec[,2:dim(temp_connec)[2]]
    #colnames(temp_connec)=NA
    connectivity_raw[,,i]=as.matrix(temp_connec)
    #temp_connec = temp_connec /sum(temp_connec[lower.tri(temp_connec, diag=FALSE)])
    #connectivity[,,i]=as.matrix(temp_connec)
  }
  else
    notfound=c(notfound, datatemp$DWI[i])
  
}

#notfound=notfound[2:length(notfound)]
#not_found_index=which( datatemp$DWI  %in%  notfound )

#datatemp=datatemp[-not_found_index,]
#connectivity=connectivity[,,-not_found_index]
#sum(is.na(connectivity))

#connectivity_raw=connectivity_raw[,,-not_found_index]
sum(is.na(connectivity_raw))
dim(connectivity_raw)

#response=datatemp
#save(response, file="response.rda")
save(connectivity_raw, file="connectivity_sift_node.rda")








###### dst_index
file_list=list.files(path_connec)
file_list = file_list[dst_index]
temp_conn= read.csv( paste0(path_connec,file_list[1]) , header = F )
#temp_conn=temp_conn[,2: dim(temp_conn)[2]]
connectivity=array( NA ,dim=c(dim(temp_conn)[1],dim(temp_conn)[2],dim(datatemp)[1]))
connectivity_raw =connectivity
dim(connectivity)

notfound=0
##read connec
for (i in 1:dim(connectivity)[3]) {
  
  temp_index=which(datatemp$DWI[i]==as.numeric(substr(file_list,2,6)))
  if (length(temp_index)>0) 
  {
    temp_connec=read.csv( paste0(path_connec,file_list[temp_index]) , header = F )
    #temp_connec=temp_connec[,2:dim(temp_connec)[2]]
    #colnames(temp_connec)=NA
    connectivity_raw[,,i]=as.matrix(temp_connec)
    #temp_connec = temp_connec /sum(temp_connec[lower.tri(temp_connec, diag=FALSE)])
    #connectivity[,,i]=as.matrix(temp_connec)
  }
  else
    notfound=c(notfound, datatemp$DWI[i])
  
}

#notfound=notfound[2:length(notfound)]
#not_found_index=which( datatemp$DWI  %in%  notfound )

#datatemp=datatemp[-not_found_index,]
#connectivity=connectivity[,,-not_found_index]
#sum(is.na(connectivity))

#connectivity_raw=connectivity_raw[,,-not_found_index]
sum(is.na(connectivity_raw))
dim(connectivity_raw)

#response=datatemp
#save(response, file="response.rda")
save(connectivity_raw, file="connectivity_dst.rda")








###### mean_FA_index
file_list=list.files(path_connec)
file_list = file_list[mean_FA_index]
temp_conn= read.csv( paste0(path_connec,file_list[1]) , header = F )
#temp_conn=temp_conn[,2: dim(temp_conn)[2]]
connectivity=array( NA ,dim=c(dim(temp_conn)[1],dim(temp_conn)[2],dim(datatemp)[1]))
connectivity_raw =connectivity
dim(connectivity)

notfound=0
##read connec
for (i in 1:dim(connectivity)[3]) {
  
  temp_index=which(datatemp$DWI[i]==as.numeric(substr(file_list,2,6)))
  if (length(temp_index)>0) 
  {
    temp_connec=read.csv( paste0(path_connec,file_list[temp_index]) , header = F )
    #temp_connec=temp_connec[,2:dim(temp_connec)[2]]
    #colnames(temp_connec)=NA
    connectivity_raw[,,i]=as.matrix(temp_connec)
    cat ( sum( is.na(as.matrix(temp_connec)  )  ) , "\n")
    #temp_connec = temp_connec /sum(temp_connec[lower.tri(temp_connec, diag=FALSE)])
    #connectivity[,,i]=as.matrix(temp_connec)
  }
  else
    notfound=c(notfound, datatemp$DWI[i])
  
}

#notfound=notfound[2:length(notfound)]
#not_found_index=which( datatemp$DWI  %in%  notfound )

#datatemp=datatemp[-not_found_index,]
#connectivity=connectivity[,,-not_found_index]
#sum(is.na(connectivity))

#connectivity_raw=connectivity_raw[,,-not_found_index]
sum(is.na(connectivity_raw))
dim(connectivity_raw)

#response=datatemp
#save(response, file="response.rda")
save(connectivity_raw, file="connectivity_mean_FA_index.rda")










#setwd(system("pwd", intern = T) )

# 
# RNA=read.delim('/Users/ali/Desktop/may/rnarpackage/micewithsymbol.txt',sep="\t", header = T)
# RNA=t(RNA)
# RNA_rows=rownames(RNA)
# 
# RNA_rows= as.numeric(gsub("\\D", "", RNA_rows))
# #RNA_rows=RNA_rows[2:(length(RNA_rows)-1)]
# 
# RNA_data=RNA[2:(dim(RNA)[1]-1),]
# colnames(RNA_data)=RNA[dim(RNA)[1],]
# rownames(RNA_data)=RNA_rows[2:(length(RNA_rows)-1)]
# #rownames(RNA_data)=NULL
# sum(is.na(RNA_data))
# #RNA_data[!is.na(RNA_data)]=NA
# sum(is.na(RNA_data))
# 
# 
# exist_response_ID=sub("\\s*\\:.*$", "",  response$CIVM_ID)
# response$CIVM_ID=as.numeric(gsub("\\D", "", exist_response_ID ))
# #as.numeric(rownames(RNA_data))
# 
# 
# found=0
# notfound=0
# for (jj in 1:dim(RNA_data)[1]) {
# 
#   index=which( as.numeric(row.names( RNA_data)   [jj])== as.numeric(response$CIVM_ID)      )
#   if (length(index)>0){
#         found=c(found,index)
#   }
# 
#   else notfound=c(notfound,jj)
# 
#   }
# found=found[2:length(found)]
# 
# response=response[found,]
# dim(response)
# 
# connectivity=connectivity[,,found]
# dim(connectivity)
# 
# notfound=notfound[2:length(notfound)]
# RNA_data=RNA_data[-notfound,]
# 
# dim(RNA_data)
# 
# rownames(RNA_data)=NULL



#save(RNA_data, file="RNA_data.rda")





