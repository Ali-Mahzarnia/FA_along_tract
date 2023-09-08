library(xlsx)
library(dplyr)


#fa files
fa_path = '/Users/ali/Desktop/Sep23/FAGNN/FA_along_tract/'
file_list=list.files(fa_path)

# read the first one to get the dimension
temp=read.delim( paste0(fa_path,file_list[2]))
temp = temp [, 2: (dim(temp)[2]-1)]
plot(as.numeric(temp[1,]))
len = length(as.numeric(temp[1,]))

#master
path_master='/Users/ali/Desktop/Sep23/FAGNN/MasterSheet_Experiments2021.xlsx'
Ages=read.xlsx2(path_master, sheetName = "18ABB11_readable02.22.22_BJ_Cor") %>%select( DWI, Age_Months )

#reading
data = matrix(NA,length(file_list),(len+1) )


for (i in 1:length(file_list)) {
  
  temp=read.delim( paste0(fa_path,file_list[i]) )
  Ages_index = which( substr( file_list[i] , 1 , 6) ==  substr(Ages$DWI , 1 , 6)   )
  if(length(Ages_index)>0){
  
  
  data[i,1] =   as.numeric(Ages[Ages_index,2 ])
  temp = temp [, 2: (dim(temp)[2]-1)]
  data[i, 2:dim(data)[2]] = as.numeric(temp[1,])
  }
}
data = na.omit(data)

### find min and max index of nonzero to trim
for (i in 2:dim(data)[2]) {
  if(sum(abs(data[,i])) > 0 ) {break}
}
min_index = i


for (i in dim(data)[2]:2) {
  if(sum(abs(data[,i])) > 0 ) {break}
}
max_index = i


data = as.data.frame(cbind(data[,1], data[,min_index:max_index]))


#agecat
median_age = median(data$V1)
agecat = data$V1; agecat[data$V1<median_age] = 1 ; agecat[data$V1>=median_age] = 2
data = as.data.frame( cbind(agecat, data)   )
FA_data = data[, 3:dim(data)[2]]
FA_agecat = as.data.frame(cbind( agecat ,  FA_data ))
FA_age =  as.data.frame(cbind(  data$V1 ,  FA_data )) ; colnames(FA_age)[1]= "age"
Age_numbers = data[,2]

### mean ages = average curves
means = aggregate(.~agecat, data=FA_agecat, mean)
means = means [, 2:dim(means)[2] ]
young = as.numeric(means[1,])
old = as.numeric(means[2,])
plot(old)
points(young, col='green', pch=12)
ks.test(old, young ,  alternative="greater")
#### ks distance would not work pval = 0.2



#### voxel based


pvals = matrix(NA,2,dim(FA_data)[2] )
for (i in 1:dim(FA_data)[2]) {
 
  lm = lm( unlist(FA_data[,i]) ~ Age_numbers )
  an=anova(lm)
  p=an$`Pr(>F)`
  p = as.numeric(p[1])
  pvals[1,i] = p
  
}



pvals[2,] = p.adjust(pvals[1,] , method = "fdr")
  
rownames(pvals) = c("P-Value","Adjusted")
which(pvals[2,] <0.05 )

write.xlsx2(pvals, "p-values.xlsx")



