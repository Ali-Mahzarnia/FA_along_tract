library(xlsx)
library(dplyr)

library(xlsx)
library(lme4)
library(lmerTest)
library(multcomp)
library(rstatix)
library(stringr)
library(ggplot2)
library(reshape2)
library(ggfortify)


#fa files
fa_path = '/Users/ali/Desktop/Sep23/FAGNN/mixed_model/fa_along_tract_mrtrix/'
file_list_all=list.files(fa_path)

keywords=  unique( str_sub(file_list_all, 7))

for (connection_keyword in keywords) {
  


 # connection_keyword= '_right_striatum_to_right_inferior_colliculus_resampled_fa_values.txt'
  # connection_keyword= '_right_corpus_callosum_to_right_inferior_colliculus_resampled_fa_values.txt'
region_index = grep(connection_keyword, file_list_all)
file_list = file_list_all[region_index]



# read the first one to get the dimension
temp=read.delim( paste0(fa_path,file_list[1]), sep=" ", header  = F)
temp = temp [ 2: (dim(temp)[1]), ]

plot(as.numeric(temp[1,]))
len = length(as.numeric(temp[1,]))

#master
path_master='/Users/ali/Desktop/Sep23/FAGNN/mixed_model/MasterSheet_Experiments2021.xlsx'
Ages=read.xlsx2(path_master, sheetName = "18ABB11_readable02.22.22_BJ_Cor") %>%select( DWI, Age_Months )

#reading
data = matrix(NA,1,(len+2) )


for (i in 1:length(file_list)) {
  
  temp=read.delim( paste0(fa_path,file_list[i]), sep=" ", header  = F)
  temp = temp [ 2: (dim(temp)[1]), ]
  
  Ages_index = which( substr( file_list[i] , 1 , 6) ==  substr(Ages$DWI , 1 , 6)   )
  if(length(Ages_index)>0){
    
    
    age =   as.numeric(Ages[Ages_index,2 ])
    
    temp_data = cbind( substr( file_list[i] , 1 , 6), age, temp ) 
    colnames(temp_data)[1] = "ID"
    colnames(data)=  colnames(temp_data)
    data = rbind(data,temp_data)
    
  }
}
data = na.omit(data)

# ### find min and max index of nonzero to trim
# for (i in 2:dim(data)[2]) {
#   if(sum(abs(data[,i])) > 0 ) {break}
# }
# min_index = i
# 
# 
# for (i in dim(data)[2]:2) {
#   if(sum(abs(data[,i])) > 0 ) {break}
# }
# max_index = i
# 
# 
# data = as.data.frame(cbind(data[,1], data[,min_index:max_index]))
# 

#agecat
median_age = median(data$age)
agecat = data$age; agecat[data$age<median_age] = 1 ; agecat[data$age>=median_age] = 2
data = as.data.frame( cbind(agecat, data)   )
FA_data = data[, 4:dim(data)[2]]
FA_agecat = as.data.frame(cbind( agecat ,  FA_data ))
FA_age =  as.data.frame(cbind(  data$age ,  FA_data )) ; colnames(FA_age)[1]= "age"
Age_numbers = data$age
# 
# ### mean ages = average curves
# means = aggregate(.~agecat, data=FA_agecat, mean)
# means = means [, 2:dim(means)[2] ]
# young = as.numeric(means[1,])
# old = as.numeric(means[2,])
# plot(old)
# points(young, col='green', pch=12)
# ks.test(old, young ,  alternative="greater")
# #### ks distance would not work pval = 0.2
# 


#### voxel based


pvals = matrix(NA,6,dim(FA_data)[2] )
for (i in 1:dim(FA_data)[2]) {
  fas= as.numeric(FA_data[,i])
  #lm = lm(fas ~ as.numeric(Age_numbers) )
  lm = lmer( fas ~ as.numeric(Age_numbers)  +  (1|ID) ,data=data, REML = TRUE)
  an=anova(lm)
  p=an$`Pr(>F)`
  p = as.numeric(p[1])
  pvals[1,i] = p
  
  eff  = effectsize::cohens_f(lm, alternative='two.sided')
  pvals[3,i] = eff$Cohens_f_partial
  pvals[4,i] = eff$CI_low
  pvals[5,i]  = eff$CI_high
}



pvals[2,] = p.adjust(pvals[1,] , method = "fdr")
pvals[6,] = sum(pvals[2,] <0.05 )
rownames(pvals) = c("P-Value","Adjusted" , "Cohen Effect Size", "95% CI lower", "95% CI upper" , "Number of significant positions Among 50")
#which(pvals[2,] <0.05 )

   write.xlsx2(pvals, "mixed.xlsx", sheetName = paste0(connection_keyword) , append = TRUE)


   # 
   # 
   # 
   # ##### plot
   # data = as.data.frame(data)
   # melt = melt(data =data , id.vars=c( "ID", "V1", "agecat") ) 
   # 
   # ggplot(melt,                            
   #        aes(x = ID,
   #            y = value,
   #            col = agecat, group=agecat)) +
   #   geom_line()
   # 
   # 
   # autoplot(ts( cbind(ts1, ts2)  , start = c(2010,5), frequency = 12 ),
   #          facets = FALSE)
   # 
   # 
   # 
   }





