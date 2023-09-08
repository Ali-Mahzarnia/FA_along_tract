library(dplyr)

#noreadcsf=c(148,152,161,314,318,327) # dont read csf already in matlab

load(file='connectivity_plain.rda')
load(file='response.rda')
connectivity = connectivity_raw
temp=connectivity[,,1]
#temp=connectivity[,,1]
indexlower=lower.tri(temp, diag=FALSE)
indexlowertrue=which(indexlower==TRUE)
temp=temp[indexlower]
len=sum(indexlower)  

# right striatum to right_inferior_colliculus
value = connectivity_raw[250,248, ]

response$Age_Months


lm = lm( value~ response$Age_Months )
an=anova(lm)
p=an$`Pr(>F)`






