library(BIOMOD)
library(ade4)
library(adehabitat)
library(sp)
library(gam)      
library(MASS)
library(mvtnorm)
library(gbm)
library(dismo)
library(dplyr)
library(grid)
library(gridExtra)
library(ff)
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

source("geb_698_sm_appendixs1/niche.overlap.functions.R")
source("geb_698_sm_appendixs1/occ.prep.functions.R")

temp_min <- stack("Pfister-etal_2019/temp_min.nc", varname="temp")
temp_max <- stack("Pfister-etal_2019/temp_max.nc", varname="temp")
prec <- stack("Pfister-etal_2019/precip_monthly.nc", varname="precip")


# load climate variable for all site of the study area 1 (column names should be x,y,X1,X2,...,Xn)
new_tmx <- subset(temp_min, which(getZ(temp_min) >= as.Date("1864-01-16") & getZ(temp_min) <= as.Date("1949-12-16"))) 
indices <- format(as.Date(names(new_tmx), format = "X%Y.%m.%d"), format = "%m")
indices <- as.numeric(indices)
historical_temp_min<- stackApply(new_tmx, indices, fun = mean)

new_tmx <- subset(temp_min, which(getZ(temp_min) >= as.Date("1950-01-16") & getZ(temp_min) <= as.Date("2017-12-16"))) 
indices <- format(as.Date(names(new_tmx), format = "X%Y.%m.%d"), format = "%m")
indices <- as.numeric(indices)
current_temp_min<- stackApply(new_tmx, indices, fun = mean)

new_tmx <- subset(temp_max, which(getZ(temp_max) >= as.Date("1864-01-16") & getZ(temp_max) <= as.Date("1949-12-16"))) 
indices <- format(as.Date(names(new_tmx), format = "X%Y.%m.%d"), format = "%m")
indices <- as.numeric(indices)
historical_temp_max<- stackApply(new_tmx, indices, fun = mean)

new_tmx <- subset(temp_max, which(getZ(temp_max) >= as.Date("1950-01-16") & getZ(temp_max) <= as.Date("2017-12-16"))) 
indices <- format(as.Date(names(new_tmx), format = "X%Y.%m.%d"), format = "%m")
indices <- as.numeric(indices)
current_temp_max<- stackApply(new_tmx, indices, fun = mean)

new_tmx <- subset(prec, which(getZ(prec) >= as.Date("1864-01-16") & getZ(prec) <= as.Date("1949-12-16"))) 
indices <- format(as.Date(names(new_tmx), format = "X%Y.%m.%d"), format = "%m")
indices <- as.numeric(indices)
historical_prec<- stackApply(new_tmx, indices, fun = mean)

new_tmx <- subset(prec, which(getZ(prec) >= as.Date("1950-01-16") & getZ(prec) <= as.Date("2017-12-16"))) 
indices <- format(as.Date(names(new_tmx), format = "X%Y.%m.%d"), format = "%m")
indices <- as.numeric(indices)
current_prec<- stackApply(new_tmx, indices, fun = mean)

historical_clim = biovars(historical_prec, historical_temp_min, historical_temp_max) 
historical_clim <- as.data.frame(rasterToPoints(historical_clim))
colnames(historical_clim)[3:ncol(historical_clim)] = paste0("X",1:(ncol(historical_clim)-2))
historical_clim<- historical_clim %>% na.omit()
historical_clim[1:3,1:5]

current_clim = biovars(current_prec, current_temp_min, current_temp_max) 
current_clim <- as.data.frame(rasterToPoints(current_clim))
colnames(current_clim)[3:ncol(current_clim)] = paste0("X",1:(ncol(current_clim)-2))
current_clim<- current_clim %>% na.omit()
current_clim[1:3,1:5]


# global climate for both ranges
# clim12<-rbind(historical_clim,current_clim)
clim12<-rbind(historical_clim, current_clim)
df <- clim12[-c(1,2)]

tmp <- cor(df)
tmp[upper.tri(tmp)] <- 0
diag(tmp) <- 0

df_new <- 
  df[, !apply(tmp, 2, function(x) any(abs(x) > 0.80, na.rm = TRUE))]
head(df_new)

clim12 <- clim12[c("x", "y", colnames(df_new))]
Xvar = colnames(df_new)

# loading occurrence sites for the species (column names should be x,y)
occ.sp.aggr<-na.exclude(read.delim("Pfister-etal_2019/all_data_every_century_with_habitats.txt",h=T,sep="\t"))
occ.sp.aggr <- occ.sp.aggr[c("longitude", "latitude", "species")]
colnames(occ.sp.aggr)[1:2] = c("x","y")
occ.sp.aggr <- occ.sp.aggr[!duplicated(occ.sp.aggr), ]
head(occ.sp.aggr)

set.seed(123)
s <- split(occ.sp.aggr, occ.sp.aggr$species)

for(i in 1:length(s)){
  
  print(paste0(i,"/", length(s),":",names(s)[i]))      
  
  # remove occurrences closer than a minimum distance to each other (remove aggregation). Setting min.dist=0 will remove no occurrence.
  occ.sp<-occ.desaggragation(df=s[[i]],colxy=1:2, min.dist=0.02083333,plot=F)
  head(occ.sp)
  # create sp occurrence dataset by adding climate variables from the global climate datasets
  # resolution should be the resolution of the climate data grid
  
  occ.sp1<-na.exclude(sample.sp.globvar(dfsp=occ.sp,colspxy=1:2,colspkept=NULL,dfvar=historical_clim,colvarxy=1:2,colvar="all",resolution=0.02083333))
  occ.sp2<-na.exclude(sample.sp.globvar(dfsp=occ.sp,colspxy=1:2,colspkept=NULL,dfvar=current_clim,colvarxy=1:2,colvar="all",resolution=0.02083333))
  
  data.env.occ<-rbind(historical_clim, current_clim, occ.sp1, occ.sp2)[Xvar]
  row.clim1<-1:nrow(historical_clim)
  row.clim2<-(nrow(historical_clim)+1):(nrow(historical_clim)+nrow(current_clim))
  row.clim12<-1:(nrow(historical_clim)+nrow(current_clim))
  row.sp1<-(nrow(historical_clim)+nrow(current_clim)+1):(nrow(historical_clim)+nrow(current_clim)+nrow(occ.sp1))
  row.sp2<-(nrow(historical_clim)+nrow(current_clim)+nrow(occ.sp1)+1):(nrow(historical_clim)+nrow(current_clim)+nrow(occ.sp1)+nrow(occ.sp2))
  
  row.w.1.env<-1-(nrow(historical_clim)/nrow(clim12))  # prevalence of clim1
  row.w.2.env<-1-(nrow(historical_clim)/nrow(clim12))  # prevalence of clim2
  row.w.env<-c(rep(row.w.1.env, nrow(historical_clim)),rep(row.w.2.env, nrow(historical_clim)),rep(0, nrow(occ.sp1)),rep(0, nrow(occ.sp2)))
  
  pca.cal <-dudi.pca(data.env.occ,row.w = row.w.env, center = T, scale = T, scannf = F, nf = 2)

  
  screeplot(pca.cal, type="lines")
  # predict the scores on the axes
  scores.clim12<- pca.cal$li[row.clim12,]
  scores.clim1<- pca.cal$li[row.clim1,]
  scores.clim2<- pca.cal$li[row.clim2,]
  scores.sp1<- pca.cal$li[row.sp1,]
  scores.sp2<- pca.cal$li[row.sp2,]
  
  # calculation of occurence density and test of niche equivalency and similarity 
  z1<- grid.clim(scores.clim12,scores.clim1,scores.sp1,R=100)
  z2<- grid.clim(scores.clim12,scores.clim2,scores.sp2,R=100)
  a<-niche.equivalency.test(z1,z2,rep=100)# test of niche equivalency and similarity according to Warren et al. 2008
  b<-niche.similarity.test(z1,z2,rep=100)
  b2<-niche.similarity.test(z2,z1,rep=100)
  
  #plot			
  pdf(file=paste0("PCA-ENV_habitat/", names(s)[i],".pdf"))
  m <- layout(matrix(c(1,1,2,2,1,1,2,2,3,3,4,5,3,3,6,7), 4, 4, byrow = TRUE))
  plot.niche(z1,title="PCA-env - Historical niche",name.axis1="PC1",name.axis2="PC2")
  plot.niche(z2,title="PCA-env - Current niche",name.axis1="PC1",name.axis2="PC2")
  plot.contrib(pca.cal$co,pca.cal$eig)
  plot.new(); text(0.5,0.5,paste("niche overlap:","\n","D=",round(as.numeric(niche.overlap(z1,z2,cor=T)[1]),3)))
  plot.overlap.test(a,"D","Equivalency")
  plot.overlap.test(b,"D","Similarity 2->1")
  plot.overlap.test(b2,"D","Similarity 1->2")
  dev.off()
  
}


var_exp <- pca.cal$eig/sum(pca.cal$eig)

pdf(file="screepolot.pdf")
plot(var_exp*100, type = "b", col = "blue", xlab="Principal Component", 
     ylab = "Variance Explained (%)", main = "Scree Plot")
dev.off()




