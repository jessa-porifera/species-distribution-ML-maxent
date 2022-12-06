#call libraries
library("raster")
library("rgeos")
library("dismo")
library("rJava")
library("sf")

```

## Prepation for the analysis

```{r data}

#import data
all<-read.table("all_data_every_century.txt", header = T)

#data cleaning
all$species[all$species=="Galium_uligunosum"]<-"Galium_uliginosum"
all$species[all$species=="Parietaria_iudaica"]<-"Parietaria_judaica"
all$species[all$species=="Polygonatum_vertizilatum"]<-"Polygonatum_vertizillatum"
sort(unique(all$species))

#define two time periods
data_before_1950 <-all[all$year<1950,]
data_after_1950  <-all[all$year >= 1950,]

#import climate data
prep.before.1950 <-raster("Prep_1864_1950.tif")
prep.after.1950 <-raster("Prep_1950_2000.tif")

temp.before.1950 <-raster("temp_1864_1950.tif")
temp.after.1950 <-raster("temp_1950_2000.tif")

#import shapefile
Switzerland <- getData('GADM', country='CHE', level=0)
Switzerland
plot(Switzerland)

#import elevation
elevation<-raster("DHM200.asc")


#import lakes
lakes.extent.corrected<-raster("lakes_with_zero.tif")


#raster stack
predictors.before.1950 <- stack(prep.before.1950,
                                temp.before.1950,
                                elevation.extent.corrected,
                                lakes.extent.corrected)


predictors.after.1950 <- stack(prep.after.1950, 
                               temp.after.1950,
                               elevation.extent.corrected,
                               lakes.extent.corrected)

#habitats
data_before_1950$habitat[data_before_1950$species=="Ballota_nigra"|
                           data_before_1950$species=="Chelidonium_majus"|
                           data_before_1950$species=="Centranthus_ruber"|
                           data_before_1950$species=="Cymbalaria_muralis"|
                           data_before_1950$species=="Lamium_album"|
                           data_before_1950$species=="Parietaria_iudaica"] <- "Perennial ruderals"

data_after_1950$habitat[data_after_1950$species=="Ballota_nigra"|
                          data_after_1950$species=="Chelidonium_majus"|
                          data_after_1950$species=="Centranthus_ruber"|
                          data_after_1950$species=="Cymbalaria_muralis"|
                          data_after_1950$species=="Lamium_album"|
                          data_after_1950$species=="Parietaria_iudaica"] <- "Perennial ruderals"

table(data_before_1950$habitat)
table(data_after_1950$habitat)


data_before_1950$habitat[data_before_1950$species=="Crepis_vesicaria_taraxacifolia"|
                           data_before_1950$species=="Descurainia_sophia"|
                           data_before_1950$species=="Lactuca_serriola"|
                           data_before_1950$species=="Reseda_lutea"|
                           data_before_1950$species=="Sisymbrium_officinale"|
                           data_before_1950$species=="Geranium_rotundifolium"] <- "Annual ruderals"

data_after_1950$habitat[data_after_1950$species=="Crepis_vesicaria_taraxacifolia"|
                          data_after_1950$species=="Descurainia_sophia"|
                          data_after_1950$species=="Lactuca_serriola"|
                          data_after_1950$species=="Reseda_lutea"|
                          data_after_1950$species=="Sisymbrium_officinale"|
                          data_after_1950$species=="Geranium_rotundifolium"] <- "Annual ruderals"


table(data_before_1950$habitat)
table(data_after_1950$habitat)


data_before_1950$habitat[data_before_1950$species=="Ajuga_genevensis"|
                           data_before_1950$species=="Campanula_patula"|
                           data_before_1950$species=="Campanula_rapunculus"|
                           data_before_1950$species=="Geranium_molle"|
                           data_before_1950$species=="Helictotrichon_pubescens"|
                           data_before_1950$species=="Malva_moschata"] <- "Semi-arid grassland"

data_after_1950$habitat[data_after_1950$species=="Ajuga_genevensis"|
                          data_after_1950$species=="Campanula_patula"|
                          data_after_1950$species=="Campanula_rapunculus"|
                          data_after_1950$species=="Geranium_molle"|
                          data_after_1950$species=="Helictotrichon_pubescens"|
                          data_after_1950$species=="Malva_moschata"] <- "Semi-arid grassland"

table(data_before_1950$habitat)
table(data_after_1950$habitat)


data_before_1950$habitat[data_before_1950$species=="Crepis_paludosa"|
                           data_before_1950$species=="Galium_uligunosum"|
                           data_before_1950$species=="Myosotis_scorpioides"|
                           data_before_1950$species=="Sanguisorba_officinalis"|
                           data_before_1950$species=="Silene_flos-cuculi"|
                           data_before_1950$species=="Stachis_palustris"] <- "Moist grasslands"

data_after_1950$habitat[data_after_1950$species=="Crepis_paludosa"|
                          data_after_1950$species=="Galium_uligunosum"|
                          data_after_1950$species=="Myosotis_scorpioides"|
                          data_after_1950$species=="Sanguisorba_officinalis"|
                          data_after_1950$species=="Silene_flos-cuculi"|
                          data_after_1950$species=="Stachis_palustris"] <- "Moist grasslands"

table(data_before_1950$habitat)
table(data_after_1950$habitat)


data_before_1950$habitat[data_before_1950$species=="Adenostyles_alliariae"|
                           data_before_1950$species=="Lilium_martagon"|
                           data_before_1950$species=="Polygonatum_vertizilatum"|
                           data_before_1950$species=="Ranunculus_platanifolius"|
                           data_before_1950$species=="Rosa_pendulina"|
                           data_before_1950$species=="Saxifraga_rotundifolia"] <- "Tall herb fringe"

data_after_1950$habitat[data_after_1950$species=="Adenostyles_alliariae"|
                          data_after_1950$species=="Lilium_martagon"|
                          data_after_1950$species=="Polygonatum_vertizilatum"|
                          data_after_1950$species=="Ranunculus_platanifolius"|
                          data_after_1950$species=="Rosa_pendulina"|
                          data_after_1950$species=="Saxifraga_rotundifolia"] <- "Tall herb fringe"

table(data_before_1950$habitat)
table(data_after_1950$habitat)

```

#Species mapping


```{r mapping, echo=FALSE}

d_mapping <- function(species){
  
  
  occ.before.1950 <- data_before_1950[data_before_1950$species==species,c(10,9)]
  cat("\nNumber of records before 1950 :", 
      nrow(occ.before.1950),
      "\nRunning MaxEnt....\n")
  
  if(nrow(occ.before.1950)>1){
    
    
    me.before.1950 <- maxent(predictors.before.1950, occ.before.1950)
    print(me.before.1950)
    
    
    ped.before.1950 <- predict(me.before.1950, predictors.before.1950)  # studyArea is the clipped rasters 
    
    
    
    
  }
  
  occ.after.1950 <- data_after_1950[data_after_1950$species==species,c(10,9)]
  cat("\nNumber of records after 1950 :", nrow(occ.after.1950),"\nRunning MaxEnt....\n")
  
  if(nrow(occ.after.1950)>1) {
    me.after.1950 <- maxent(predictors.after.1950, occ.after.1950)
    print(me.after.1950)
    
    ped.after.1950 <- predict(me.after.1950, predictors.after.1950)  # studyArea is the clipped rasters 
    
    
  }
  
  
  
  par(mfrow=c(2,2))
  
  max(values(ped.before.1950),na.rm = T)
  max(values(ped.after.1950),na.rm = T)
  
  max<- ifelse(max(values(ped.before.1950),na.rm = T)>max(values(ped.after.1950),na.rm = T),
               max(values(ped.before.1950),na.rm = T),
               max(values(ped.after.1950),na.rm = T))
  
  plot(ped.before.1950,main="Distribution : Before 1950",zlim=c(0,max))
  plot(st_geometry(lakes[order(-lakes$area)[1:874],]),
       add=T,col="lightblue", border = 'blue')
  
  
  plot(ped.after.1950,main="Distribution : After 1950",zlim=c(0,max))
  plot(st_geometry(lakes[order(-lakes$area)[1:874],]),
       add=T,col="lightblue", border = 'blue')
  
  
  
  if(nrow(occ.before.1950)>1&nrow(occ.after.1950)>1) {
    
    plot(ped.after.1950-ped.before.1950 , main="Change from before 1950 to after 1950" )
    plot(st_geometry(lakes[order(-lakes$area)[1:874],]),
         add=T,col="lightblue", border = 'blue')
    
  }
  
  print(species)
  cat("\nNumber of records before 1950 :", 
      nrow(occ.before.1950))
  cat("\nNumber of records after 1950 :", nrow(occ.after.1950))
  
}


```

```{r}
d_mapping("Galium_uliginosum")
d_mapping("Stachis_palustris")
d_mapping("Helictotrichon_pubescens")
d_mapping("Parietaria_judaica")
d_mapping("Rosa_pendulina")
d_mapping("Crepis_paludosa")
d_mapping("Campanula_rapunculus")
d_mapping("Descurainia_sophia")
d_mapping("Lilium_martagon")
d_mapping("Myosotis_scorpioides")
d_mapping("Ajuga_genevensis")
d_mapping("Geranium_molle")
d_mapping("Adenostyles_alliariae")
d_mapping("Malva_moschata")
d_mapping("Saxifraga_rotundifolia")
d_mapping("Silene_flos-cuculi")
d_mapping("Reseda_lutea")
d_mapping("Chelidonium_majus")
d_mapping("Polygonatum_vertizillatum")
d_mapping("Sanguisorba_officinalis")
d_mapping("Campanula_patula")
d_mapping("Lactuca_serriola")
d_mapping("Sisymbrium_officinale")
d_mapping("Cymbalaria_muralis")
d_mapping("Ballota_nigra")
d_mapping("Ranunculus_platanifolius")
d_mapping("Lamium_album")
d_mapping("Centranthus_ruber")
d_mapping("Geranium_rotundifolium")
d_mapping("Polygonatum_vertizillatum")
d_mapping("Crepis_vesicaria_taraxacifolia")
d_mapping("Parietaria_judaica")


```

##Habitat mapping

```{r habitat_mapping, echo=FALSE}

h_mapping <- function(habitat){
  
  
  occ.before.1950 <- data_before_1950[data_before_1950$habitat==habitat,c(10,9)]
  cat("\nNumber of records before 1950 :", 
      nrow(occ.before.1950),
      "\nRunning MaxEnt....\n")
  
  if(nrow(occ.before.1950)>1){
    
    #here can use lakes removed files if needed
    #but adding lakes is similar (easy)
    me.before.1950 <- maxent(predictors.before.1950, occ.before.1950)
    print(me.before.1950)
    #me.1864
    #plot(me.1864)
    
    ped.before.1950 <- predict(me.before.1950, predictors.before.1950)  # studyArea is the clipped rasters 
    
    # par(mfrow=c(2,2))
    
    # plot(ped.before.1950,main="Distribution : Before 1950")
    # plot(st_geometry(lakes),add=T,col="lightblue", border = 'blue')
    
    
  }
  
  occ.after.1950 <- data_after_1950[data_after_1950$habitat==habitat,c(10,9)]
  cat("\nNumber of records after 1950 :", nrow(occ.after.1950),"\nRunning MaxEnt....\n")
  
  if(nrow(occ.after.1950)>1) {
    me.after.1950 <- maxent(predictors.after.1950, occ.after.1950)
    print(me.after.1950)
    #me.1864
    #plot(me.1864)
    
    ped.after.1950 <- predict(me.after.1950, predictors.after.1950)  # studyArea is the clipped rasters 
    # plot(ped.after.1950,main="Distribution : After 1950")
    # plot(st_geometry(lakes),add=T,col="lightblue", border = 'blue')
    
  }
  
  par(mfrow=c(2,2))
  
  max<- ifelse(max(values(ped.before.1950),na.rm = T)>max(values(ped.after.1950),na.rm = T),
               max(values(ped.before.1950),na.rm = T),
               max(values(ped.after.1950),na.rm = T))
  
  plot(ped.before.1950,main="Distribution : Before 1950",zlim=c(0,max))
  plot(st_geometry(lakes[order(-lakes$area)[1:874],]),
       add=T,col="lightblue", border = 'blue')
  
  
  plot(ped.after.1950,main="Distribution : After 1950",zlim=c(0,max))
  plot(st_geometry(lakes[order(-lakes$area)[1:874],]),
       add=T,col="lightblue", border = 'blue')
  
  if(nrow(occ.before.1950)>1&nrow(occ.after.1950)>1) {
    
    plot(ped.after.1950-ped.before.1950 , main="Change from before 1950 to after 1950" )
    plot(st_geometry(lakes[order(-lakes$area)[1:874],]),
         add=T,col="lightblue", border = 'blue')
    
  }
  
  print(habitat)
  cat("\nNumber of records before 1950 :", 
      nrow(occ.before.1950))
  cat("\nNumber of records after 1950 :", nrow(occ.after.1950))
  
}



```

```{r}
h_mapping("Moist grasslands")
h_mapping("Semi-arid grassland")
h_mapping("Perennial ruderals")
h_mapping("Tall herb fringe")
h_mapping("Annual ruderals")

```

