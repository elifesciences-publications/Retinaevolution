####################################################
### R-script for retinal oxygen supply evolution ###
####################################################

###################
### Import data ###
###################

# Load Packages

Packages                 <-c("phyloseq","splitstackshape","RColorBrewer","ggsignif","ape","cowplot","readxl","geiger","phytools","ggplot2","nlme","coda","gridExtra","grid","phangorn")
lapply(Packages, library, character.only = TRUE) 

# Set working directory, import data and tree
setwd("/Users/christiandamsgaard/Dropbox/Projects/Piscine_retina_elife/")
data                     <-as.data.frame(read_excel("./Data/data.xlsx", sheet = "Sheet1"))
data$Species             <-sub("\\ ", "_", data$Species)                      # Separate genus and species by underscore
data$Root                <-data$Root*100                                      # Root effect in percent
data$SAD<-data$`Surface area [mm2]`
tax                      <- read.csv("./Data/PFC_short_classification.csv")                              # Load the fish taxonomy
tax$genus.species             <-sub("\\ ", "_", tax$genus.species)                      # Separate genus and species by underscore
tax[which(tax$genus.species=="Astyanax_mexicanus"),]<-c("Characiformes","Characidae","Astyanax","Astyanax_mexicanus_Surface")
tax[which(tax$genus.species=="Astyanax_jordani"),]<-c("Characiformes","Characidae","Astyanax","Astyanax_mexicanus_Chica")
tax2<-as.data.frame(rbind(c("Characiformes","Characidae","Astyanax","Astyanax_mexicanus_Micos"),
                          c("Characiformes","Characidae","Astyanax","Astyanax_mexicanus_Pachon")))
colnames(tax2)           <-colnames(tax)
tax                      <-rbind(tax2,tax)
data$Order               <-as.character(tax$order[match(data$Species,tax$genus.species)])


####################################################
### Table S1: De novo and literature data points ###
####################################################

fish<-data[which(data$Class=="Actinopterygii"),]                                                   # Whole data set, excluding all non-ray-finned fishes
not.fish<-data[which(data$Class!="Actinopterygii"),]                                               # Whole data set, excluding all ray-finned fishes                      

T1                       <-matrix(ncol = 9,nrow = 10)                                               # Table 1
colnames(T1)             <-c("De novo fishes","De novo others","De novo all",
                             "Literature fishes","Literature others","Literature all",
                             "Total fishes","Total others","Total all")

rownames(T1)             <-c("Root effect magnitude","Retinal Thickness","Retinal layers thicknesses",
                             "Choroid rete mirabile surface area","Magnitude of pre-retinal capillarization",
                             "Presence of choroid rete mirabile","Presence of intra-retinal capillaries","Presence of pre-retinal capillaries",
                             "Eye mass","Total")

## ROOT EFFECT
  length(which(fish$Root_ref[!is.na(fish$Root_ref)]=="This study"))                    ->T1[1,1]   # De novo ray-finned fishes
  length(which(not.fish$Root_ref[!is.na(not.fish$Root_ref)]=="This study"))            ->T1[1,2]   # De novo other vertebrates
  length(which(data$Root_ref[!is.na(data$Root_ref)]=="This study"))                    ->T1[1,3]   # De novo all
  length(which(fish$Root_ref[!is.na(fish$Root_ref)]!="This study"))                    ->T1[1,4]   # Literature ray-finned fishes
  length(which(not.fish$Root_ref[!is.na(not.fish$Root_ref)]!="This study"))            ->T1[1,5]   # Literature other vertebrates
  length(which(data$Root_ref[!is.na(data$Root_ref)]!="This study"))                    ->T1[1,6]   # Literature all
  length(fish$Root_ref[!is.na(fish$Root_ref)])                                         ->T1[1,7]   # Total ray-finned fishes
  length(not.fish$Root_ref[!is.na(not.fish$Root_ref)])                                 ->T1[1,8]   # Total other vertebrates
  length(data$Root_ref[!is.na(data$Root_ref)])                                         ->T1[1,9]   # Total all

## MAXIMAL RETINAL THICKNESS 
  length(which(fish$Thickness_ref[!is.na(fish$Thickness_ref)]=="This study"))          ->T1[2,1]   # De novo ray-finned fishes
  length(which(not.fish$Thickness_ref[!is.na(not.fish$Thickness_ref)]=="This study"))  ->T1[2,2]   # De novo other vertebrates
  length(which(data$Thickness_ref[!is.na(data$Thickness_ref)]=="This study"))          ->T1[2,3]   # De novo all
  length(which(fish$Thickness_ref[!is.na(fish$Thickness_ref)]!="This study"))          ->T1[2,4]   # Literature ray-finned fishes
  length(which(not.fish$Thickness_ref[!is.na(not.fish$Thickness_ref)]!="This study"))  ->T1[2,5]   # Literature other vertebrates
  length(which(data$Thickness_ref[!is.na(data$Thickness_ref)]!="This study"))          ->T1[2,6]   # Literature all
  length(fish$Thickness_ref[!is.na(fish$Thickness_ref)])                               ->T1[2,7]   # Total ray-finned fishes
  length(not.fish$Thickness_ref[!is.na(not.fish$Thickness_ref)])                       ->T1[2,8]   # Total other vertebrates
  length(data$Thickness_ref[!is.na(data$Thickness_ref)])                               ->T1[2,9]   # Total all
  
## RETINAL LAYER THICKNESSES
  length(fish$NFL[!is.na(fish$NFL)])                                                   ->T1[3,c(1,7)]# De novo ray-finned fishes
  length(not.fish$NFL[!is.na(not.fish$NFL!=0)])                                        ->T1[3,c(2,8)]# De novo other vertebrates
  length(data$NFL[!is.na(data$NFL!=0)])                                                ->T1[3,c(3,9)]# De novo all
  0 ->T1[3,4]->T1[3,5]->T1[3,6]

## CHOROID RETE MIRABILE SURFACE AREA
  length(fish$SAD[!is.na(fish$SAD)])                                                   ->T1[4,c(1,7)]# De novo ray-finned fishes
  length(not.fish$SAD[!is.na(not.fish$SAD!=0)])                                        ->T1[4,c(2,8)]# De novo other vertebrates
  length(data$SAD[!is.na(data$SAD!=0)])                                                ->T1[4,c(3,9)]# De novo all
  0 ->T1[4,4]->T1[4,5]->T1[4,6]
  
## MAGNITUDE OF PRE-RETINAL CAPILLARIZATION
  length(which(fish$PRC[!is.na(fish$PRC)]!=0))                                         ->T1[5,c(1,7)]# De novo ray-finned fishes
  length(which(not.fish$PRC[!is.na(not.fish$PRC)]!=0))                                 ->T1[5,c(2,8)]# De novo other vertebrates
  length(which(data$PRC[!is.na(data$PRC)]!=0))                                         ->T1[5,c(3,9)]# De novo all
  0 ->T1[5,4]->T1[5,5]->T1[5,6]

## PRESENCE OF CHOROID RETE MIRABILE
  length(which(fish$CRM_ref[!is.na(fish$CRM_ref)]=="This study"))                      ->T1[6,1]   # De novo ray-finned fishes
  length(which(not.fish$CRM_ref[!is.na(not.fish$CRM_ref)]=="This study"))              ->T1[6,2]   # De novo other vertebrates
  length(which(data$CRM_ref[!is.na(data$CRM_ref)]=="This study"))                      ->T1[6,3]   # De novo all
  length(which(fish$CRM_ref[!is.na(fish$CRM_ref)]!="This study"))                      ->T1[6,4]   # Literature ray-finned fishes
  length(which(not.fish$CRM_ref[!is.na(not.fish$CRM_ref)]!="This study"))              ->T1[6,5]   # Literature other vertebrates
  length(which(data$CRM_ref[!is.na(data$CRM_ref)]!="This study"))                      ->T1[6,6]   # Literature all
  length(fish$CRM_ref[!is.na(fish$CRM_ref)])                                           ->T1[6,7]   # Total ray-finned fishes
  length(not.fish$CRM_ref[!is.na(not.fish$CRM_ref)])                                   ->T1[6,8]   # Total other vertebrates
  length(data$CRM_ref[!is.na(data$CRM_ref)])                                           ->T1[6,9]   # Total all


## INTRA-RETINAL CAPILLARIES
  length(which(fish$IRC_ref[!is.na(fish$IRC_ref)]=="This study"))                      ->T1[7,1]   # De novo ray-finned fishes
  length(which(not.fish$IRC_ref[!is.na(not.fish$IRC_ref)]=="This study"))              ->T1[7,2]   # De novo other vertebrates
  length(which(data$IRC_ref[!is.na(data$IRC_ref)]=="This study"))                      ->T1[7,3]   # De novo all
  length(which(fish$IRC_ref[!is.na(fish$IRC_ref)]!="This study"))                      ->T1[7,4]   # Literature ray-finned fishes
  length(which(not.fish$IRC_ref[!is.na(not.fish$IRC_ref)]!="This study"))              ->T1[7,5]   # Literature other vertebrates
  length(which(data$IRC_ref[!is.na(data$IRC_ref)]!="This study"))                      ->T1[7,6]   # Literature all
  length(fish$IRC_ref[!is.na(fish$IRC_ref)])                                           ->T1[7,7]   # Total ray-finned fishes
  length(not.fish$IRC_ref[!is.na(not.fish$IRC_ref)])                                   ->T1[7,8]   # Total other vertebrates
  length(data$IRC_ref[!is.na(data$IRC_ref)])                                           ->T1[7,9]   # Total all
  
## PRE-RETINAL CAPILLARISATION
  length(which(fish$PRC_ref[!is.na(fish$PRC_ref)]=="This study"))                      ->T1[8,1]   # De novo ray-finned fishes
  length(which(not.fish$PRC_ref[!is.na(not.fish$PRC_ref)]=="This study"))              ->T1[8,2]   # De novo other vertebrates
  length(which(data$PRC_ref[!is.na(data$PRC_ref)]=="This study"))                      ->T1[8,3]   # De novo all
  length(which(fish$PRC_ref[!is.na(fish$PRC_ref)]!="This study"))                      ->T1[8,4]   # Literature ray-finned fishes
  length(which(not.fish$PRC_ref[!is.na(not.fish$PRC_ref)]!="This study"))              ->T1[8,5]   # Literature other vertebrates
  length(which(data$PRC_ref[!is.na(data$PRC_ref)]!="This study"))                      ->T1[8,6]   # Literature all
  length(fish$PRC_ref[!is.na(fish$PRC_ref)])                                           ->T1[8,7]   # Total ray-finned fishes
  length(not.fish$PRC_ref[!is.na(not.fish$PRC_ref)])                                   ->T1[8,8]   # Total other vertebrates
  length(data$PRC_ref[!is.na(data$PRC_ref)])                                           ->T1[8,9]   # Total all

## EYE MASS
  length(which(fish$EM_ref[!is.na(fish$EM_ref)]=="This study"))                        ->T1[9,1]   # De novo ray-finned fishes
  length(which(not.fish$EM_ref[!is.na(not.fish$EM_ref)]=="This study"))                ->T1[9,2]   # De novo other vertebrates
  length(which(data$EM_ref[!is.na(data$EM_ref)]=="This study"))                        ->T1[9,3]   # De novo all
  length(which(fish$EM_ref[!is.na(fish$EM_ref)]!="This study"))                        ->T1[9,4]   # Literature ray-finned fishes
  length(which(not.fish$EM_ref[!is.na(not.fish$EM_ref)]!="This study"))                ->T1[9,5]   # Literature other vertebrates
  length(which(data$EM_ref[!is.na(data$EM_ref)]!="This study"))                        ->T1[9,6]   # Literature all
  length(fish$EM_ref[!is.na(fish$EM_ref)])                                             ->T1[9,7]   # Total ray-finned fishes
  length(not.fish$EM_ref[!is.na(not.fish$EM_ref)])                                     ->T1[9,8]   # Total other vertebrates
  length(data$EM_ref[!is.na(data$EM_ref)])                                             ->T1[9,9]   # Total all



## TOTAL NUMBER OF SPECIES IN DE NOVO DATA POINTS
  # Ray-finned fishes
  length(
    unique(c(which(fish$EM_ref=="This study"),
             which(fish$SAD>0),
             which(fish$PRC>0),
             which(fish$GCL>0),
             which(fish$CRM_ref=="This study"),
             which(fish$IRC_ref=="This study"),
             which(fish$Root_ref=="This study"),
             which(fish$Thickness_ref=="This study")
    ))
  )->T1[10,1]
  
  # All vertebrates except ray-finned fishes
  length(
    unique(c(which(not.fish$EM_ref=="This study"),
             which(not.fish$SAD>0),
             which(not.fish$PRC>0),
             which(not.fish$GCL>0),
             which(not.fish$CRM_ref=="This study"),
             which(not.fish$IRC_ref=="This study"),
             which(not.fish$Root_ref=="This study"),
             which(not.fish$Thickness_ref=="This study")
    ))
  )->T1[10,2]
  
  # Total number of species
  # All vertebrates except ray-finned fishes
  length(
    unique(c(which(data$EM_ref=="This study"),
             which(data$SAD>0),
             which(data$PRC>0),
             which(data$GCL>0),
             which(data$CRM_ref=="This study"),
             which(data$IRC_ref=="This study"),
             which(data$Root_ref=="This study"),
             which(data$Thickness_ref=="This study")
    ))
  )->T1[10,3]
  

## TOTAL NUMBER OF SPECIES WITH LITERATURE DATA POINTS
  # Ray-finned fishes
  length(
    unique(c(which(fish$EM_ref!="This study"),
             which(fish$CRM_ref!="This study"),
             which(fish$IRC_ref!="This study"),
             which(fish$Root_ref!="This study"),
             which(fish$Thickness_ref!="This study")
    ))
  )->T1[10,4]
  
  # All vertebrates except ray-finned fishes
  length(
    unique(c(which(not.fish$EM_ref!="This study"),
             which(not.fish$CRM_ref!="This study"),
             which(not.fish$IRC_ref!="This study"),
             which(not.fish$Root_ref!="This study"),
             which(not.fish$Thickness_ref!="This study")
    ))
  )->T1[10,5]
  
  # Total number of species
  length(
    unique(c(which(data$EM_ref!="This study"),
             which(data$CRM_ref!="This study"),
             which(data$IRC_ref!="This study"),
             which(data$Root_ref!="This study"),
             which(data$Thickness_ref!="This study")
    ))
  )->T1[10,6]


## TOTAL NUMBER OF SPECIES WITH WHOLE DATA SET
length(fish$Species)->T1[10,7]
length(not.fish$Species)->T1[10,8]
length(data$Species)->T1[10,9]


T1 # Table S1




# How many species were used to identify capillaries and Root effect in our data set?
length(data$Species[unique(c(which(data$CRM_ref=="This study"),
                             which(data$IRC_ref=="This study"),
                             which(data$Root_ref=="This study"),
                             which(data$Thickness_ref=="This study")))])





########################
### Custom functions ###
########################

# Prune phylogeny
keep.tip                 <-function(tree,tip) drop.tip(tree,setdiff(tree$tip.label,tip))

# Export 3D coordinates
coordinate               <-function(tree,ace,tip) {
  tree_ace<-drop.tip(tree,tree$tip.label[-match(names(tip),tree$tip.label)])
  fit<-ace
  hor <- as.data.frame(tree_layout(tree_ace)$edgeDT)
  ver <- as.data.frame(tree_layout(tree_ace)$vertDT)
  H<-matrix(nrow = length(hor$V1), ncol = 6)
  for (i in 1:length(H[,1])) { H[i,1] <- hor$xleft[i] }
  for (i in 1:length(H[,1])) { H[i,2] <- hor$xright[i] }
  for (i in 1:length(H[,1])) { H[i,3] <- hor$y[i] }
  for (i in 1:length(H[,1])) { H[i,4] <- hor$y[i] }
  for (i in 1:length(H[,1])) { H[i,5] <- unname(fit[match(hor$V1[i],names(fit))]) }
  for (i in 1:length(H[,1])) { H[i,6] <- ifelse(!is.na(hor$OTU[i]),
                                                unname(tip[match(hor$OTU[i],names(tip))]),
                                                unname(fit[match(hor$V2[i],names(fit))]))}
  V<-matrix(nrow = length(ver$V1), ncol = 6)
  for (i in 1:length(V[,1])) { V[i,1] <- tree_layout(tree_ace)$vertDT$x[i] }
  for (i in 1:length(V[,1])) { V[i,2] <- tree_layout(tree_ace)$vertDT$x[i] }
  for (i in 1:length(V[,1])) { V[i,3] <- tree_layout(tree_ace)$vertDT$vmin[i] }
  for (i in 1:length(V[,1])) { V[i,4] <- tree_layout(tree_ace)$vertDT$vmax[i] }
  for (i in 1:length(V[,1])) { V[i,5] <- unname(fit[match(ver$V1[i],names(fit))]) }
  for (i in 1:length(V[,1])) { V[i,6] <- unname(fit[match(ver$V1[i],names(fit))]) }
  
  D<-as.data.frame(rbind(H,V));colnames(D) <- c("x1","x2","y1","y2","z1","z2")
  return(D)
}

# Pick model of discrete character evolution using likelihood ratio test
LRT.pick.model           <- function (tree,x) {
  ace_ER <- fitDiscrete(tree, x, type="discrete", model = "ER")  
  ace_SYM <-fitDiscrete(tree, x, type="discrete", model = "SYM")  
  ace_ARD <-fitDiscrete(tree, x, type="discrete", model = "ARD")  
  S                        <-c(ace_ER$opt$lnL,ace_ER$opt$k,ace_ER$opt$aicc,
                               ace_SYM$opt$lnL,ace_SYM$opt$k,ace_SYM$opt$aicc,
                               ace_ARD$opt$lnL,ace_ARD$opt$k,ace_ARD$opt$aicc,
                               pchisq(abs(2*(ace_ER$opt$lnL-ace_ARD$opt$lnL)) , 
                                      ace_ARD$opt$k-ace_ER$opt$k, lower.tail=FALSE), 
                               pchisq(abs(2*(ace_ER$opt$lnL-ace_SYM$opt$lnL)) , 
                                      ace_SYM$opt$k-ace_ER$opt$k, lower.tail=FALSE) ,
                               pchisq(abs(2*(ace_SYM$opt$lnL-ace_ARD$opt$lnL)) , 
                                      ace_ARD$opt$k-ace_SYM$opt$k, lower.tail=FALSE))
  names(S)<-c("lnL_ER","df_ER","AICc_ER","lnL_SYM","df_SYM","AICc_SYM","lnL_ARD","df_ARD","AICc_ARD",
              "p_ERvsARD","p_ERvsSYM","p_SYMvsARD")
  ARDvsER <-ifelse(S[10]<=0.05 & S[9]<S[3],"ARD", "ER") #ARD vs ER
  SYMvsER <-ifelse(S[11]<=0.05 & S[6]<S[3],"SYM", "ER") #SYM vs ER
  ARDvsSYM<-ifelse(S[12]<=0.05 & S[9]<S[6],"ARD","SYM") #ARD vs SYM
  model<-ifelse(test = ARDvsSYM=="SYM",yes = SYMvsER,no = ARDvsER)
  model<-unname(model)
  return(c(model,S))
}

  # Calculate divergence times
divergencetimes          <- function (phy,x,ACE) {
  hor <- as.data.frame(tree_layout(phy)$edgeDT)
  H<-matrix(nrow = length(hor$V1), ncol = 7)
  for (i in 1:length(H[,1])) { H[i,1] <- max(hor$xright)-hor$xleft[i] }  # branch in phylogeny goes from this age
  for (i in 1:length(H[,1])) { H[i,2] <- max(hor$xright)-hor$xright[i] } # to this age
  for (i in 1:length(H[,1])) { H[i,4] <- ACE[match(hor$V1[i],row.names(ACE))]} # Posterior probability at first point
  for (i in 1:length(H[,1])) { H[i,5] <- ACE[match(hor$V2[i],row.names(ACE))]} # Posterior probability at second point
  for (i in 1:length(H[,1])) { H[i,5] <- ifelse(is.na(ACE[match(hor$V2[i],row.names(ACE))])==T,
                                                unname(ifelse(x[match(phy$tip[hor$V2[i]],names(x))]==0,1,0)),
                                                H[i,5])} # Posterior probability at second point
  for (i in 1:length(H[,1])) { H[i,6] <- ifelse(H[i,4]<0.5&H[i,5]>0.5,
                                                (0.5-lm(c(H[i,4],H[i,5])~c(H[i,1],H[i,2]))$coefficients[1])/lm(c(H[i,4],H[i,5])~c(H[i,1],H[i,2]))$coefficients[2],
                                                NA)}
  for (i in 1:length(H[,1])) { H[i,7] <- ifelse(H[i,4]>0.5&H[i,5]<0.5,
                                                (0.5-lm(c(H[i,4],H[i,5])~c(H[i,1],H[i,2]))$coefficients[1])/lm(c(H[i,4],H[i,5])~c(H[i,1],H[i,2]))$coefficients[2],
                                                NA)}
  wa<-H[,6];wa<-wa[!is.na(wa)]
  aw<-H[,7];aw<-aw[!is.na(aw)]
  
  M<-matrix(ncol = 2,nrow = (length(aw)+length(wa)))
  for (i in 1:length(wa)) { M[i,1]<-"Loss"} 
  for (i in 1:length(aw)) { j=i+length(wa);  M[j,1]<-"Gain"} 
  for (i in 1:length(wa)) { M[i,2]<-wa[i]} 
  for (i in 1:length(aw)) { j=i+length(wa);  M[j,2]<-aw[i]} 
  
  df<-as.data.frame(M)
  colnames(df)<-c("mode","time")
  df$time<-as.numeric(as.character(df$time))
  
  # Add label with time of transition to density map
  k<-cbind(hor$V1[as.numeric(row.names((subset(as.data.frame(H),is.na(H[,6])==F))))],
           hor$V2[as.numeric(row.names((subset(as.data.frame(H),is.na(H[,6])==F))))]);k
  l<-cbind(hor$V1[as.numeric(row.names((subset(as.data.frame(H),is.na(H[,7])==F))))],
           hor$V2[as.numeric(row.names((subset(as.data.frame(H),is.na(H[,7])==F))))]);l
  g<-as.data.frame(phy$edge)
  
  if (length(k[,2])>0) {
    wa.edges<-matrix(ncol=1,nrow=length(k[,2]))
    wa.nodes<-k[,2]
    for (i in 1:length(k[,1])) {
      wa.edges[i]<-rownames(subset(g,g$V1==k[i,1]&g$V2==k[i,2]))
    }
  }
  
  if (length(l[,2])>0) {
    aw.edges<-matrix(ncol=1,nrow=length(l[,2]))
    aw.nodes<-l[,2]
    for (i in 1:length(l[,1])) {
      aw.edges[i]<-rownames(subset(g,g$V1==l[i,1]&g$V2==l[i,2]))
    }}
  
  df<-cbind(c(wa.edges,aw.edges),c(wa.nodes,aw.nodes),df)
  colnames(df)[1:2]<-c("edge","node")
  return(df)
}

# Set color theme
cols                     <- brewer.pal(8,"Dark2")


########################################
### Build the phylogenetic tree data ###
########################################

# Import phylogenetic tree of Ray-finned fishes
tree                     <-read.tree("/Users/christiandamsgaard/Dropbox/Projects/Air-breathing review_full/mcc.nexus") # MCC tree from Rabosky, download from github page
tree                     <-drop.tip(tree,c("Lepidosiren_paradoxa","Neoceratodus_forsteri","Latimeria_chalumnae","Protopterus_aethiopicus_annectens"))
hor                      <-as.data.frame(tree_layout(tree)$edgeDT);       
actin_crown              <-max(max(hor$xright)-hor$xleft)
tree$tip.label[which(tree$tip.label=="Astyanax_mexicanus")]<-"Astyanax_mexicanus_Surface"
tree$tip.label[which(tree$tip.label=="Astyanax_jordani")]<-"Astyanax_mexicanus_Chica"

# Add Astyanax mexicanus Pachon to the tree
tree                     <-bind.tip(tree = tree,
                                    tip.label = "Astyanax_mexicanus_Pachon",
                                    edge.length = 3,
                                    where = match("Astyanax_mexicanus_Surface",
                                                  tree$tip.label,
                                                  nomatch = NA_integer_, 
                                                  incomparables = NULL),
                                    position = 3)

# Add Astyanax mexicanus Micos to the tree
tree                     <-bind.tip(tree = tree,
                                    tip.label = "Astyanax_mexicanus_Micos",
                                    edge.length = 0.03,
                                    where = match("Astyanax_mexicanus_Surface",tree$tip.label,
                                                  nomatch = NA_integer_, 
                                                  incomparables = NULL),
                                    position = 0.03)

# Load sarcopterygean tree
sarc                     <-read.tree(text = "(((((((Pseudemys_scripta:44.1,Clemmys_guttata:44.1):74.5,(Chelydra_serpentina:113.1,Chelonia_mydas:113.1):5.5):145.9,(((((Puffinus_griseus:66.7,Larus_philadelphia:66.7):2.5,Aquila_rapax:69.2):0,(Streptopelia_capicola:39.1,Columba_livia:39.1):30.1):19.4,Gallus_gallus:88.6):150.5,(Crocodylus_acutus:33,Alligator_mississippiensis:33):206):25.5):35.3,((((((Thamnophis_melanogaster:35.5,Natrix_natrix:35.5):17.5,Coluber_constrictor:53):8.4,Agkistrodon_piscivorus:61.4):31.3,((Python_regius:16.7,Python_molurus:16.7):51,Boa_constrictor:67.7):25):91.9,((Iguana_iguana:26.7,Amblyrhynchus_cristatus:26.7):64.6,Phrynosoma_cornutum:91.3):93.3):17.5,Tarentola_mauritanica:202.1):97.7):29.3,(((((((Rattus_norvegicus:11.8,Mus_musculus:11.8):10,Microtus_pennsylvanicus:21.8):45.2,(Octodon_degus:38.1,Cavia_cutleri:38.1):28.9):10.1,Oryctolagus_cuniculus:77.1):4.5,((Macaca_mulatta:21.1,Homo_sapiens:21.1):10.2,Aotus_trivirgatus:31.3):50.3):8,((Sus_scrofa:81.2,Felis_domesticus:81.2):0,(Pteropus_vampyrus:65.6,Desmodus_rotundus:65.6):15.6):8.4):97.1,(Trichosurus_vulpecula:67.7,Dasyurus_viverrinus:67.7):119):142.4):40.2,(((((Rana_temporaria:50.6,Rana_catesbeiana:50.6):112.8,Bufo_americanus:163.4):54.1,Xenopus_laevis:217.5):5.7,(Bombinator_pachypus:199.4,Alytes_obstetricans:199.4):23.8):57.1,Ambystoma_mexicanum:280.3):89):41.4,((Protopterus_annectens:103.2,Lepidosiren_paradoxa:103.2):138.6,Neoceratodus_forsteri:241.8):168.9);")
plot(sarc)
edgelabels(sarc$edge.length,frame = "none")
plot(mamm)
edgelabels(mamm$edge.length,frame = "none")
# Add Latimeria_chalumnae to the sarcopterygean tree
sarc                     <-bind.tip(tree = sarc,
                                    tip.label = "Latimeria_chalumnae",
                                    edge.length = 409.4,
                                    where = 94,
                                    position = 167.6)

# Add mammals
mamm                     <-read.nexus(file = "./Data/composite mammal phylogeny.nex")
mamm$root.edge<-113.5242
mamm$edge.length<-mamm$edge.length*100
sarc_m<-drop.tip(sarc,data$Species[which(data$Class=="Mammalia")])
sarc<-bind.tree(sarc_m,mamm,where = 37,position = 29.3)



write.tree(sarc)
sarc<-read.tree(text = "(((((((Pseudemys_scripta:44.1,Clemmys_guttata:44.1):74.5,(Chelydra_serpentina:113.1,Chelonia_mydas:113.1):5.5):145.9,(((((Puffinus_griseus:66.7,Larus_philadelphia:66.7):2.5,Aquila_rapax:69.2):0,(Streptopelia_capicola:39.1,Columba_livia:39.1):30.1):19.4,Gallus_gallus:88.6):150.5,(Crocodylus_acutus:33,Alligator_mississippiensis:33):206):25.5):35.3,((((((Thamnophis_melanogaster:35.5,Natrix_natrix:35.5):17.5,Coluber_constrictor:53):8.4,Agkistrodon_piscivorus:61.4):31.3,((Python_regius:16.7,Python_molurus:16.7):51,Boa_constrictor:67.7):25):91.9,((Iguana_iguana:26.7,Amblyrhynchus_cristatus:26.7):64.6,Phrynosoma_cornutum:91.3):93.3):17.5,Tarentola_mauritanica:202.1):97.7):29.3,((Tachyglossus_aculeatus:46.7284,(Ornithorhynchus_anatinus:13.7,Zaglossus_bruijni:13.7):33.0284):168.8472,(((Didelphis_virginiana:29.06942,Marmosa_mexicana:29.06942):51.14518,(((((((Dasyurus_viverrinus:9.43491,Sarcophilus_harrisii:9.43491):9.08945,Antechinus_godmani:18.52436):6.39918,Sminthopsis_crassicaudata:24.92354):8.64746,Myrmecobius_fasciatus:33.571):5.01025,Thylacinus_cynocephalus:38.58125):25.67055,(Macrotis_lagotis:30.379,(Isoodon_macrourus:1.37071,Isoodon_obesulus:1.37071):29.00829):33.8728):3.4661,((Dendrolagus_bennettianus:52.829,(Tarsipes_rostratus:44.176,Petaurus_breviceps:44.176):8.6529):1.633,Trichosurus_vulpecula:54.462):13.256):12.4968):106.4645,((((Erinaceus_europaeus:70.1469,Talpa_europaea:70.1469):13.161,(((((Felis_domesticus:11.9,Acinonyx_jubatus:11.9):4.4,Panthera_leo:16.3):17.0576,(Viverra_zibetha:32.5906,(Hyaena_hyaena:27.6334,Cynictis_penicillata:27.6335):4.9572):0.767):23.3019,(((Canis_lupus:9,Canis_mesomelas:9):7.1,Vulpes_vulpes:16.1):31.8381,((Mephitis_mephitis:32.7652,(Procyon_lotor:29.724,Galictis_cuja:29.724):3.0412):6.5874,(Ursus_arctos:38.0534,Phoca_vitulina:38.0534):1.2992):8.5856):8.7214):24.5781,((Desmodus_rotundus:65.5663,Pteropus_vampyrus:65.5661):15.2307,((Equus_ferus:57.9101,(Ceratotherium_simum:51.8134,Tapirus_terrestris:51.8134):6.0967):22.3459,(Lama_glama:65.3597,(Sus_scrofa:63.9418,(((Bos_taurus:15.9,(Ovis_aries:8,Capra_hircus:8):7.9):2.5893,Moschus_fuscus:18.4893):40.8446,Globicephala_macrorhynchus:59.3339):4.6079):1.4179):14.8962):0.541):0.4409):2.0702):6.2888,((Tupaia_glis:80.4944,(Oryctolagus_cuniculus:77.1082,((Castor_canadensis:63.828,(((Mus_musculus:11.2,Rattus_norvegicus:11.2):8.1,(Meriones_unguiculatus:16.6,Psammomys_obesus:16.6):2.7):2.5087,Mesocricetus_auratus:21.8087):42.0194):3.1847,((Glis_glis:59.7773,(Marmota_flaviventris:8,Spermophilus_citellus:8):51.7773):5.8344,(Hystrix_africaeaustralis:44.6375,((Chinchilla_lanigera:34.9244,(Octodon_degus:24.7992,Myocastor_coypus:24.7993):10.1252):3.2207,(Cuniculus_paca:29.7814,(Dasyprocta_punctata:27.9103,(Cavia_cutleri:18.3,Hydrochoerus_hydrochaeris:18.3):9.6103):1.8711):8.3637):6.4925):20.9742):1.401):10.0954):3.3863):1.0725,(Galeopterus_variegatus:79.3647,((((Macaca_fascicularis:3.44,Macaca_mulatta:3.44):17.6272,Homo_sapiens:21.0672):25.6528,Aotus_trivirgatus:46.72):26.4179,(Otolemur_crassicaudatus:55.1057,Lemur_catta:55.1057):18.0321):6.2269):2.2022):8.0297):9.7154,((Dasypus_novemcinctus:67.7572,(Bradypus_variegatus:57.2717,Myrmecophaga_tridactyla:57.2716):10.4856):29.2448,(Loxodonta_africana:61.4128,Heterohyrax_brucei:61.4127):35.5893):2.3102):87.3669):28.8965):113.5242):40.2,(((((Rana_temporaria:50.6,Rana_catesbeiana:50.6):112.8,Bufo_americanus:163.4):54.1,Xenopus_laevis:217.5):5.7,(Bombinator_pachypus:199.4,Alytes_obstetricans:199.4):23.8):57.1,Ambystoma_mexicanum:280.3):89):41.4,(((Protopterus_annectens:103.2,Lepidosiren_paradoxa:103.2):138.6,Neoceratodus_forsteri:241.8):167.6,Latimeria_chalumnae:409.4):1.3):14;")

# Add Microtus_pennsylvanicus to sarc tree
sarc                     <-bind.tip(tree = sarc,
                                    tip.label = "Microtus_pennsylvanicus",
                                    edge.length = 15.5,
                                    where = match("Mesocricetus_auratus",sarc$tip.label,
                                                  nomatch = NA_integer_, 
                                                  incomparables = NULL),
                                    position = 15.5)






# Add sarcopterygean tree to actinopterygean tree

hor                      <-as.data.frame(tree_layout(sarc)$edgeDT);       
sarc_crown               <-max(max(hor$xright)-hor$xleft)
actin_sarc_dt            <-424.8   # Betancur-R 2017 
sarc$root.edge           <-actin_sarc_dt   -sarc_crown
hor                      <-as.data.frame(tree_layout(tree)$edgeDT)
tree$root.edge           <-actin_sarc_dt-actin_crown
tree                     <-bind.tree(tree,
                                     sarc,
                                     position = actin_sarc_dt-actin_crown)

# add shark phylogeny from timetree
elas                   <-read.tree(text = "(Hydrolagus_colliei:399.3989352,(((Scyliorhinus:139.3745,Mustelus_asterias:139.3745)29:59.6255,Squalus_acanthias:199)39:65.67780412,Dasyatis_centreoura:264.6778041)140:134.7211311);")
hor<-as.data.frame(tree_layout(elas)$edgeDT);elas_crown<-max(max(hor$xright)-hor$xleft)
elas_oste_sarc_dt            <-462.4 # Betancur-R et al. 2015
elas$root.edge           <-elas_oste_sarc_dt-elas_crown
hor<-as.data.frame(tree_layout(tree)$edgeDT);oste_crown<-max(max(hor$xright)-hor$xleft)
tree$root.edge           <-actin_sarc_dt-actin_crown
tree                     <-bind.tree(tree,
                                     elas,
                                     position = elas_oste_sarc_dt-oste_crown)

# add agnathan phylogeny (from timetree)
agna                     <-read.tree(text = "(Myxine_glutinosa:470.51250000,(Lampetra_fluviatilis:16.00000000,Petromyzon_marinus:16.00000000)'14':454.51250000);")
hor                      <-as.data.frame(tree_layout(agna)$edgeDT);agna_crown<-max(max(hor$xright)-hor$xleft)
agna_elas_oste_sarc_dt   <-615 # Timetree
agna$root.edge           <-agna_elas_oste_sarc_dt-agna_crown
hor                      <-as.data.frame(tree_layout(tree)$edgeDT)
gnat_crown               <-max(max(hor$xright)-hor$xleft)
tree$root.edge           <-agna_elas_oste_sarc_dt-gnat_crown
tree                     <-bind.tree(tree,
                                     agna,
                                     position = agna_elas_oste_sarc_dt-gnat_crown)




################ 
### PLOTTING ###
################

## SETUP THEMES ##
# Scatter plot
scatter<-theme(
  # Legend
  #legend.position = c(1,0),
  legend.position = "none",
  legend.key.size = unit(0.10, "cm"),
  legend.justification = c("right", "bottom"),
  legend.box.just = "right",
  
  # Text
  legend.text = element_text(size=6),
  legend.title = element_blank(),
  axis.title.x = element_text(size=6,margin = margin(t = 0)),
  axis.title.y = element_text(size=6,margin = margin(r = 0)),
  axis.text.x = element_text(size=6,margin = margin(t = 0)),
  axis.text.y = element_text(size=6,margin = margin(r = 0)),
  
  # Axis
  axis.ticks = element_blank(),
  axis.line = element_line(size = 2),
  panel.grid.major   = element_line(colour = "grey90",size = 0.2),
  
  # Margin
  plot.margin=unit(rep(0.05,4),"cm")
  )


###################################################################
### Fig. 1 Ancestral state reconstruction of retinal thickness ###
###################################################################

# Named vector of retinal thickness
df<-setNames(data$Thickness,data$Species)
Thickness<-df[!is.na(df)]

# Ancestral state reconstruction using maximum likelihood and an BM model for character evolution
tree.Thickness           <- ladderize(keep.tip(tree,names(Thickness)))

asr_Thickness            <- anc.ML(tree = tree.Thickness,
                                   x = Thickness,
                                   model="BM",
                                   maxit = 100000)


# Plot figure 1A
pdf("./Figures/01_ASR_thickness.pdf",width = 5,height = 7,useDingbats = F)
par(mar = c(2,0,0,0))
plot(tree.Thickness,root.edge = F,use.edge.length = T,cex = 0.5,show.tip.label = T)
title(xlab="Time (MYA)",font.lab=1,cex.lab=0.5,line = 1,adj = .31)
nodelabels(round(asr_Thickness$ace,0),frame = "none",adj = c(1.2,1.3),cex = .5)
axisPhylo(lwd = 1,cex.axis=0.5,padj=-2.5)
dev.off()

df_1                     <- as.data.frame(Thickness)
df_1$PEPRL               <- rep(0,length(df_1$Thickness))
df_1$ONL                 <- rep(0,length(df_1$Thickness))
df_1$OPL                 <- rep(0,length(df_1$Thickness))
df_1$INL                 <- rep(0,length(df_1$Thickness))
df_1$IPL                 <- rep(0,length(df_1$Thickness))
df_1$GCL                 <- rep(0,length(df_1$Thickness))
df_1$NFL                 <- rep(0,length(df_1$Thickness))

for (i in 1:length(df_1$Thickness)) {
  match                  <-match(rownames(df_1)[i],data$Species)
  df_1$PEPRL[i]          <-data$PEPRL[match]
  df_1$ONL[i]            <-data$ONL[match]
  df_1$OPL[i]            <-data$OPL[match]
  df_1$INL[i]            <-data$INL[match]
  df_1$IPL[i]            <-data$IPL[match]
  df_1$GCL[i]            <-data$GCL[match]
  df_1$NFL[i]            <-data$NFL[match]
}

df_1[is.na(df_1)]<-0
df_1$Thickness[df_1$PEPRL>0]=0

df_1b<-
rbind(
  cbind(
    rownames(df_1),
    df_1$PEPRL,
    rep("8.PEPRL",length(df_1$Thickness))
  ),
  cbind(
    rownames(df_1),
    df_1$ONL,
    rep("7.ONL",length(df_1$Thickness))
  ),
  cbind(
    rownames(df_1),
    df_1$OPL,
    rep("6.OPL",length(df_1$Thickness))
  ),
  cbind(
    rownames(df_1),
    df_1$INL,
    rep("5.INL",length(df_1$Thickness))
  ),
  cbind(
    rownames(df_1),
    df_1$IPL,
    rep("4.IPL",length(df_1$Thickness))
  ),
  cbind(
    rownames(df_1),
    df_1$GCL,
    rep("3.GCL",length(df_1$Thickness))
  ),
  cbind(
    rownames(df_1),
    df_1$NFL,
    rep("2.NFL",length(df_1$Thickness))
  ),
  cbind(
    rownames(df_1),
    df_1$Thickness,
    rep("1.Total",length(df_1$Thickness))
  ))
  

df_1b<-as.data.frame(df_1b)
colnames(df_1b)<-c("Species","Thickness","Layer")

df_1b$Species<-as.character(df_1b$Species)
df_1b$Layer<-as.character(df_1b$Layer)
df_1b$Thickness<-as.numeric(as.character(df_1b$Thickness))

is_tip <- tree.Thickness$edge[,2] <= length(tree.Thickness$tip.label)
ordered_tips <- tree.Thickness$edge[is_tip, 2]

ggplot(df_1b, aes(x = Species, y = Thickness, fill = Layer, label = Layer)) +
  geom_bar(stat = "identity",width = 0.8,alpha = 0.8)+
  scale_fill_manual(breaks = c("1.Total","2.PEPRL","3.ONL","4.OPL","5.INL","6.IPL","7.GCL","8.NFL"),
                    #labels = c("Thickness","PEPRL","ONL","OPL","INL","IPL","GCL","NFL"),
                    values = c("grey40",brewer.pal(7,"RdYlBu")))+
  theme(
    #legend.key.size = unit(.07, "cm"),
    #legend.title = element_blank(),
    #legend.text = element_text(size = 6),
    legend.position = "none",
    #legend.justification = c(0,0),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey90",size = 0.2),
    axis.title=element_blank(),
    axis.text=element_blank()
  )+
  scale_x_discrete(limits = tree.Thickness$tip.label[ordered_tips])+
  scale_y_continuous(expand = c(0,0))+
  ylab("Maximal retinal thickness (µm)")+
  coord_flip() 
ggsave("./Figures/01_barplot.pdf",width = 2.8263,height = 6.6008)

# Q: What is the inferred retinal thickness for the MRCA of bony fishes
asr_Thickness$ace[1]
# A: 194 micrometer

# Q: When did this ancestor live?
hor <- as.data.frame(tree_layout(tree.Thickness)$edgeDT);max(max(hor$xright)-hor$xleft)
# A: 425 MYA

# Q: Where does retinal thickness double?
which(asr_Thickness$ace>2*asr_Thickness$ace[1])                                                    # Internal branches where retinal thickness doubles
which(Thickness>2*asr_Thickness$ace[1])                                                            # Terminal branches where retinal thickness doubles
# A: retinal thickness doubled on 6 independent occations; all in terminal branches

# Q: Where does retinal thickness half?
which(asr_Thickness$ace<.5*asr_Thickness$ace[1])                                                    # Internal branches where retinal thickness halved
Thickness[which(Thickness<.5*asr_Thickness$ace[1])]                                                            # Terminal branches where retinal thickness halved
# A: retinal thickness halved on 2 independent occations; both in terminal branches



#########################################################
### Fig. 2 Residual eye mass versus retinal thickness ###
#########################################################


## FIG 2A: LOG(EYEMASS) VS LOG(BODYMASS)

# Prune data set and tree 
df_2a                    <- data.frame(logmass = data$logmass_mean,
                                       logeyemass = data$logeyemass_mean,
                                       Species = data$Species,
                                       Class = data$Class)
row.names(df_2a)            <-df_2a$Species
df_2a                       <-as.data.frame(na.omit(df_2a))
tree_pgls                <-keep.tip(tree,row.names(df_2a))

# Brownian motion model
BM_2a                       <- gls(logeyemass~logmass, 
                                 correlation = corPagel(value = 1,phy = tree_pgls,fixed = F),
                                 method = "ML",
                                 data = df_2a)
summary(BM_2a) # Summary of model


# Ornstein–Uhlenbeck model
OU_2a <- gls(logeyemass~logmass, 
          data=df_2a, 
          correlation=corMartins(2, tree_pgls, fixed=FALSE), method="ML")
summary(OU_2a) # Summary of model


# Model selection based on Akaike weights
aicw(c(AIC(BM_2a),AIC(OU_2a)))
# Conclusion: Use brownian motion model

# Q: Is data heteroscedastic?
plot(BM_2a, resid(., type="n")~fitted(.), col="blue", main="Normalized Residuals vs. Fitted Values",abline=c(0,0)) 
# A: No

# Q: Does the residuals depart from a normal distribution?
rEM                      <-resid(BM_2a, type="n")
qqnorm(rEM)
qqline(rEM) 
# A: No


# Add rEM to the data frame "data" 
rEM                      <-setNames(as.vector(rEM),names(rEM))
data["rEM"]              <-rep(NA,dim(data)[1])
data$rEM[match(names(rEM),data$Species)]<-unname(rEM)

# Plot the data
ggplot(df_2a,aes(x=logmass,y=logeyemass))+
  stat_function(fun = function(x) BM_2a$coefficients[1]+x*BM_2a$coefficients[2], linetype = "solid",col = "black",lwd = 0.2) +
  geom_point(size = 0.5,alpha = 1,pch = 16)+
  scatter+
  labs(x = expression("log"[10]*"(body mass [g])"),
       y = expression("log"[10]*"(eye mass [g])"))->plot2a


## Fig. 2B: Scatter plot of retinal thickness vs. rEM
# Prune data set and tree 
df_2b                       <-data.frame(rEM = data$rEM,
                                      Thickness = data$Thickness,
                                      Species = data$Species)

row.names(df_2b)            <-df_2b$Species
df_2b                       <-as.data.frame(na.omit(df_2b))
tree_pgls                <-keep.tip(tree,row.names(df_2b))

# Brownian motion model
BM_2b                       <- gls(Thickness~rEM, 
                                 correlation = corPagel(value = 0,phy = tree_pgls,fixed = F),
                                 method = "ML",
                                 data = df_2b)
summary(BM_2b) # Summary of model


# Ornstein–Uhlenbeck model model
OU_2b <- gls(Thickness~rEM, 
          data=df_2b, 
          correlation=corMartins(0.00000001, tree_pgls, fixed=FALSE), method="ML")
summary(OU_2b) # Summary of model


# Model selection based on Akaike weights
aicw(c(AIC(BM_2b),AIC(OU_2b)))
# Conclusion: Use # Ornstein–Uhlenbeck model model



# Q: Is data heteroscedastic?
plot(OU_2b, resid(., type="n")~fitted(.), col="blue", main="Normalized Residuals vs. Fitted Values",abline=c(0,0)) 
# A: No

# Q: Does the residuals depart from a normal distribution?
res                      <-resid(OU_2b, type="n")
qqnorm(res)
qqline(res) 
# A: No


# Plot the data
ggplot(df_2b,aes(x=rEM,y=Thickness))+
  scatter+
  stat_function(fun = function(x) OU_2b$coefficients[1]+x*OU_2b$coefficients[2], linetype = "solid",col = "black",lwd = 0.2) +
  labs(x = expression("rEM"),
       y = expression("Maximal retinal thickness (µm)"))+
  geom_point(size = 0.5,alpha = 1,pch = 16)->plot2b;plot2b

## Combine panels to final figure
pdf("./Figures/02_eyesize_thickness.pdf",width = 3,height = 1.5,useDingbats = F)
plot_grid(plot2a,plot2b,ncol = 2, align = 'h',labels = "AUTO",label_size = 12,hjust = c(-2.6,-3),vjust = 1.2)
dev.off()

jpeg(filename = "./Figures/02_eyesize_thickness.jpeg",width = 3,height = 1.5,units = "in",res = 2400)
plot_grid(plot2a,plot2b,ncol = 2, align = 'h',labels = "AUTO",label_size = 12,hjust = c(-2.6,-3),vjust = 1.2)
dev.off()


################################################################
### Fig. 3 FUNCTIONAL DIVERSIFICATION OF RETINAL OXYGENATION ###
################################################################


## EVOLUTION OF CHOROID RETE MIRABILE 
# Named vector of choroid rete mirabile
CRM                      <-setNames(data$CRM,data$Species)
CRM                      <-CRM[CRM!=2]

# Stochastic character mapping (SCM) of choroid rete mirabile. This may take a few hours with 10000 simulations
tree.crm                 <-keep.tip(tree,names(CRM))
SCM_CRM_ER               <-make.simmap(tree = tree.crm,
                                       x = CRM,
                                       nsim=10000,
                                       model = "ER")

pdf("./Figures/Supporting figures/CRM_densitymap_ER.pdf",height = 50,width = 20)
densityMap(SCM_CRM_ER)
dev.off()

# Describe SCM
dSCM_CRM_ER             <-describe.simmap(SCM_CRM_ER, plot = FALSE)

# Find branches where CRM evolved or was lost
dt_CRM                   <-divergencetimes(phy = tree.crm,x = CRM,ACE = dSCM_CRM_ER$ace)
gains_crm                <-as.numeric(as.character(dt_CRM$node[which(dt_CRM$mode=="Gain")]))
losses_crm               <-as.numeric(as.character(dt_CRM$node[which(dt_CRM$mode=="Loss")]))

# Q: When did the CRM evolve and when was it lost?
dt_CRM

# Q: What was the probability for the presence of the choroid rete mirabile at key divergence points?
# MRCA of jawed vertebrates
MRCA                   <-getMRCA(keep.tip(tree,names(CRM)),c("Homo_sapiens","Squalus_acanthias"))# Internal branch number for MRCA
dSCM_CRM_ER$ace[match(MRCA,row.names(dSCM_CRM_ER$ace)),] 
# A: 0.2% 


# MRCA of bony fishes
MRCA                   <-getMRCA(keep.tip(tree,names(CRM)),c("Homo_sapiens","Amia_calva"))       
dSCM_CRM_ER$ace[match(MRCA,row.names(dSCM_CRM_ER$ace)),]
# A: 0.1%


# MRCA of teleosts
MRCA                     <-getMRCA(keep.tip(tree,names(CRM)),c("Anguilla_anguilla","Danio_rerio")) 
dSCM_CRM_ER$ace[match(MRCA,row.names(dSCM_CRM_ER$ace)),]
# A: 94.2%



### Evolution of the intra-retinal capillarization ###
IRC                      <-setNames(data$IRC,data$Species)                                         # Named vector of choroid rete mirabile
IRC                      <-IRC[!is.na(IRC)]
tree.irc                 <-ladderize(keep.tip(tree,names(IRC[IRC!=2])))
SCM_IRC_ER               <-make.simmap(tree = tree.irc,                                            # Stochastic character mapping (SCM) of choroid rete mirabile presence
                                       x = IRC[IRC!=2],
                                       nsim=10000,
                                       model = "ER")

pdf("./Figures/Supporting figures/IRC_densitymap_ER.pdf",height = 50,width = 20)
densityMap(SCM_IRC_ER)
dev.off()


dSCM_IRC_ER              <-describe.simmap(SCM_IRC_ER, plot = FALSE)                             # Describe SCM
transition_IRC           <-divergencetimes(phy = tree.irc,            # Find branches where IRC evolved and was lost
                                           x = IRC[IRC!=2],
                                           ACE = dSCM_IRC_ER$ace)

gains_irc                <-as.numeric(as.character(transition_IRC$node[which(transition_IRC$mode=="Gain")]))    # Branches where IRC evolved 
losses_irc               <-as.numeric(as.character(transition_IRC$node[which(transition_IRC$mode=="Loss")]))    # Branches where IRC was lost 











### Evolution of the intra-retinal capillarization ###
IRC                      <-setNames(data$IRC,data$Species)                                         # Named vector of intra retinal capillaries
IRC                      <-IRC[!is.na(IRC)]
tree.irc                 <-ladderize(keep.tip(tree,names(IRC[IRC!=2])))
SCM_IRC_ER               <-make.simmap(tree = tree.irc,                                            # Stochastic character mapping (SCM) of intra retinal capillary presence
                                       x = IRC[IRC!=2],
                                       nsim=10000,
                                       model = "ER")

pdf("./Figures/Supporting figures/IRC_densitymap_ER.pdf",height = 50,width = 20)
densityMap(SCM_IRC_ER)
dev.off()


dSCM_IRC_ER              <-describe.simmap(SCM_IRC_ER, plot = FALSE)                             # Describe SCM
transition_IRC           <-divergencetimes(phy = tree.irc,            # Find branches where IRC evolved and was lost
                                           x = IRC[IRC!=2],
                                           ACE = dSCM_IRC_ER$ace)

gains_irc                <-as.numeric(as.character(transition_IRC$node[which(transition_IRC$mode=="Gain")]))    # Branches where IRC evolved 
losses_irc               <-as.numeric(as.character(transition_IRC$node[which(transition_IRC$mode=="Loss")]))    # Branches where IRC was lost 


# Probability for intra-retinal capillarization in MRCA of bony fishes
MRCA                   <-getMRCA(tree.irc,c("Homo_sapiens","Amia_calva"))       
dSCM_IRC_ER$ace[match(MRCA,row.names(dSCM_IRC_ER$ace)),] 
# A: 99.6% probability for absence IRC

# Probability for intra-retinal capillarization in MRCA of extant mammals
MRCA                   <-getMRCA(tree.irc,c("Homo_sapiens","Ornithorhynchus_anatinus"))       
dSCM_IRC_ER$ace[match(MRCA,row.names(dSCM_IRC_ER$ace)),] 
# A: 99.1% probability for IRC






### Evolution of pre-retinal capillarization ###


PRC                      <-setNames(data$PRC_bi,data$Species)                                         # Named vector of pre-retinal capillary presence
PRC                      <-PRC[!is.na(PRC)]
tree.prc                 <-ladderize(keep.tip(tree,names(PRC[PRC!=2])))

SCM_PRC_ER               <-make.simmap(tree = tree.prc,                                            
                                       x = PRC[PRC!=2],
                                       nsim=10000,
                                       model = "ER")

pdf("./Figures/Supporting figures/PRC_densitymap_ER.pdf",height = 50,width = 20)
densityMap(SCM_PRC_ER)
dev.off()


dSCM_PRC_ER              <-describe.simmap(SCM_PRC_ER, plot = FALSE)                             # Describe SCM
transition_PRC           <-divergencetimes(phy = tree.prc,            # Find branches where PRC evolved and was lost
                                           x = PRC[PRC!=2],
                                           ACE = dSCM_PRC_ER$ace)

gains_prc                <-as.numeric(as.character(transition_PRC$node[which(transition_PRC$mode=="Gain")]))    # Branches where PRC evolved 
losses_prc               <-as.numeric(as.character(transition_PRC$node[which(transition_PRC$mode=="Loss")]))    # Branches where PRC was lost 

# Probability for presence of pre-retinal capillarization in MRCA of bony fishes
MRCA                     <-getMRCA(tree.prc,c("Homo_sapiens","Amia_calva"))       
dSCM_PRC_ER$ace[match(MRCA,row.names(dSCM_PRC_ER$ace)),] 
# A: 32.9%






# Generate tree for all species in data set
tree.plot<-ladderize(keep.tip(tree,data$Species),F)
old.tip.labels<-tree.plot$tip.label
tree.plot$root.edge<-35
no.data.species<-data$Species[which(data$CRM==2&data$PRC_bi==2&data$IRC==2)]
tree.plot<-drop.tip(tree.plot,no.data.species)
no.data.species
# Abbreviate tip labels
#species                  <-as.data.frame(tree.plot$tip.label)                                    # Load all species names from the species-level BPP tree
#colnames(species)        <-c("Names")
#a                        <-cSplit(indt = species,splitCols = "Names",sep="_",type.convert=FALSE)  # Separate genus and species name
#tree.plot$tip.label      <-paste(substr(a$Names_1,1,1),".",substr(a$Names_2,1,3),sep = "")

order.labels<-aggregate(Species~Order,data,length)
order.labels<-order.labels$Order[order.labels$Species>6]
order.labels
order.labels<-order.labels[-6]
order.labels

# PLOT THE FIGURE
fig3<-function(x=1){
par(mar = c(2,2,2,2))

## The phylogeny
plot(tree.plot,show.tip.label = F,
     type = "fan",
     lwd = 1,
     cex = 5/12,
     rotate.tree = 0,
     root.edge = T,
     label.offset = 46,
     open.angle = 10)


## Mark larger orders of fishes, mammals and lungfishes
lapply(1:length(order.labels),function(i){
  tip<-data$Species[which(data$Order==order.labels[i])]
  tip<-tip[is.na(match(tip,no.data.species))]
  arc.cladelabels(
    text=order.labels[i],
    node=getMRCA(phy = tree.plot,
                 tip = tip),
    mark.node=FALSE,lwd=1,ln.offset = 1.07,lab.offset = 1.1,cex=0.5,orientation = "horizontal")
})

arc.cladelabels(text="Mammals",node=getMRCA(tree.plot,data$Species[data$Class=="Mammalia"]),mark.node=FALSE,lwd=1,ln.offset = 1.07,lab.offset = 1.1,cex = 0.5,orientation = "horizontal")
arc.cladelabels(text="Lungfishes",node=getMRCA(tree.plot,data$Species[data$Class=="Dipnoi"]),mark.node=FALSE,lwd=1,ln.offset = 1.07,lab.offset = 1.1,cex = 0.5,orientation = "horizontal")

## Mark major branch points in the phylogeny with dot and text
nodelabels(node = getMRCA(tree.plot,c("Homo_sapiens","Amia_calva")),pch=16,cex=1)
nodelabels(node = getMRCA(tree.plot,c("Polypterus_senegalus","Amia_calva")),pch=16,cex=1)
nodelabels(node = getMRCA(tree.plot,c("Anguilla_anguilla","Pangio_kuhlii")),pch=16,cex=1)
nodelabels(node = getMRCA(tree.plot,c("Homo_sapiens","Ambystoma_mexicanum")),pch=16,cex=1)
nodelabels(text = "Bony fishes",node = getMRCA(tree.plot,c("Homo_sapiens","Amia_calva")),frame = "none",cex = 0.5,adj = c(1.2,0))
nodelabels(text = "Ray-finned fishes",node = getMRCA(tree.plot,c("Polypterus_senegalus","Amia_calva")),frame = "none",cex = 0.5,adj = c(1.05,1.05))
nodelabels(text = "Teleosts",node = getMRCA(tree.plot,c("Anguilla_anguilla","Pangio_kuhlii")),frame = "none",cex = 0.5,adj = c(-0.3,0.6))
nodelabels(text = "Tetrapods",node = getMRCA(tree.plot,c("Homo_sapiens","Ambystoma_mexicanum")),frame = "none",cex = 0.5,adj = c(0.7,-2.2))


# Add time axis
obj<-axis(side = 1,pos=-20,at=c(650,550,450,350,250,150,50),cex.axis=0.5,labels=FALSE)
text(sort(obj,decreasing = T),rep(-50,length(obj)),(obj-50),cex=0.5)
text(mean(obj),-75,"Time (MYA)",cex=0.5)

## Tip phenotypes
lapply(1:length(tree.plot$tip.label),function(i){
  # Intra-retinal capillaries  
  phenotype            <-to.matrix(IRC[tree.plot$tip.label],seq=sort(unique(IRC)))[i,]
  if(all(phenotype==c(1,0,0))){
    tiplabels(tip = i,pch = 21, col = "black",cex=.5,offset = 22,bg="white")}
  if(all(phenotype==c(0,1,0))){
    tiplabels(tip = i,pch = 21, col = "black",cex=.5,offset = 22,bg=cols[2])}
  # Choroid rete mirabile  
  phenotype          <-to.matrix(CRM[tree.plot$tip.label],seq=sort(unique(CRM)))[i,]
  if(all(phenotype==c(1,0))){
    tiplabels(tip = i,pch = 21, col = "black",cex=.5,offset = 10,bg="white")}
  if(all(phenotype==c(0,1))){
    tiplabels(tip = i,pch = 21, col = "black",cex=.5,offset = 10,bg=cols[1])}
  # Pre-retinal capillarization
  phenotype            <-to.matrix(PRC[tree.plot$tip.label],seq=sort(unique(PRC)))[i,]
  if(all(phenotype==c(1,0,0))){
    tiplabels(tip = i,pch = 21, col = "black",cex=.5,offset = 34,bg="white")}
  if(all(phenotype==c(0,1,0))){
    tiplabels(tip = i,pch = 21, col = "black",cex=.5,offset = 34,bg=cols[3])}
})


## Pie charts at the MRCA of bony fishes
#nodelabels(node = getMRCA(tree.plot,c("Homo_sapiens","Amia_calva")),piecol = c("white",cols[1]),cex = 1,adj = c(-37,105),
#           pie = dSCM_CRM_ER$ace[match(getMRCA(tree.crm,c("Homo_sapiens","Amia_calva")),row.names(dSCM_CRM_ER$ace)),])
#nodelabels(node = getMRCA(tree.plot,c("Homo_sapiens","Amia_calva")),piecol = c("white",cols[2]),cex = 1,adj = c(37,105),
#           pie = dSCM_IRC_ER$ace[match(getMRCA(tree.irc,c("Homo_sapiens","Amia_calva")),row.names(dSCM_IRC_ER$ace)),])
#nodelabels(node = getMRCA(tree.plot,c("Homo_sapiens","Amia_calva")),piecol = c("white",cols[3]),cex = 1,adj = c(0,50),
#           pie = dSCM_PRC_ER$ace[match(getMRCA(tree.prc,c("Homo_sapiens","Amia_calva")),row.names(dSCM_PRC_ER$ace)),])



## MARK GAINS OF CRM
lapply(1:length(gains_crm),function(i){
  downstream<-tree.crm$tip.label[Descendants(x = tree.crm,
                                             node = gains_crm[i],
                                             type = "tips")[[1]]];downstream
  hor <- as.data.frame(tree_layout(tree.plot)$edgeDT);hor
  # Find MRCA in tree.plot
  if(length(downstream)==1){
    row<-hor[match(downstream,hor$OTU),];row$V1
    df<-as.data.frame(tree.plot$edge);df
    df
    mrca<-as.numeric(rownames(subset(df , V1 == row$V1 & V2 == row$V2)));mrca
  }
  if(length(downstream)>1){
    mrca<-getMRCA(tree.plot,downstream);mrca
    df<-as.data.frame(tree.plot$edge);df
    mrca<-which(df$V2==mrca)[1];mrca
  }
  
  edgelabels(edge=mrca, bg=cols[1],col = "black",cex=1.5,pch = 21)
})

## MARK LOSSES OF CRM
lapply(1:length(losses_crm),function(i){
  downstream<-tree.crm$tip.label[Descendants(x = tree.crm,
                                             node = losses_crm[i],
                                             type = "tips")[[1]]];downstream
  hor <- as.data.frame(tree_layout(tree.plot)$edgeDT);hor
  # Find MRCA in tree.plot
  if(length(downstream)==1){
    row<-hor[match(downstream,hor$OTU),];row$V1
    df<-as.data.frame(tree.plot$edge);df
    df
    mrca<-as.numeric(rownames(subset(df , V1 == row$V1 & V2 == row$V2)));mrca
  }
  if(length(downstream)>1){
    mrca<-getMRCA(tree.plot,downstream);mrca
    df<-as.data.frame(tree.plot$edge);df
    mrca<-which(df$V2==mrca)[1];mrca
  }
  edgelabels(edge=mrca, col=cols[1],cex=1.5,pch = 1)
})


## MARK GAINS OF IRC
lapply(1:length(gains_irc),function(i){
  downstream<-tree.irc$tip.label[Descendants(x = tree.irc,
                                             node = gains_irc[i],
                                             type = "tips")[[1]]];downstream
  hor <- as.data.frame(tree_layout(tree.plot)$edgeDT);hor
  # Find MRCA in tree.plot
  if(length(downstream)==1){
    row<-hor[match(downstream,hor$OTU),];row$V1
    df<-as.data.frame(tree.plot$edge)
    mrca<-as.numeric(rownames(subset(df , V1 == row$V1 & V2 == row$V2)));mrca
  }
  if(length(downstream)>1){
    mrca<-getMRCA(tree.plot,downstream);mrca
    df<-as.data.frame(tree.plot$edge);df
    mrca<-which(df$V2==mrca)[1];mrca
  }
  
  edgelabels(edge=mrca, bg=cols[2],col = "black",cex=1.5,pch = 21)
})


## MARK LOSSES OF IRC
lapply(1:length(losses_irc),function(i){
  downstream<-tree.irc$tip.label[Descendants(x = tree.irc,
                                             node = losses_irc[i],
                                             type = "tips")[[1]]];downstream
  hor <- as.data.frame(tree_layout(tree.plot)$edgeDT);hor
  # Find MRCA in tree.plot
  if(length(downstream)==1){
    row<-hor[match(downstream,hor$OTU),];row$V1
    df<-as.data.frame(tree.plot$edge);df
    df
    mrca<-as.numeric(rownames(subset(df , V1 == row$V1 & V2 == row$V2)));mrca
  }
  if(length(downstream)>1){
    mrca<-getMRCA(tree.plot,downstream);mrca
    df<-as.data.frame(tree.plot$edge);df
    mrca<-which(df$V2==mrca)[1];mrca
  }
  edgelabels(edge=mrca, col=cols[2],cex=1.5,pch = 1)
})


## MARK GAINS OF PRC
lapply(1:length(gains_prc),function(i){
  downstream<-tree.prc$tip.label[Descendants(x = tree.prc,
                                             node = gains_prc[i],
                                             type = "tips")[[1]]];downstream
  hor <- as.data.frame(tree_layout(tree.plot)$edgeDT);hor
  # Find MRCA in tree.plot
  if(length(downstream)==1){
    row<-hor[match(downstream,hor$OTU),];row$V1
    df<-as.data.frame(tree.plot$edge)
    mrca<-as.numeric(rownames(subset(df , V1 == row$V1 & V2 == row$V2)));mrca
  }
  if(length(downstream)>1){
    mrca<-getMRCA(tree.plot,downstream);mrca
    df<-as.data.frame(tree.plot$edge);df
    mrca<-which(df$V2==mrca)[1];mrca
  }
  
  edgelabels(edge=mrca, bg=cols[3],col = "black",cex=1.5,pch = 21)
})


## MARK LOSSES OF PRC
lapply(1:length(losses_prc),function(i){
  downstream<-tree.prc$tip.label[Descendants(x = tree.prc,
                                             node = losses_prc[i],
                                             type = "tips")[[1]]];downstream
  hor <- as.data.frame(tree_layout(tree.plot)$edgeDT);hor
  # Find MRCA in tree.plot
  if(length(downstream)==1){
    row<-hor[match(downstream,hor$OTU),];row$V1
    df<-as.data.frame(tree.plot$edge);df
    df
    mrca<-as.numeric(rownames(subset(df , V1 == row$V1 & V2 == row$V2)));mrca
  }
  if(length(downstream)>1){
    mrca<-getMRCA(tree.plot,downstream);mrca
    df<-as.data.frame(tree.plot$edge);df
    mrca<-which(df$V2==mrca)[1];mrca
  }
  edgelabels(edge=mrca, col=cols[3],cex=1.5,pch = 1)
})

}
pdf("./Figures/03_Functional diversification.pdf",height = 7.5,width = 7.5, useDingbats = F)
fig3()
dev.off()


### Fig. S1 FUNCTIONAL DIVERSIFICATION ###

figs1<-function(x=1){
  par(mar = c(1,1,1,1))
# The phylogeny
plot(tree.plot,
     label.offset = 40,
     lwd = 1,
     cex = 0.5,
     rotate.tree = 0,
     root.edge = T)

## Tip phenotypes
lapply(1:length(tree.plot$tip.label),function(i){
  # Intra-retinal capillaries  
  phenotype            <-to.matrix(IRC[tree.plot$tip.label],seq=sort(unique(IRC)))[i,]
  if(all(phenotype==c(1,0,0))){
    tiplabels(tip = i,pch = 21, col = "black",cex=.5,offset = 22,bg="white")}
  if(all(phenotype==c(0,1,0))){
    tiplabels(tip = i,pch = 21, col = "black",cex=.5,offset = 22,bg=cols[2])}
  # Choroid rete mirabile  
  phenotype          <-to.matrix(CRM[tree.plot$tip.label],seq=sort(unique(CRM)))[i,]
  if(all(phenotype==c(1,0))){
    tiplabels(tip = i,pch = 21, col = "black",cex=.5,offset = 10,bg="white")}
  if(all(phenotype==c(0,1))){
    tiplabels(tip = i,pch = 21, col = "black",cex=.5,offset = 10,bg=cols[1])}
  # Pre-retinal capillarization
  phenotype            <-to.matrix(PRC[tree.plot$tip.label],seq=sort(unique(PRC)))[i,]
  if(all(phenotype==c(1,0,0))){
    tiplabels(tip = i,pch = 21, col = "black",cex=.5,offset = 34,bg="white")}
  if(all(phenotype==c(0,1,0))){
    tiplabels(tip = i,pch = 21, col = "black",cex=.5,offset = 34,bg=cols[3])}
})


## Pie charts at the MRCA of bony fishes
#nodelabels(node = getMRCA(tree.plot,c("Homo_sapiens","Amia_calva")),piecol = c("white",cols[1]),cex = 1,adj = c(-37,105),
#           pie = dSCM_CRM_ER$ace[match(getMRCA(tree.crm,c("Homo_sapiens","Amia_calva")),row.names(dSCM_CRM_ER$ace)),])
#nodelabels(node = getMRCA(tree.plot,c("Homo_sapiens","Amia_calva")),piecol = c("white",cols[2]),cex = 1,adj = c(37,105),
#           pie = dSCM_IRC_ER$ace[match(getMRCA(tree.irc,c("Homo_sapiens","Amia_calva")),row.names(dSCM_IRC_ER$ace)),])
#nodelabels(node = getMRCA(tree.plot,c("Homo_sapiens","Amia_calva")),piecol = c("white",cols[3]),cex = 1,adj = c(0,50),
#           pie = dSCM_PRC_ER$ace[match(getMRCA(tree.prc,c("Homo_sapiens","Amia_calva")),row.names(dSCM_PRC_ER$ace)),])



## MARK GAINS OF CRM
lapply(1:length(gains_crm),function(i){
  downstream<-tree.crm$tip.label[Descendants(x = tree.crm,
                                             node = gains_crm[i],
                                             type = "tips")[[1]]];downstream
  hor <- as.data.frame(tree_layout(tree.plot)$edgeDT);hor
  # Find MRCA in tree.plot
  if(length(downstream)==1){
    row<-hor[match(downstream,hor$OTU),];row$V1
    df<-as.data.frame(tree.plot$edge);df
    df
    mrca<-as.numeric(rownames(subset(df , V1 == row$V1 & V2 == row$V2)));mrca
  }
  if(length(downstream)>1){
    mrca<-getMRCA(tree.plot,downstream);mrca
    df<-as.data.frame(tree.plot$edge);df
    mrca<-which(df$V2==mrca)[1];mrca
  }
  
  edgelabels(edge=mrca, bg=cols[1],col = "black",cex=1.5,pch = 21)
})

## MARK LOSSES OF CRM
lapply(1:length(losses_crm),function(i){
  downstream<-tree.crm$tip.label[Descendants(x = tree.crm,
                                             node = losses_crm[i],
                                             type = "tips")[[1]]];downstream
  hor <- as.data.frame(tree_layout(tree.plot)$edgeDT);hor
  # Find MRCA in tree.plot
  if(length(downstream)==1){
    row<-hor[match(downstream,hor$OTU),];row$V1
    df<-as.data.frame(tree.plot$edge);df
    df
    mrca<-as.numeric(rownames(subset(df , V1 == row$V1 & V2 == row$V2)));mrca
  }
  if(length(downstream)>1){
    mrca<-getMRCA(tree.plot,downstream);mrca
    df<-as.data.frame(tree.plot$edge);df
    mrca<-which(df$V2==mrca)[1];mrca
  }
  edgelabels(edge=mrca, col=cols[1],cex=1.5,pch = 1)
})


## MARK GAINS OF IRC
lapply(1:length(gains_irc),function(i){
  downstream<-tree.irc$tip.label[Descendants(x = tree.irc,
                                             node = gains_irc[i],
                                             type = "tips")[[1]]];downstream
  hor <- as.data.frame(tree_layout(tree.plot)$edgeDT);hor
  # Find MRCA in tree.plot
  if(length(downstream)==1){
    row<-hor[match(downstream,hor$OTU),];row$V1
    df<-as.data.frame(tree.plot$edge)
    mrca<-as.numeric(rownames(subset(df , V1 == row$V1 & V2 == row$V2)));mrca
  }
  if(length(downstream)>1){
    mrca<-getMRCA(tree.plot,downstream);mrca
    df<-as.data.frame(tree.plot$edge);df
    mrca<-which(df$V2==mrca)[1];mrca
  }
  
  edgelabels(edge=mrca, bg=cols[2],col = "black",cex=1.5,pch = 21)
})


## MARK LOSSES OF IRC
lapply(1:length(losses_irc),function(i){
  downstream<-tree.irc$tip.label[Descendants(x = tree.irc,
                                             node = losses_irc[i],
                                             type = "tips")[[1]]];downstream
  hor <- as.data.frame(tree_layout(tree.plot)$edgeDT);hor
  # Find MRCA in tree.plot
  if(length(downstream)==1){
    row<-hor[match(downstream,hor$OTU),];row$V1
    df<-as.data.frame(tree.plot$edge);df
    df
    mrca<-as.numeric(rownames(subset(df , V1 == row$V1 & V2 == row$V2)));mrca
  }
  if(length(downstream)>1){
    mrca<-getMRCA(tree.plot,downstream);mrca
    df<-as.data.frame(tree.plot$edge);df
    mrca<-which(df$V2==mrca)[1];mrca
  }
  edgelabels(edge=mrca, col=cols[2],cex=1.5,pch = 1)
})


## MARK GAINS OF PRC
lapply(1:length(gains_prc),function(i){
  downstream<-tree.prc$tip.label[Descendants(x = tree.prc,
                                             node = gains_prc[i],
                                             type = "tips")[[1]]];downstream
  hor <- as.data.frame(tree_layout(tree.plot)$edgeDT);hor
  # Find MRCA in tree.plot
  if(length(downstream)==1){
    row<-hor[match(downstream,hor$OTU),];row$V1
    df<-as.data.frame(tree.plot$edge)
    mrca<-as.numeric(rownames(subset(df , V1 == row$V1 & V2 == row$V2)));mrca
  }
  if(length(downstream)>1){
    mrca<-getMRCA(tree.plot,downstream);mrca
    df<-as.data.frame(tree.plot$edge);df
    mrca<-which(df$V2==mrca)[1];mrca
  }
  
  edgelabels(edge=mrca, bg=cols[2],col = "black",cex=1.5,pch = 21)
})


## MARK LOSSES OF IRC
lapply(1:length(losses_prc),function(i){
  downstream<-tree.prc$tip.label[Descendants(x = tree.prc,
                                             node = losses_prc[i],
                                             type = "tips")[[1]]];downstream
  hor <- as.data.frame(tree_layout(tree.plot)$edgeDT);hor
  # Find MRCA in tree.plot
  if(length(downstream)==1){
    row<-hor[match(downstream,hor$OTU),];row$V1
    df<-as.data.frame(tree.plot$edge);df
    df
    mrca<-as.numeric(rownames(subset(df , V1 == row$V1 & V2 == row$V2)));mrca
  }
  if(length(downstream)>1){
    mrca<-getMRCA(tree.plot,downstream);mrca
    df<-as.data.frame(tree.plot$edge);df
    mrca<-which(df$V2==mrca)[1];mrca
  }
  edgelabels(edge=mrca, col=cols[2],cex=1.5,pch = 1)
})
# Time axis
axisPhylo(lwd = 1,cex.axis=0.5,line = -5)
title(xlab="Time (MYA)",font.lab=1,cex.lab=0.5,line = -3,adj = .31)

}
pdf("./Figures/S1_Functional diversification.pdf",height = 40,width = 10, useDingbats = F)
figs1()
dev.off()



#######################################################
### Fig. 4: Intraretinal capillarization in mammals ###
#######################################################

mammal                   <-as.data.frame(read_excel("./Data/data.xlsx", sheet = "Mammal"))
type                     <-setNames(mammal$type,mammal$species)
type[which(type=="A")]   <-"Anangiotic"
type[which(type=="M")]   <-"Merangiotic"
type[which(type=="H")]   <-"Holangiotic"
tree_mammals             <-ladderize(keep.tip(tree,names(type)))

# Model fitting
# Q: Which model best predicts retinal capillary type evolution?
LRT.pick.model(tree = tree_mammals, 
               x = type)
# A: SYM

# Prior
pi<-setNames(c(.988,0.006,0.006),c("Anangiotic","Merangiotic","Holangiotic"))


# Stochastic character mapping of intra retinal capillary type using SYM transition matrix
SCM_IRC_ER_mammals       <- make.simmap(tree = tree_mammals,
                                        x = type,
                                        nsim=10000,
                                        model = "SYM",
                                        pi=pi)
dSCM_IRC_ER_mammals      <-describe.simmap(SCM_IRC_ER_mammals)
states                   <-sort(unique(getStates(SCM_IRC_ER_mammals[[1]],"tips")));states


pdf("./Figures/04_IRC_mammals.pdf", width = 5.65,height = 7,useDingbats = F)
par(mar = c(2,0,0,0))
plot(tree_mammals,lwd = 1,label.offset = 8,cex = .5)
axisPhylo(lwd = 1,cex.axis=0.5,padj=-2.5)
title(xlab="Time (MYA)",font.lab=1,cex.lab=0.5,line = 1,adj = 0.35)
nodelabels(pie = dSCM_IRC_ER_mammals$ace, piecol = c("white",cols[2],"grey"), cex = 0.6)
edgelabels(text = "Eutherian mammals",
           edge = match(getMRCA(tree_mammals,c("Homo_sapiens","Loxodonta_africana")),as.data.frame(tree_mammals$edge)$V2),
           frame = "none", cex = 0.5, adj = c(0.5,-1))
edgelabels(text = "Monotremes",
           edge = match(getMRCA(tree_mammals,c("Tachyglossus_aculeatus","Ornithorhynchus_anatinus")),as.data.frame(tree_mammals$edge)$V2),
           frame = "none", cex = 0.5, adj = c(0.5,-1))
edgelabels(text = "Marsupials",
           edge = match(getMRCA(tree_mammals,c("Marmosa_mexicana","Isoodon_obesulus")),as.data.frame(tree_mammals$edge)$V2),
           frame = "none", cex = 0.5, adj = c(0.5,-1))
tiplabels(offset = 3,pie=to.matrix(type[tree_mammals$tip.label],seq=sort(unique(type))),
          piecol=c("white",cols[2],"grey"),cex=0.3,adj=c(3,0.5))
dev.off()


######################################################################
### FIG. 5. ROOT EFFECT, CHOROID RETE, PRE-RETINAL CAPILLARIZATION ###
######################################################################

## Fig. 5A: Frequency distribution of Root effect in fishes with/without a choroid rete. 
df_5a                    <- data.frame(Root = data$Root,
                                       Species = data$Species,
                                       Rete = data$CRM,
                                       Class = data$Class)
df_5a$Rete               <- as.character(df_5a$Rete)
df_5a                    <- df_5a[df_5a$Rete!="2",]
df_5a                    <- df_5a[df_5a$Class=="Actinopterygii",]
df_5a                    <- as.data.frame(na.omit(df_5a))

# Q: Do species with a choroid rete mirabile have higher Root effect?
aov_5a<-
  phylANOVA(tree = keep.tip(tree,df_5a$Species),
            x = setNames(df_5a$Rete,df_5a$Species),
            y = setNames(df_5a$Root,df_5a$Species),
            nsim = 50000);aov_5a
# A: Yes

ggplot(df_5a,aes(x=Rete,y=Root,color=Rete))+
  scale_color_manual(values=c("black",cols[1]))+
  scatter+
  scale_x_discrete(breaks = c("0","1"),labels = c("Absent","Present"))+
  labs(x = expression("Choroid rete mirabile"),
       y = expression("Root effect (%)"))+
  lapply(1, function(i){
    if(aov_5a$Pf<0.001){
    geom_signif(comparisons=list(c("0", "1")),
                y_position = max(1.1*df_5a$Root), vjust=0,
                annotation=paste("P < 0.001",sep=""),
                size = 0.2,textsize = 6/3,color="black")}})+
  theme(panel.grid.major.x   = element_blank())+
  ylim(min(df_5a$Root)*0.9,max(df_5a$Root)*1.2)+
  geom_jitter(size = 1,alpha = 1,pch = 1,width = 0.3)->plot5a;plot5a


## Fig. 5B: pre-retinal capillarization vs. Root effect
# Prune data set and tree 
df_5b                    <-cbind(10000*data["PRC"],
                                 data["Root"])
row.names(df_5b)         <-data$Species
df_5b                    <-df_5b[df_5b$PRC!=0,]
df_5b                    <-as.data.frame(na.omit(df_5b))
tree_pgls                <-keep.tip(tree,row.names(df_5b))

# Brownian motion model
BM_5b                       <- gls(PRC~Root, 
                                 correlation = corPagel(value = 1,phy = tree_pgls,fixed = F),
                                 method = "ML",
                                 data = df_5b)
summary(BM_5b) # Summary of model

plot(BM_5b, resid(., type="n")~fitted(.), col="blue", main="Normalized Residuals vs. Fitted Values",abline=c(0,0)) #check for heteroscedasticity
res <- resid(BM_5b, type="n");qqnorm(res, col="blue");qqline(res, col="blue") #check for departures from normal distribution of residuals

# OU model
OU_5b <- gls(PRC~Root, 
          data=df_5b, 
          correlation=corMartins(0.1, tree_pgls, fixed=FALSE), method="ML")
summary(OU_5b)

aicw(c(AIC(BM_5b),AIC(OU_5b)))
# Conclusion: BM model is best

ggplot(df_5b,aes(x=Root,y=PRC))+
  scatter+
  stat_function(fun = function(x) BM_5b$coefficients[1]+x*BM_5b$coefficients[2], linetype = "solid",col = "black",lwd = 0.2) +
  labs(x = expression("Root effect (%)"),
       y = expression("Preretinal capillarization (10"^"4"*"mm)"))+
  geom_point(size = 1,alpha = 1,pch = 16)->plot5b;plot5b

pdf("./Figures/05_root_CRM_PRC.pdf",width = 3,height = 1.5,useDingbats = F)
plot_grid(plot5a,plot5b,ncol = 2, align = 'h',labels = "AUTO",label_size = 12,hjust = c(-2.9,-3),vjust = 1.2)
dev.off()

jpeg(filename = "./Figures/05_root_CRM_PRC.jpeg",width = 3,height = 1.5,units = "in",res = 2400)
plot_grid(plot5a,plot5b,ncol = 2, align = 'h',labels = "AUTO",label_size = 12,hjust = c(-2.9,-3),vjust = 1.2)
dev.off()


#############################################################################
### Fig. 7 Choroid rete / intra-retinal capillaries vs. retinal thickness ###
#############################################################################

## Fig. 7A: Frequency distribution of retinal thickness in fishes with/without a choroid rete. 
df_7a                     <- data.frame(Thickness = data$Thickness,
                                       Species = data$Species,
                                       Rete = as.character(data$CRM),
                                       Class = data$Class,
                                       IRC = data$IRC)
df_7a                       <- subset(df_7a,Class=="Actinopterygii")
df_7a                       <- as.data.frame(na.omit(df_7a))
rownames(df_7a)             <-df_7a$Species
df_7a$Rete                  <-as.character(df_7a$Rete)
df_7a$IRC                   <-as.character(df_7a$IRC)


# Q: Do species with a choroid rete mirabile have thicker retinae?
aov_7a                      <-phylANOVA(tree = keep.tip(tree,df_7a$Species),
                                     x = setNames(df_7a$Rete,df_7a$Species),
                                     y = setNames(df_7a$Thickness,df_7a$Species),
                                     nsim = 50000);aov
# A: Yes



# Q: Within species without a choroid rete, does IRC, PRC and/or Root effect explain the variation in retinal thickness?
# Generate a data frame with data
df_0                     <-df_7a[which(df_7a$Rete==0),]
df_0["PRC"]              <-data$PRC[match(df_0$Species,data$Species)]
df_0["Root"]             <-data$Root[match(df_0$Species,data$Species)]
df_0                     <-as.data.frame(na.omit(df_0))
df_0
# Fit different models with BM and OU correlation matices
BM1                      <- gls(Thickness~IRC, data = df_0, correlation = corBrownian(phy = keep.tip(tree,df_0$Species)),method = "ML")
BM2                      <- gls(Thickness~Root, data = df_0, correlation = corBrownian(phy = keep.tip(tree,df_0$Species)),method = "ML")
BM3                      <- gls(Thickness~PRC, data = df_0, correlation = corBrownian(phy = keep.tip(tree,df_0$Species)),method = "ML")
BM4                      <- gls(Thickness~IRC+PRC, data = df_0, correlation = corBrownian(phy = keep.tip(tree,df_0$Species)),method = "ML")
BM5                      <- gls(Thickness~Root+PRC, data = df_0, correlation = corBrownian(phy = keep.tip(tree,df_0$Species)),method = "ML")
BM6                      <- gls(Thickness~Root+IRC, data = df_0, correlation = corBrownian(phy = keep.tip(tree,df_0$Species)),method = "ML")
BM7                      <- gls(Thickness~IRC*PRC, data = df_0, correlation = corBrownian(phy = keep.tip(tree,df_0$Species)),method = "ML")
BM8                      <- gls(Thickness~Root*PRC, data = df_0, correlation = corBrownian(phy = keep.tip(tree,df_0$Species)),method = "ML")
BM9                      <- gls(Thickness~Root*IRC, data = df_0, correlation = corBrownian(phy = keep.tip(tree,df_0$Species)),method = "ML")
OU1                      <- gls(Thickness~IRC, correlation = corMartins(value =.1,phy=keep.tip(tree,df_0$Species),fixed = F),method="ML",data=df_0)
OU2                      <- gls(Thickness~Root, correlation = corMartins(value =.1,phy=keep.tip(tree,df_0$Species),fixed = F),method="ML",data=df_0)
OU3                      <- gls(Thickness~PRC, correlation = corMartins(value =.1,phy=keep.tip(tree,df_0$Species),fixed = F),method="ML",data=df_0)
OU4                      <- gls(Thickness~IRC+PRC, correlation = corMartins(value =.1,phy=keep.tip(tree,df_0$Species),fixed = F),method="ML",data=df_0)
OU5                      <- gls(Thickness~Root+PRC, correlation = corMartins(value =.1,phy=keep.tip(tree,df_0$Species),fixed = F),method="ML",data=df_0)
OU6                      <- gls(Thickness~Root+IRC, correlation = corMartins(value =.1,phy=keep.tip(tree,df_0$Species),fixed = F),method="ML",data=df_0)
OU7                      <- gls(Thickness~IRC*PRC, correlation = corMartins(value =.1,phy=keep.tip(tree,df_0$Species),fixed = F),method="ML",data=df_0)
OU8                      <- gls(Thickness~Root+PRC, correlation = corMartins(value =.1,phy=keep.tip(tree,df_0$Species),fixed = F),method="ML",data=df_0)
OU9                      <- gls(Thickness~Root*IRC, correlation = corMartins(value =.1,phy=keep.tip(tree,df_0$Species),fixed = F),method="ML",data=df_0)

# Model selection
aicw(c(AIC(BM1),AIC(BM2),AIC(BM3),AIC(BM4),AIC(BM5),AIC(BM6),AIC(BM7),AIC(BM8),AIC(BM9),AIC(OU1),AIC(OU2),AIC(OU3),AIC(OU4),AIC(OU5),AIC(OU6),AIC(OU7),AIC(OU8),AIC(OU9)))
anova(BM1,BM2,BM3,BM4,BM5,BM6,BM7,BM8,BM9,OU1,OU2,OU3,OU4,OU5,OU6,OU7,OU8,OU9)
summary(OU1)
# OU1 is best model - IRC explains the residual variation, but PRC and Root effect do not. 



# Q: Within species with a choroid rete, does PRC and/or Root effect explain the variation in retinal thickness?
df_1                     <-df_7a[which(df_7a$Rete==1),]
df_1["PRC"]              <-data$PRC[match(df_1$Species,data$Species)]
df_1["Root"]             <-data$Root[match(df_1$Species,data$Species)]
df_1["SAD"]<-data$`Surface area density [mm-1]`[match(df_1$Species,data$Species)]
df_1                     <-as.data.frame(na.omit(df_1))


BM1                      <- gls(Thickness~SAD, data = df_1, correlation = corBrownian(phy = keep.tip(tree,df_1$Species)),method = "ML")
BM2                      <- gls(Thickness~Root, data = df_1, correlation = corBrownian(phy = keep.tip(tree,df_1$Species)),method = "ML")
BM3                      <- gls(Thickness~PRC, data = df_1, correlation = corBrownian(phy = keep.tip(tree,df_1$Species)),method = "ML")
BM4                      <- gls(Thickness~SAD+PRC, data = df_1, correlation = corBrownian(phy = keep.tip(tree,df_1$Species)),method = "ML")
BM5                      <- gls(Thickness~Root+PRC, data = df_1, correlation = corBrownian(phy = keep.tip(tree,df_1$Species)),method = "ML")
BM6                      <- gls(Thickness~Root+SAD, data = df_1, correlation = corBrownian(phy = keep.tip(tree,df_1$Species)),method = "ML")
BM7                      <- gls(Thickness~SAD*PRC, data = df_1, correlation = corBrownian(phy = keep.tip(tree,df_1$Species)),method = "ML")
BM8                      <- gls(Thickness~Root*PRC, data = df_1, correlation = corBrownian(phy = keep.tip(tree,df_1$Species)),method = "ML")
BM9                      <- gls(Thickness~Root*SAD, data = df_1, correlation = corBrownian(phy = keep.tip(tree,df_1$Species)),method = "ML")
OU1                      <- gls(Thickness~SAD, correlation = corMartins(value =.1,phy=keep.tip(tree,df_1$Species),fixed = F),method="ML",data=df_1)
OU2                      <- gls(Thickness~Root, correlation = corMartins(value =.1,phy=keep.tip(tree,df_1$Species),fixed = F),method="ML",data=df_1)
OU3                      <- gls(Thickness~PRC, correlation = corMartins(value =.1,phy=keep.tip(tree,df_1$Species),fixed = F),method="ML",data=df_1)
OU4                      <- gls(Thickness~SAD+PRC, correlation = corMartins(value =.1,phy=keep.tip(tree,df_1$Species),fixed = F),method="ML",data=df_1)
OU5                      <- gls(Thickness~Root+PRC, correlation = corMartins(value =.1,phy=keep.tip(tree,df_1$Species),fixed = F),method="ML",data=df_1)
OU6                      <- gls(Thickness~Root+SAD, correlation = corMartins(value =.1,phy=keep.tip(tree,df_1$Species),fixed = F),method="ML",data=df_1)
OU7                      <- gls(Thickness~SAD*PRC, correlation = corMartins(value =.1,phy=keep.tip(tree,df_1$Species),fixed = F),method="ML",data=df_1)
OU8                      <- gls(Thickness~Root+PRC, correlation = corMartins(value =.1,phy=keep.tip(tree,df_1$Species),fixed = F),method="ML",data=df_1)
OU9                      <- gls(Thickness~Root*SAD, correlation = corMartins(value =.1,phy=keep.tip(tree,df_1$Species),fixed = F),method="ML",data=df_1)
aicw(c(AIC(BM1),AIC(BM2),AIC(BM3),AIC(BM4),AIC(BM5),AIC(BM6),AIC(BM7),AIC(BM8),AIC(BM9),AIC(OU1),AIC(OU2),AIC(OU3),AIC(OU4),AIC(OU5),AIC(OU6),AIC(OU7),AIC(OU8),AIC(OU9)))
anova(BM1,BM2,BM3,BM4,BM5,BM6,BM7,BM8,BM9,OU1,OU2,OU3,OU4,OU5,OU6,OU7,OU8,OU9)
summary(OU2)
# Model OU1 is best fit - Root effect does not explain the residual variation in retinal thickness. 


# Plot 
ggplot(df_7a[df_7a$IRC!=1,],aes(x=Rete,y=Thickness,color=Rete))+
  scale_color_manual(values=c("black",cols[1]))+
  scatter+
  
  scale_x_discrete(breaks = c("0","1"),labels = c("Absent","Present"))+
  labs(x = expression("Choroid rete mirabile"),
       y = expression("Retinal thickness (µm)"))+
  geom_signif(comparisons=list(c("0", "1")),
              y_position = max(1.1*df_7a$Thickness), vjust=0,annotation=paste("P = ",aov_7a$Pf,sep=""),size = 0.2,textsize = 6/3,color="black") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major.x   = element_blank())+
  ylim(c(0,600))+
  geom_jitter(df_7a[df_7a$IRC==1,],mapping = aes(x=Rete,y=Thickness,color=Rete), col = cols[2], size = 1,alpha = 1,pch = 1,width = 0.3)+
  geom_jitter(size = 1,alpha = 1,pch = 1,width = 0.3)->plot7a;plot7a;pdf("./Figures/7A_thicknessvsRete.pdf",width = 1.5,height = 1.5,useDingbats = F);plot7a;dev.off()


## Fig. 7B: Frequency distribution of retinal thickness in mammals with/without a intraretinal capillaries. 
df_7b                       <- data.frame(Thickness = data$Thickness,
                                       Species = data$Species,
                                       IRC = as.character(data$IRC),
                                       Class = data$Class)
df_7b                       <-subset(df_7b,Class=="Mammalia")
df_7b                       <-as.data.frame(na.omit(df_7b))


# Q: Do mammals with intraretinal capillaries have thicker retinae?
aov_7b<-phylANOVA(tree = keep.tip(tree,df_7b$Species),x = setNames(as.character(df_7b$IRC),df_7b$Species),y = setNames(df_7b$Thickness,df_7b$Species),nsim = 50000);aov
# A: Yes


ggplot(df_7b,aes(x=IRC,y=Thickness,color = IRC))+
  scale_color_manual(values=c("black",cols[2]))+
  scatter+
  geom_jitter(size = 1,alpha = 1,pch = 1,width = 0.3)+
  scale_x_discrete(breaks = c("0","1"),labels = c("Absent","Present"))+
  labs(x = expression("Intraretinal capillaries"),
       y = expression("Retinal thickness (µm)"))+
  geom_signif(comparisons=list(c("0", "1")),
              y_position = max(1.1*df_7b$Thickness), vjust=0,annotation=paste("P = ",aov_7b$Pf,sep=""),size = 0.2,textsize = 6/3,color="black") +
  theme(axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major.x   = element_blank())+
  ylim(c(0,600))->plot7b;plot7b
pdf("./Figures/7B_thicknessvsIRC.pdf",width = 1.5,height = 1.5,useDingbats = F);plot7b;dev.off()


plot7<-plot_grid(plot7a,plot7b,ncol = 2, align = 'h',labels = "AUTO",label_size = 12,hjust = c(-.7,-.7),vjust = 1.2)
pdf("./Figures/07_CRM_IRC_Thickness.pdf",width = 3,height = 1.5,useDingbats = F)
grid.arrange(arrangeGrob(plot7, left = textGrob("Maximal retinal thickness (µm)", rot = 90, hjust = 0.4, vjust = -.3,gp=gpar(fontsize=6))))
dev.off()

## REMEMBER TO ADD Y AXIS TEXT


#######











### PLOT TRAJECTORIES FOR EACH SPECIES ###
# Make a data frame containing species with complete data sets
data$PEPRL.p = data$PEPRL/data$Thickness
data$ONL.p = data$ONL/data$Thickness
data$OPL.p = data$OPL/data$Thickness
data$INL.p = data$INL/data$Thickness
data$IPL.p = data$IPL/data$Thickness
data$GCL.p = data$GCL/data$Thickness
data$NFL.p = data$NFL/data$Thickness

data_trajectories        <-data.frame(Species = data$Species, 
                                      Thickness = data$Thickness, 
                                      Root = data$Root, 
                                      PRC = data$PRC,
                                      PEPRL.p = data$PEPRL.p,
                                      ONL.p = data$ONL.p,
                                      OPL.p = data$OPL.p,
                                      INL.p = data$INL.p,
                                      IPL.p = data$IPL.p,
                                      GCL.p = data$GCL.p,
                                      NFL.p = data$NFL.p,
                                      IRC = as.character(data$IRC),
                                      CRM = as.character(data$CRM))

data_trajectories        <-data_trajectories[data_trajectories$IRC!="2",]
data_trajectories        <-data_trajectories[data_trajectories$CRM!="2",]
                         
data_trajectories        <-na.omit(data_trajectories)
Char_traj                <-colnames(data_trajectories)[-1]
labels                   <-c("Thickness (µm)","Root effect (%)","PRC","IRC","CRM")

# Make a list of named characters vectors
Charlist<-list()
for (i in 1:length(Char_traj)) {
  Charlist[[i]]          <-data[match(x = Char_traj[i],table = names(data))]
  Charlist[[i]]          <-unlist(Charlist[[i]])
  names(Charlist[[i]])   <-data$Species
  Charlist[[i]]          <-Charlist[[i]][ !is.na(Charlist[[i]])]
  if(i==11){Charlist[[i]] <-Charlist[[i]][Charlist[[i]]!="2"]}
  if(i==12){Charlist[[i]] <-Charlist[[i]][Charlist[[i]]!="2"]}
}

# Make trajectory phylogeny by pruning the big phylogeny to species with complete data sets
tree_traj                <-ladderize(keep.tip(tree = tree,
                                              tip = data_trajectories$Species))
tree_traj_nn             <-(1:tree_traj$Nnode + length(tree_traj$tip))                             # This is a list of node numbers
hor                      <-as.data.frame(tree_layout(tree_traj)$edgeDT)                            # Get time for each branch

pdf("./Figures/Supporting figures/Trajectory tree.pdf")
plot(tree_traj,cex = 0.5)
dev.off()

# Make phylogenies for ancestral state reconstructions for each character
treelist<-list()
for (i in 1:length(Char_traj)) {
  treelist[[i]]          <-keep.tip(tree = tree,
                                    tip = names(Charlist[[i]]))
}

# Maximum likelihood ancestral state reconstruction for continuous characters
asrlist<-list()
n<-length(Char_traj)-2
for (i in 1:n) {
  asrlist[[i]]           <-fastAnc(treelist[[i]],Charlist[[i]])
}


# Stochastic character mapping for ancestral state reconstruction of binary characters
for (i in 1:2) {
  if(i==1){
    SIMMAP                 <-make.simmap(tree = treelist[[i+n]], 
                                         x = Charlist[[i+n]],
                                         nsim=10000,
                                         model = "ER")
    XX                     <-describe.simmap(SIMMAP)
    asrlist[[i+n]]         <-XX$ace[,2]
  }
  if(i==2){
    SIMMAP                 <-make.simmap(tree = treelist[[i+n]], 
                                         x = Charlist[[i+n]],
                                         nsim=10000,
                                         model = "ER")
    XX                     <-describe.simmap(SIMMAP)
    asrlist[[i+n]]         <-XX$ace[,2]
  }
}



# Transfer ancestral states to trajectory phylogeny
nodelist<-list()

# A matrix with "A" containing 
# 1) node numbers in the trajectory tree
# 2) corresponding node number in the phylogeny pruned to represent species with data for that specific character only

for (j in 1:length(Charlist)){
  A                      <-matrix(nrow=length(tree_traj_nn),ncol=3) 
  A[,1]                  <-tree_traj_nn
  
  for (i in 1:length(tree_traj_nn)) {                                                              # For each internal node number do the following:
    des                  <-Descendants(x = tree_traj,                                              # Find the extant descendants in the trajectory phylogeny
                                       node = A[i,1])
    des                  <-tree_traj$tip.label[unlist(des)]
    
    A[i,2]               <-getMRCA(phy = treelist[[j]],
                                   tip = des);A                        # Find the internal node number of the MRCA of those species in the phylogeny pruned to represent species with data for that specific character only
    A[i,3]               <-unname(asrlist[[j]][match(A[i,2],names(asrlist[[j]]))]);A                 # 
    a                    <-A[,3]
    names(a)             <-A[,1]
    nodelist[[j]]        <-a
    
  }
}

# Plot trajectories for each character for each species
for (j in 1:length(tree_traj$tip.label)) {
  
  plotlist_seq           <-list()
  plotlist_time          <-list()
  
  # Generate a vector with the trajectory from root to species
  trajectory             <-Ancestors(tree_traj, j);trajectory
  trajectory             <-append(x = trajectory,values = j,after = length(trajectory));trajectory
  l                      <-length(trajectory);l
  
  # Plot trajectory on phylogeny
  pdf(file = paste("./Figures/Trajectories/",tree_traj$tip.label[j],"_a",".pdf",sep = ""), width = 1,height = 1, useDingbats = F)
  plot(tree_traj,no.margin = T,show.tip.label = F)
  nodelabels(node = trajectory, text=rep(".",length(trajectory)),frame = "circle",bg="red",col="red",cex = 0.3)
  #axisPhylo(lwd = 1,cex.axis=0.7,padj=-1.5)
  #title(font=2,cex.lab=0.7,line = 1.5) 
  dev.off()

  
  
  for (k in 1:length(nodelist)) {
  
    asr                  <-nodelist[[k]];asr
    Char                 <-Charlist[[k]];Char
    
    # Data frame with time and character values
    A                    <-matrix(ncol = 4,nrow=l)
    
    colnames(A)          <-c("Node","Value","Time","Seq")
    A[,1]                <-trajectory
    
    
    for (i in 1:l) {
      A[i,2]             <-unname(asr[match(A[i,1],names(asr))])
      A[i,3]             <-max(hor$xright)-hor$xright[match(trajectory[i],hor$V2)]
      A[i,3]             <-ifelse(is.na(A[i,3])==TRUE, yes = max(hor$xright), no = A[i,3])
    }
    A[l,2]               <-unname(Char[match(tree_traj$tip.label[j],names(Char))])
    A                    <-A[order(A[,3]),] 
    A[,4]<- seq(from = 1, to = l, by = 1)
    
    if (k==1){
      B<-matrix(ncol = 10,nrow=l)
      colnames(B)<-c("Node","PEPRL","ONL","OPL","INL","IPL","GCL","NFL","Time","Seq")
      B[,1]<-trajectory
      asr_1                <-nodelist[[4]];asr_1
      asr_2                <-nodelist[[5]];asr_2
      asr_3                <-nodelist[[6]];asr_3
      asr_4                <-nodelist[[7]];asr_4
      asr_5                <-nodelist[[8]];asr_5
      asr_6                <-nodelist[[9]];asr_6
      asr_7                <-nodelist[[10]];asr_7
      
      Char_1                <-Charlist[[4]]
      Char_2                <-Charlist[[5]]
      Char_3                <-Charlist[[6]]
      Char_4                <-Charlist[[7]]
      Char_5                <-Charlist[[8]]
      Char_6                <-Charlist[[9]]
      Char_7                <-Charlist[[10]]
      
    for (i in 1:l) {
      B[i,2]             <-unname(asr_1[match(B[i,1],names(asr_1))])
      B[i,3]             <-unname(asr_2[match(B[i,1],names(asr_2))])
      B[i,4]             <-unname(asr_3[match(B[i,1],names(asr_3))])
      B[i,5]             <-unname(asr_4[match(B[i,1],names(asr_4))])
      B[i,6]             <-unname(asr_5[match(B[i,1],names(asr_5))])
      B[i,7]             <-unname(asr_6[match(B[i,1],names(asr_6))])
      B[i,8]             <-unname(asr_7[match(B[i,1],names(asr_7))])
      
      B[i,9]             <-max(hor$xright)-hor$xright[match(trajectory[i],hor$V2)]
      B[i,9]             <-ifelse(is.na(B[i,9])==TRUE, yes = max(hor$xright), no = B[i,9])
    }
      
      B[l,2]               <-unname(Char_1[match(tree_traj$tip.label[j],names(Char_1))]);B
      B[l,3]               <-unname(Char_2[match(tree_traj$tip.label[j],names(Char_2))]);B
      B[l,4]               <-unname(Char_3[match(tree_traj$tip.label[j],names(Char_3))]);B
      B[l,5]               <-unname(Char_4[match(tree_traj$tip.label[j],names(Char_4))]);B
      B[l,6]               <-unname(Char_5[match(tree_traj$tip.label[j],names(Char_5))]);B
      B[l,7]               <-unname(Char_6[match(tree_traj$tip.label[j],names(Char_6))]);B
      B[l,8]               <-unname(Char_7[match(tree_traj$tip.label[j],names(Char_7))]);B
    B                    <-B[order(B[,9]),] 
    B[,10]                <- seq(from = 1, to = l, by = 1)
    A<-cbind(A,B[,2:8])
    
    
    }
    
    A<-as.data.frame(A);A
    
    if (k==1){
      A$PEPRL<-A$PEPRL*A$Value
      A$ONL<-A$ONL*A$Value
      A$OPL<-A$OPL*A$Value
      A$INL<-A$INL*A$Value
      A$IPL<-A$IPL*A$Value
      A$GCL<-A$GCL*A$Value
      A$NFL<-A$NFL*A$Value
    
    
    
    layers               <-rbind(cbind(A$Node,A$Time,A$Seq,A$PEPRL,rep("7.PEPRL",dim(A)[1])),
                                 cbind(A$Node,A$Time,A$Seq,A$ONL,rep("6.ONL",dim(A)[1])),
                                 cbind(A$Node,A$Time,A$Seq,A$OPL,rep("5.OPL",dim(A)[1])),
                                 cbind(A$Node,A$Time,A$Seq,A$INL,rep("4.INL",dim(A)[1])),
                                 cbind(A$Node,A$Time,A$Seq,A$IPL,rep("3.IPL",dim(A)[1])),
                                 cbind(A$Node,A$Time,A$Seq,A$GCL,rep("2.GCL",dim(A)[1])),
                                 cbind(A$Node,A$Time,A$Seq,A$NFL,rep("1.NFL",dim(A)[1])));layers
    
    layers<-as.data.frame(layers);layers
    colnames(layers)     <-c("Node","Time","Seq","Value","Layer")
    layers$Node<-as.numeric(as.character(layers$Node))
    layers$Time<-as.numeric(as.character(layers$Time))
    layers$Seq<-as.numeric(as.character(layers$Seq))
    layers$Value<-as.numeric(as.character(layers$Value))
    layers$Layer<-as.character(layers$Layer)
    layers
    
    
    
      plotlist_seq[[k]]<- 
        ggplot(A, aes(x = Seq, y = Value)) + 
        geom_area(layers, mapping = aes(x = Seq, y = Value, fill = Layer),alpha = 0.75)+
        scale_fill_manual(breaks = rev(c("7.PEPRL","6.ONL","5.OPL","4.INL","3.IPL","2.GCL","1.NFL")),
                          labels = rev(c("PEPRL","ONL","OPL","INL","IPL","GCL","NFL")),
                          values = brewer.pal(7,"RdYlBu"))+
        scale_y_continuous(limits = c(0,570))+
        geom_point(pch = 19, size = 1) + 
        geom_line(lty=1)+
        scale_x_reverse()+
        theme(
          legend.key.size = unit(.07, "cm"),
          legend.title = element_blank(),
          legend.text = element_text(size = 6),
          legend.position = c(-0.03,.3),
          legend.justification = c(0,0),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          panel.grid.major = element_line(colour = "grey90",size = 0.2),
          axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks.x=element_blank()
          );plotlist_seq[[k]]
      
      
      
      plotlist_time[[k]]<-
        ggplot(A, aes(x = Time, y = Value)) + 

        geom_area(layers, mapping = aes(x = Time, y = Value, fill = Layer),alpha = 0.75)+
        scale_fill_manual(breaks = rev(c("7.PEPRL","6.ONL","5.OPL","4.INL","3.IPL","2.GCL","1.NFL")),
                          labels = rev(c("PEPRL","ONL","OPL","INL","IPL","GCL","NFL")),
                          values = brewer.pal(7,"RdYlBu"))+
        scale_y_continuous(limits = c(0,570))+
        theme(
          legend.key.size = unit(.07, "cm"),
          legend.title = element_blank(),
          legend.text = element_text(size = 6),
          legend.position = c(-.03,.3),
          legend.justification = c(0,0),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          panel.grid.major = element_line(colour = "grey90",size = 0.2),
          axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks.x=element_blank()
        )+
        geom_line(lty=1) +
        scale_x_reverse()+
        geom_point(pch = 19, size = 1);plotlist_time[[k]]
      
    }
    
    if(k!=1){
      plotlist_seq[[k]]<-ggplot(A, aes(x = Seq, y = Value)) + 
        scale_y_continuous(limits = c(min(Char),max(Char)))+
        geom_point(pch = 19, size = 1) + 
        geom_line(lty=1)+
        theme(
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          panel.grid.major = element_line(colour = "grey90",size = 0.2),
          axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks.x=element_blank()
        )+
      scale_x_reverse();plotlist_seq[[k]]
      
      plotlist_time[[k]]<-ggplot(A, aes(x = Time, y = Value)) + 
        scale_y_continuous(limits = c(min(Char),max(Char)))+
        geom_point(pch = 19, size = 1) + 
        geom_line(lty=1)+
        theme(
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          panel.grid.major = element_line(colour = "grey90",size = 0.2),
          axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks.x=element_blank()
        )+
        scale_x_reverse();plotlist_time[[k]]
    }
  }
  
  
  
  # Save character trajectories
  plot_grid(plotlist_seq[[1]], plotlist_seq[[2]], plotlist_seq[[3]], 
            plotlist_seq[[11]], plotlist_seq[[12]], #plotlist_seq[[6]], 
            ncol = 1, align = 'v')
  ggsave(paste("./Figures/Trajectories/",tree_traj$tip.label[j],"_b",".pdf",sep = ""),width = 1.5,height = 4*1.5)
  
  plot_grid(plotlist_time[[1]], plotlist_time[[2]], plotlist_time[[3]], 
            plotlist_time[[11]], plotlist_time[[12]], #plotlist_time[[6]], 
            ncol = 1, align = 'v')
  ggsave(paste("./Figures/Trajectories/",tree_traj$tip.label[j],"_c",".pdf",sep = ""),width = 1.5,height = 4*1.5)
}



############################## 
### Icefish reconstruction ###
##############################
noto.tree<-keep.tip(tree.Thickness,c("Gobionotothen_gibberifrons","Trematomus_hansoni","Notothenia_coriiceps","Chaenodraco_wilsoni","Chionodraco_rastrospinosus"));noto.tree

#noto.tree$root.edge      <-59.951405
noto.tree$root.edge      <-10
noto.tree_nn             <-(1:noto.tree$Nnode + length(noto.tree$tip))                             # This is a list of node numbers
hor                      <-as.data.frame(tree_layout(noto.tree)$edgeDT)                            # Get time for each branch

A                      <-matrix(nrow=length(noto.tree_nn),ncol=3) 
A[,1]                  <-noto.tree_nn

for (i in 1:noto.tree$Nnode) {                                                              # For each internal node number do the following:
  des                  <-Descendants(x = noto.tree,                                              # Find the extant descendants in the trajectory phylogeny
                                       node = A[i,1])
  des                  <-noto.tree$tip.label[unlist(des)]
  
  A[i,2]               <-getMRCA(phy = tree.Thickness,
                                   tip = des);A                        # Find the internal node number of the MRCA of those species in the phylogeny pruned to represent species with data for that specific character only
  A[i,3]               <-unname(asr_Thickness$ace[match(A[i,2],names(asr_Thickness$ace))]);A                 # 
  }

noto.tree.plot           <-noto.tree
noto.tree.plot$tip.label<-paste(noto.tree.plot$tip.label,round(as.numeric(Thickness[noto.tree.plot$tip.label])),sep = " ")

# Plot figure S1
pdf("./Figures/09_noto_thickness.pdf",width = 4,height = 2,useDingbats = F)
par(mar = c(3,0,0,0))
plot(noto.tree.plot,root.edge = T,use.edge.length = T,cex = 0.5,show.tip.label = T,no.margin = F,label.offset = 4)
#tiplabels(text = noto.tree.plot$tip.label,frame = "none",adj = 0,offset = 3)
nodelabels(round(A[,3],0),frame = "none",adj = c(-.3,0),cex = .5)

# Pie charts at the MRCA of bony fishes
nodelabels(node = getMRCA(noto.tree,c("Trematomus_hansoni","Chionodraco_rastrospinosus")),piecol = c("white",cols[1]),cex = .75,adj = c(-1,1.15),
           pie = dSCM_CRM_ER$ace[match(getMRCA(tree.crm,c("Trematomus_hansoni","Chionodraco_rastrospinosus")),row.names(dSCM_CRM_ER$ace)),])
nodelabels(node = getMRCA(noto.tree,c("Trematomus_hansoni","Chionodraco_rastrospinosus")),piecol = c("white",cols[2]),cex = .75,adj = c(-4,1.15),
           pie = dSCM_IRC_ER$ace[match(getMRCA(tree.irc,c("Trematomus_hansoni","Chionodraco_rastrospinosus")),row.names(dSCM_IRC_ER$ace)),])
nodelabels(node = getMRCA(noto.tree,c("Trematomus_hansoni","Chionodraco_rastrospinosus")),piecol = c("white",cols[3]),cex = .75,adj = c(-2.4,.75),
           pie = dSCM_PRC_ER$ace[match(getMRCA(tree.prc,c("Trematomus_hansoni","Chionodraco_rastrospinosus")),row.names(dSCM_PRC_ER$ace)),])

axisPhylo(cex.axis = .5) # Time axis

# Tip labels
lapply(1:length(noto.tree$tip.label),function(i){
  # Intra-retinal capillaries  
  phenotype            <-to.matrix(IRC[noto.tree$tip.label],seq=sort(unique(IRC)))[i,]
  if(all(phenotype==c(1,0,0))){
    tiplabels(tip = i,pch = 21, col = "black",cex=.75,offset = 1,bg="white")}
  if(all(phenotype==c(0,1,0))){
    tiplabels(tip = i,pch = 21, col = "black",cex=.75,offset = 1,bg=cols[2])}
  # Choroid rete mirabile  
  phenotype          <-to.matrix(CRM[noto.tree$tip.label],seq=sort(unique(CRM)))[i,]
  if(all(phenotype==c(1,0))){
    tiplabels(tip = i,pch = 21, col = "black",cex=.75,offset = 2.5,bg="white")}
  if(all(phenotype==c(0,1))){
    tiplabels(tip = i,pch = 21, col = "black",cex=.75,offset = 2.5,bg=cols[1])}
  # Pre-retinal capillarization
  phenotype            <-to.matrix(PRC[noto.tree$tip.label],seq=sort(unique(PRC)))[i,]
  if(all(phenotype==c(1,0,0))){
    tiplabels(tip = i,pch = 21, col = "black",cex=.75,offset = 4,bg="white")}
  if(all(phenotype==c(0,1,0))){
    tiplabels(tip = i,pch = 21, col = "black",cex=.75,offset = 4,bg=cols[3])}
})
dev.off()






###################
### Other plots ###
###################

## LOG(SA) VS LOG(BODYMASS)

# Prune data set and tree 
df                       <- data.frame(logmass = data$`logmass for sa`,
                                       logsa = data$logsa,
                                       Species = data$Species,
                                       Class = data$Class)
row.names(df)            <-df$Species
df                       <-as.data.frame(na.omit(df))
tree_pgls                <-keep.tip(tree,row.names(df))

# Brownian motion model
BM                       <- gls(logsa~logmass, 
                                correlation = corPagel(value = 0,phy = tree_pgls,fixed = F),
                                method = "ML",
                                data = df)
summary(BM) # Summary of model


# Ornstein–Uhlenbeck model
OU <- gls(logsa~logmass, 
          data=df, 
          correlation=corMartins(2, tree_pgls, fixed=FALSE), method="ML")
summary(OU) # Summary of model


# Model selection based on Akaike weights
aicw(c(AIC(BM),AIC(OU)))
# Conclusion: Use brownian motion model

# Q: Is data heteroscedastic?
plot(BM, resid(., type="n")~fitted(.), col="blue", main="Normalized Residuals vs. Fitted Values",abline=c(0,0)) 
# A: No

# Q: Does the residuals depart from a normal distribution?
rSA                      <-resid(BM, type="n")
qqnorm(rSA)
qqline(rSA) 
# A: No


# Add rEM to the data frame "data" 
rSA                      <-setNames(as.vector(rSA),names(rSA))
data["rSA"]              <-rep(NA,dim(data)[1])
data$rSA[match(names(rSA),data$Species)]<-unname(rSA)




# Plot the data
ggplot(df,aes(x=logmass,y=logsa))+
  stat_function(fun = function(x) BM$coefficients[1]+x*BM$coefficients[2], linetype = "solid",col = "black",lwd = 0.2) +
  scale_color_discrete(cols)+
  geom_point(size = 0.5,alpha = 1,pch = 16)+
  scatter+
  labs(x = expression("log"[10]*"(body mass [g])"),
       y = expression("log"[10]*"(surface area [m2])"))->plotx;plotx;pdf("./Figures/LogSAvsLogM.pdf",width = 1.5,height = 1.5,useDingbats = F);plotx;dev.off()



setNames(data$`Surface area density [mm-1]`,names(data$Species))
!is.na(data$`Surface area density [mm-1]`)[setNames(data$`Surface area density [mm-1]`,names(data$Species))]




## rEM VS CLASS
# Fig. 3B: Jitter plot of rEM vs. class
df                       <- data.frame(rEM = data$rEM,
                                       Species = data$Species,
                                       Class = data$Class)
df                       <-as.data.frame(na.omit(df))

ggplot(df,aes(x=Class,y=rEM,color=Class))+
  scale_color_discrete(cols)+
  scatter+
  geom_jitter(size = 0.5,alpha = 1,pch = 16,width = 0.2)->plot2;plot2;pdf("./Figures/X_rEMvsClass.pdf",width = 1.5,height = 1.5,useDingbats = F);plot2;dev.off()






## Fig. 7C: Retinal thickness vs. Root effect
# Prune data set and tree 
df                       <-data.frame(Thickness = data$Thickness,
                                      Root = data$Root,
                                      Species = data$Species,
                                      Class = data$Class)
row.names(df)            <-data$Species
df                       <-subset(df,Class=="Actinopterygii")
df                       <-as.data.frame(na.omit(df))
tree_pgls                <-keep.tip(tree,row.names(df))


# Brownian motion model
BM                       <- gls(Thickness~Root, 
                                correlation = corPagel(value = 1,phy = tree_pgls,fixed = F),
                                method = "ML",
                                data = df)
summary(BM) # Summary of model

plot(BM, resid(., type="n")~fitted(.), col="blue", main="Normalized Residuals vs. Fitted Values",abline=c(0,0)) #check for heteroscedasticity
res <- resid(BM1, type="n");qqnorm(res, col="blue");qqline(res, col="blue") #check for departures from normal distribution of residuals

# OU model
OU <- gls(Thickness~Root, 
          data=df, 
          correlation=corMartins(0.1, tree_pgls, fixed=FALSE), method="ML")
summary(OU)

aicw(c(AIC(BM),AIC(OU)))
# Conclusion: BM model is best


# Q: Is data heteroscedastic?
plot(BM, resid(., type="n")~fitted(.), col="blue", main="Normalized Residuals vs. Fitted Values",abline=c(0,0)) 
# A: No

# Q: Does the residuals depart from a normal distribution?
res                      <-resid(BM, type="n")
qqnorm(res)
qqline(res) 
# A: No


ggplot(df,aes(x=Root,y=Thickness))+
  scatter+
  stat_function(fun = function(x) BM$coefficients[1]+x*BM$coefficients[2], linetype = "solid",col = "black",lwd = 0.2) +
  labs(x = expression("Root effect (%)"),
       y = expression("Retinal thickness (µm)"))+
  geom_point(size = 1,alpha = 1,pch = 16)->plot7c;plot7c;pdf("./Figures/7C_ThicknessVsRoot.pdf",width = 1.5,height = 1.5,useDingbats = F);plot7c;dev.off()
