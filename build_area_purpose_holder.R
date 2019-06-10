# Date: 2019-04-17
# S Ogletree
# Description: Build dataset with common purpose, holder, and area

library(tidyverse)
library(sf)
# build a dataset of all data ---------------------------------------------

# the county abbreviations
cnty <- read.csv("C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/Papers/NCED/Tables_In/county_key.txt", stringsAsFactors = F)
#cnty <- c("ALB","BLD","CHS","DGL","GVL","LDN","LEB","MES","SAC","SON","WAS","YRK")

gdb <- "C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/Papers/NCED/NCED_CUCED_2018.gdb"

k1 <- read.csv("C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/Papers/NCED/Tables_In/cu_sub.csv", stringsAsFactors = F) %>% 
  filter(variable == "CEHldrTyp" | variable == "CEReas")
k2 <- read.csv("C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/Papers/NCED/Tables_In/nced_sub.csv", stringsAsFactors = F) %>% 
  filter(Field == "OwnType and EHoldType" | Field == "Purpose")

# loop it -----------------------------------------------------------------

dlist <- list()

for (i in 1:12) {
  cuce <- st_read(gdb, layer = paste0(cnty[i,3], "_CE_data"), stringsAsFactors = F)
  st_geometry(cuce) <- NULL
  cuce$CEReas_1 <- as.numeric(cuce$CEReas_1)
  
  nced <- st_read(gdb, layer = paste0("NCED_timeframe_",cnty[i,3]), stringsAsFactors = F)
  st_geometry(nced) <- NULL
  
  cuce2 <- cuce %>% 
    left_join((k1 %>% filter(variable == "CEHldrTyp")), by = c("CEHldrTyp"="code")) %>% 
    left_join((k1 %>% filter(variable == "CEReas")), by = c("CEReas_1"="code"))

  
  nced2 <- nced %>% 
    left_join((k2 %>% filter(Field == "OwnType and EHoldType")), by = c("eholdtype"="Code")) %>% 
    left_join((k2 %>% filter(Field == "Purpose")), by = c("purpose"="Code")) 
  
  cuce3 <- cuce2 %>% 
    filter(YearCEEnd == 3000) %>% 
    select(HolderCommon = Common.x, PurposeCommon = Common.y, Area = CEArea_Ac) %>%
    mutate(cty = cnty[i,3], set = "CUCE")
  nced3 <- nced2 %>% 
    select(HolderCommon = Common.x, PurposeCommon = Common.y, Area = gis_acres) %>%
    mutate(cty = cnty[i,3], set = "NCED")
  Pur <- cuce3 %>% bind_rows(nced3)
  
  dlist[[i]] <- Pur
}

dset <- bind_rows(dlist)

saveRDS(dset, "data_area-purpose-holder.rds")

dset_plot <- dset %>% ggplot(aes(PurposeCommon, log(Area))) + geom_boxplot() + ggtitle("CUCE + NCED")
dset_plot + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme(plot.title = element_text(hjust = 0.5))


dset_CUCE <- filter(dset, set == "CUCE") 
dset_CUCE_plot <- dset_CUCE %>% ggplot(aes(PurposeCommon, log(Area)))+ geom_boxplot() + ggtitle("CUCE")
dset_CUCE_plot + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme(plot.title = element_text(hjust = 0.5))


dset_NCED <- filter(dset, set == "NCED") 
dset_NCED_plot <- dset_NCED %>% ggplot(aes(PurposeCommon, log(Area))) + geom_boxplot() + ggtitle("NCED")
dset_NCED_plot + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme(plot.title = element_text(hjust = 0.5))

dset_plot <- dset %>% ggplot(aes(HolderCommon, log(Area))) + geom_boxplot() + ggtitle("CUCE + NCED")
dset_plot + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme(plot.title = element_text(hjust = 0.5))


dset_CUCE <- filter(dset, set == "CUCE") 
dset_CUCE_plot <- dset_CUCE %>% ggplot(aes(HolderCommon, log(Area)))+ geom_boxplot() + ggtitle("CUCE")
dset_CUCE_plot + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme(plot.title = element_text(hjust = 0.5))


dset_NCED <- filter(dset, set == "NCED") 
dset_NCED_plot <- dset_NCED %>% ggplot(aes(HolderCommon, log(Area))) + geom_boxplot() + ggtitle("NCED")
dset_NCED_plot + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme(plot.title = element_text(hjust = 0.5))


m1 <- lm(Area ~ PurposeCommon, data = dset_CUCE)
plot(m1)
summary(m1)



