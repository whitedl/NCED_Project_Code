# Date: 2019-04-17
# S Ogletree
# Description: Build dataset with common purpose, holder, and area

library(tidyverse)
library(sf)
# build a dataset of all data ---------------------------------------------

cnty <- read.csv("county_key.txt", stringsAsFactors = F)

gdb <- "../NCED_CUCED_2018.gdb"

k1 <- read.csv("cu_sub.csv", stringsAsFactors = F) %>% 
  filter(variable == "CEHldrTyp" | variable == "CEReas")
k2 <- read.csv("nced_sub.csv", stringsAsFactors = F) %>% 
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

dset %>% ggplot(aes(PurposeCommon, log(Area))) + geom_boxplot()

# m1 <- lm(Area ~ PurposeCommon + HolderCommon, data = dset)
# plot(m1)
