#Generates a multiplot bar chart for counties comparing NCED vs. CUCED
#Date: 06/07/2019
#BY: D. White and S.Ogletree


library(dplyr)
library(tidyverse)
library(ggplot2)
library(tidyverse)
library(ggplot2)

setwd("C:/Users/whitedl/Box Sync/Default Sync Folder/Projects/NSF_CNH/Papers/NCED/tabulate")

dir()
gdb <- "C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/Papers/NCED/NCED_CUCED_2018.gdb"
# make a list to fill
CE_df_list <- list()
#df_all <- data.frame("county_ID"=character(), "Supersedes"=character(), "CE_Source"=character(), "YearCEAdd"=integer())
# the county abbreviations
cnty <- c("ALB","BLD","CHS","DGL","GVL","LDN","LEB","MES","SAC","SON","WAS","YRK")
# loop to get data
for (i  in 1:12) {
  cname <- cnty[i]
  # get data
  c_layer <- paste0(cname, "_CE_data")
  CE_df <- sf:::st_read(dsn = gdb, layer = c_layer, stringsAsFactors = F, quiet = TRUE)
  sf:::st_geometry(CE_df) <- NULL
  CE_df <- mutate(CE_df, county_ID = cname, CE_Source = "CUCE")
  # get parcels sizes and county ID
  CE_df_list[[i]] <- CE_df %>% select(county_ID, Supersedes, CE_Source, YearCEAdd)
}


# combine all of the county data
alldata_CUCE <- bind_rows(CE_df_list)

NCED_df_list <- list()

for (i  in 1:12) {
  cname <- cnty[i]
  # get data
  n_layer <- paste0("NCED_timeframe_", cname)
  NCED_df <- sf:::st_read(dsn = gdb, layer = n_layer, stringsAsFactors = F, quiet = TRUE)
  sf:::st_geometry(NCED_df) <- NULL
  NCED_df <- mutate(NCED_df, county_ID = paste0(cname), CE_Source = "NCED", Supersedes ="NA", YearCEAdd = year_est)
  print(tally(NCED_df))
  # get parcels sizes and county ID
  NCED_df_list[[i]] <- NCED_df %>% select(county_ID, Supersedes, CE_Source, YearCEAdd)
  #df_all <- bind_rows(df_all, df)
}


# combine all of the county data
alldata_NCED <- bind_rows(NCED_df_list)

all_data <- bind_rows(alldata_CUCE,alldata_NCED)

#### Count of CE's ####

all_dat_group <- group_by(all_data,CE_Source,county_ID) %>% summarize( count = n() )

ggplot(all_dat_group) + geom_bar(aes(x=CE_Source,y=count, fill=CE_Source), stat="identity", width = .5, show.legend = FALSE) + coord_flip() +
  scale_fill_manual(values = c("orange", "blue")) + facet_wrap(~county_ID) + 
  labs(title = "CUCE vs. NCED Counts by County", x='', y='Counts') +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/Papers/NCED/R_Code/CUCE_NCED_counts_by_county.pdf", width = 10, height = 8)

