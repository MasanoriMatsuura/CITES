#####CITES, Trade database#####
#####Author: Matsu#############
#####Date:2023/07/19###########

setwd("---")

###install packages ###
pacman::p_load(tidyverse, plyr, dplyr, ggplot2, readr, here, data.table, parallel, ggridges, here, lattice, devtools)

### import and append multiple csv ###
mydir = "C:/Users/Masanori_Matsuura/Documents/Research/one health/analysis/Trade_database_download_v2023.1/Trade_database_download_v2023.1" #ウェブサイト(https://trade.cites.org/)からダウンロードしたフォルダーの置き場所
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE) #そのフォルダーの中にあるcsvを一括読み込み
dat_csv = ldply(myfiles, read_csv)


# Focusing on Japanese cases ----------------------------------------------

cites <- dat_csv #data_all

cites %>% names()
# [1] "Id"                     "Year"                   "Appendix"
# [4] "Taxon"                  "Class"                  "Order"
# [7] "Family"                 "Genus"                  "Term"
# [10] "Quantity"               "Unit"                   "Importer"
# [13] "Exporter"               "Origin"                 "Purpose"
# [16] "Source"                 "Reporter.type"          "Import.permit.RandomID"
# [19] "Export.permit.RandomID" "Origin.permit.RandomID"

summary(cites$Year)
table(cites$Term=="live")
table(cites$Exporter)
table(cites$Source)

cites %>% glimpse()

cites$Term %>% unique()
cites$Source %>% unique()
cites$Taxon %>% unique()
cites$Quantity %>% unique()
cites$Importer %>% unique() #Importer

##Japan
#### importer and exporting country information 

citesimpj <- subset(cites, cites$Year!="2022" & cites$Importer=="JP" & cites$Term=="live" & cites$Source=="W" & cites$Class==c("Reptilia", "Aves", "Mammalia", "Amphibia"))
#Importer is Japan, non-plant and term is live 哺乳類・鳥類・両生類・爬虫類,
citesimpj[is.na(citesimpj)] <- 0
citesimpj
citesimpj_unit <- citesimpj %>% 
  mutate(Unit = replace(Unit, Unit==0, "q"))

citesimpj <- subset(citesimpj_unit, citesimpj_unit$Unit=="q") ## keep only 個体数


# citesimpj <- subset(citesimpj, select = c(Year, Taxon,Importer, Exporter,Class,Purpose, Quantity))　# select some variables
citesimpj %>% glimpse()
citesimpj$Class %>% unique()
# [1] "Reptilia"       "Aves"           "Mammalia"       "Hirudinoidea"  
#[5] "Anthozoa"       "Actinopteri"    "Amphibia"       "Insecta"       
#[9] "Bivalvia"       "Arachnida"      "Hydrozoa"       "Dipneusti"     
#[13] "Cephalopoda"    "Elasmobranchii" "Gastropoda"  
citesimpj$Unit %>% unique()

citesimpj %>% subset(select = c(Class, Quantity)) %>%
  na.omit(Class) %>% subset(select = c(Quantity)) %>%
  colSums() # 446384 


saveRDS(citesimpj, "cites")

### EDA 
###　日本の年別輸入件数
ggplot(citesimpj, aes(x=Year))+
  geom_histogram(color="darkblue", fill="lightblue", binwidth = 1)+ 
  ylab("取引件数")+
  xlab("年")
  ggsave("取引件数.png")

E### 日本の国別輸入件数
cites_country_export<-count(citesimpj$Exporter)
write.csv(cites_country_export, "取引件数_国.csv")

cites_country_export %>% 
  top_n(10, freq) %>%
  ggplot(aes(x=x, y=freq))+
  geom_bar(color="darkblue", fill="lightblue", stat = "identity")+ 
  ylab("取引件数")+
  xlab("国") # by year
ggsave("取引件数_国.png")


### インドと日本の取引実態
citesimpj_id <- subset(citesimpj, Exporter %in% c("ID"))
cites_id_export<-count(citesimpj_id$Family)
write.csv(cites_id_export, "インド_科.csv")


### マダガスカルと日本の取引実態
citesimpj_us <- subset(citesimpj, Exporter %in% c("MG"))
cites_us_export<-count(citesimpj_us$Family)
write.csv(cites_us_export, "マダガスカル_科.csv")

### アメリカと日本の取引実態
citesimpj_au <- subset(citesimpj, Exporter %in% c("US"))
cites_au_export<-count(citesimpj_au$Family)
write.csv(cites_au_export, "us_科.csv")

### タンザニアと日本の取引実態
citesimpj_my <- subset(citesimpj, Exporter %in% c("TZ"))
cites_my_export<-count(citesimpj_my$Family)
write.csv(cites_my_export, "tz_科.csv")

### ガイアナと日本の取引実態
citesimpj_cn <- subset(citesimpj, Exporter %in% c("GY"))
cites_cn_export<-count(citesimpj_cn$Family)
write.csv(cites_cn_export, "gy_科.csv")


### categorized by countries Importing from Japan
a <- citesexpj %>% subset(select = c(Importer, Quantity)) %>% 
  group_by(Importer) %>% summarise_each(funs(sum)) %>%
  na.omit(Importer) 
a <- a[with(a,order(-Quantity)),]
a <- a[1:10,]
ggplot(a,aes(x=Importer, y=Quantity))+
  geom_bar(stat="identity")+
  coord_flip()

### line chart of year and quantity
ggplot(citesjsub, aes(x=Exporter))+geom_histogram(color="darkblue", fill="lightblue")

##China
#### importer and exporting country information 
citesimpc <- subset(cites, cites$Year!="2022" & cites$Importer=="CN" & cites$Term=="live" & cites$Source=="W" & cites$Class==c("Reptilia", "Aves", "Mammalia", "Amphibia"))
#Importer is China, non-plant and term is live 哺乳類・鳥類・両生類・爬虫類,
citesimpc[is.na(citesimpc)] <- 0
citesimpc
citesimpc_unit <- citesimpc %>% 
  mutate(Unit = replace(Unit, Unit==0, "q"))

citesimpc <- subset(citesimpc_unit, citesimpc_unit$Unit=="q") ## keep only 個体数


# citesimpj <- subset(citesimpj, select = c(Year, Taxon,Importer, Exporter,Class,Purpose, Quantity))　# select some variables
citesimpc %>% glimpse()
citesimpc$Class %>% unique()
# [1] "Reptilia"       "Aves"           "Mammalia"       "Hirudinoidea"  
#[5] "Anthozoa"       "Actinopteri"    "Amphibia"       "Insecta"       
#[9] "Bivalvia"       "Arachnida"      "Hydrozoa"       "Dipneusti"     
#[13] "Cephalopoda"    "Elasmobranchii" "Gastropoda"  
citesimpc$Unit %>% unique()

citesimpc %>% subset(select = c(Class, Quantity)) %>%
  na.omit(Class) %>% subset(select = c(Quantity)) %>%
  colSums() # 515159.7 
saveRDS(citesimpc, "cites")

### EDA 
###　中国の年別輸入件数
ggplot(citesimpc, aes(x=Year))+
  geom_histogram(color="darkblue", fill="lightblue", binwidth = 1)+ 
  ylab("取引件数")+
  xlab("年")
ggsave("取引件数_CN.png")

### 中国の国別輸入件数
cites_country_export<-count(citesimpc$Exporter)
write.csv(cites_country_export, "取引件数_国.csv")

### インドと中国の取引実態
citesimpc_id <- subset(citesimpc, Exporter %in% c("ID"))
cites_id_export<-count(citesimpc_id$Family)
write.csv(cites_id_export, "インド_科_CN.csv")


### マレーシアと中国の取引実態
citesimpc_us <- subset(citesimpc, Exporter %in% c("MY"))
cites_us_export<-count(citesimpc_us$Family)
write.csv(cites_us_export, "マレーシア_科_CN.csv")

### ガイアナと中国の取引実態
citesimpc_au <- subset(citesimpc, Exporter %in% c("GY"))
cites_au_export<-count(citesimpc_au$Family)
write.csv(cites_au_export, "ガイアナ_科_CN.csv")

### 日本と中国の取引実態
citesimpc_my <- subset(citesimpc, Exporter %in% c("JP"))
cites_my_export<-count(citesimpc_my$Family)
write.csv(cites_my_export, "日本_科_CN.csv")

### スリナム中国の取引実態
citesimpc_cn <- subset(citesimpc, Exporter %in% c("SR"))
cites_cn_export<-count(citesimpc_cn$Family)
write.csv(cites_cn_export, "SR_科_CN.csv")


##Thailand
#### importer and exporting country information 
citesimpt <- subset(cites, cites$Year!="2022" & cites$Importer=="TH" & cites$Term=="live" & cites$Source=="W" & cites$Class==c("Reptilia", "Aves", "Mammalia", "Amphibia"))
#Importer is China, non-plant and term is live 哺乳類・鳥類・両生類・爬虫類,
citesimpt[is.na(citesimpt)] <- 0
citesimpt
citesimpt_unit <- citesimpt %>% 
  mutate(Unit = replace(Unit, Unit==0, "q"))

citesimpt <- subset(citesimpt_unit, citesimpt_unit$Unit=="q") ## keep only 個体数


# citesimpj <- subset(citesimpj, select = c(Year, Taxon,Importer, Exporter,Class,Purpose, Quantity))　# select some variables
citesimpt %>% glimpse()
citesimpt$Class %>% unique()
# [1] "Reptilia"       "Aves"           "Mammalia"       "Hirudinoidea"  
#[5] "Anthozoa"       "Actinopteri"    "Amphibia"       "Insecta"       
#[9] "Bivalvia"       "Arachnida"      "Hydrozoa"       "Dipneusti"     
#[13] "Cephalopoda"    "Elasmobranchii" "Gastropoda"  
citesimpt$Unit %>% unique()

citesimpt %>% subset(select = c(Class, Quantity)) %>%
  na.omit(Class) %>% subset(select = c(Quantity)) %>%
  colSums() #  40374 
saveRDS(citesimpt, "cites")

### EDA 
###　タイの年別輸入件数
ggplot(citesimpt, aes(x=Year))+
  geom_histogram(color="darkblue", fill="lightblue", binwidth = 1)+ 
  ylab("取引件数")+
  xlab("年")
ggsave("取引件数_TH.png")

### タイの国別輸入件数
cites_country_export<-count(citesimpt$Exporter)
write.csv(cites_country_export, "取引件数_国_TH.csv")

cites_country_export %>% 
  top_n(10, freq) %>%
  ggplot(aes(x=x, y=freq))+
  geom_bar(color="darkblue", fill="lightblue", stat = "identity")+ 
  ylab("取引件数")+
  xlab("国") # by year
ggsave("取引件数_国_TH.png")