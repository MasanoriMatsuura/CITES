#####CITES, Trade database#####
#####Author: Matsu#############
#####Date:2023/07/19###########

setwd("---")

###install packages ###
pacman::p_load(cowplot, patchwork, tidyverse, plyr, dplyr, ggplot2, readr, here, data.table, parallel, ggridges, here, lattice, devtools)

### import and append multiple csv ###
mydir = "C:/Users/Masanori_Matsuura/Documents/Research/one health/analysis/Trade_database_download_v2023.1/Trade_database_download_v2023.1" #ウェブサイト(https://trade.cites.org/)からダウンロードしたフォルダーの置き場所
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE) #そのフォルダーの中にあるcsvを一括読み込み
dat_csv = ldply(myfiles, read_csv)


# Focusing on Japanese cases ----------------------------------------------
cites <-  readRDS("C:/Users/mm_wi/Documents/research/onehealth/R/cites.rds")

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
  colSums() #446384 


saveRDS(citesimpj, "cites")

### EDA 
###　日本の年別輸入件数
ggplot(citesimpj, aes(x=Year))+
  geom_histogram(color="darkblue", fill="lightblue", binwidth = 1)+ 
  ylab("取引件数")+
  xlab("年")
  ggsave("取引件数.png")

japan_import_year <- aggregate( .~ Year, data = citesimpj, FUN = length)
japan_import_year <- subset(japan_import_year, select = c(Year, Quantity)) 
japan_import_year <- sapply(japan_import_year, as.numeric)
japan_import_year <- as.data.frame(japan_import_year)
print(japan_import_year) #輸入件数

japan_case_pot <- plot(japan_import_year$Year, y=japan_import_year$Quantity,
                       xlab='Year', ylab='輸入件数', ylim = c(0,600), type="l")

###日本実質GDP per capita (USドル)
gdp <- read.csv("C:\\Users\\mm_wi\\Documents\\research\\onehealth\\R\\gdp\\API_NY.GDP.PCAP.KD_DS2_en_csv_v2_6298445.csv", skip = 4)
japan <- gdp %>% filter(Country.Name == "Japan") ## extract japan
new_names <- gsub("X", "", names(japan))
japan <- setNames(japan, new_names)
japan <- gather(japan, key = Year, value = "GDP")
japan <- japan[-c(1:34, 66:68),]
japan <- sapply(japan, as.numeric)
japan <- as.data.frame(japan)　#japan GDPデータ

### GDP per capitaと件数
par(oma = c(0, 1, 0, 3))
japan_case_pot <- plot(japan_import_year$Year, y=japan_import_year$Quantity,
                       xlab='Year', ylab='輸入件数', ylim = c(0,600), type="l")
axis(2)
par(new = T)
japan_gdpp_case <- plot(japan$Year, japan$GDP, ylim = c(15000,36000),
                        xlab='', ylab='',
                        type='l', lty='dotted', axes = F)
mtext('実質GDP per capita (USドル)', side = 4, line = 3)
axis(4)
legend("bottomleft", legend = c("輸入件数", "GDP per capita"),
       lty = c('solid', 'dotted'), bty="n", cex=1)

###日本GDP
gdp_p <- read.csv("C:\\Users\\mm_wi\\Documents\\research\\onehealth\\R\\gdeppercapita\\API_NY.GDP.MKTP.KD_DS2_en_csv_v2_6298496.csv", skip = 4)
japan_p <- gdp_p %>% filter(Country.Name == "Japan") ## extract japan_p
new_names <- gsub("X", "", names(japan_p))
japan_p <- setNames(japan_p, new_names)
japan_p <- gather(japan_p, key = Year, value = "GDP")
japan_p <- japan_p[-c(1:34, 66:68),]
japan_p <- sapply(japan_p, as.numeric)
japan_p <- as.data.frame(japan_p)　#japan_p GDPデータ

###GDPと件数
par(oma = c(0, 1, 0, 3))
plot(japan_import_year$Year, y=japan_import_year$Quantity,
                       xlab='Year', ylab='輸入件数', ylim = c(0,600), type="l")
axis(2)
par(new = T)
plot(japan_p$Year, japan_p$GDP, ylim = c(1500000000000,4600000000000),
                        xlab='', ylab='',
                        type='l', lty='dotted', axes = F)
mtext('実質GDP (USドル)', side = 4, line = 3)
axis(4)
legend("bottomleft", legend = c("輸入件数", "GDP"),
       lty = c('solid', 'dotted'), bty="n", cex=1)

### 日本の輸入動物
### 日本の動物別輸入件数
cites_taxon_import<-count(citesimpj$Family)
write.csv(cites_taxon_import, "jp_taxon.csv")


### カメレオン科
#### 国
japan_chamae <- subset(citesimpj, Family %in% c("Chamaeleonidae"))
japan_chamae <- count(japan_chamae$Exporter)
write.csv(japan_chamae, "japan_chamae.csv")
#### 目的
japan_chamae_p <- subset(citesimpj, Family %in% c("Chamaeleonidae"))
japan_chamae_p <- count(japan_chamae_p$Purpose)
write.csv(japan_chamae_p, "japan_chamae_p.csv")

### オオトカゲ科
#### 国
japan_Varanidae <- subset(citesimpj, Family %in% c("Varanidae"))
japan_Varanidae <- count(japan_Varanidae$Exporter)
write.csv(japan_Varanidae, "japan_Varanidae.csv")
#### 目的
japan_Varanidae_p <- subset(citesimpj, Family %in% c("Varanidae"))
japan_Varanidae_p <- count(japan_Varanidae_p$Purpose)
write.csv(japan_Varanidae_p, "japan_Varanidae_p.csv")

###　ニシキヘビ科
#### 国
japan_Pythonidae <- subset(citesimpj, Family %in% c("Pythonidae"))
japan_Pythonidae <- count(japan_Pythonidae$Exporter)
write.csv(japan_Pythonidae, "japan_Pythonidae.csv")
#### 目的
japan_Pythonidae_p <- subset(citesimpj, Family %in% c("Pythonidae"))
japan_Pythonidae_p <- count(japan_Pythonidae_p$Purpose)
write.csv(japan_Pythonidae_p, "japan_Pythonidae_p.csv")

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
###　中国の年別輸入件数と輸入量
china_import_year <- aggregate( .~ Year, data = citesimpc, FUN = length)
china_import_year <- subset(china_import_year, select = c(Year, Quantity)) 
china_import_year <- sapply(china_import_year, as.numeric)
china_import_year <- as.data.frame(china_import_year)
print(china_import_year) #輸入件数

china_case <- ggplot(citesimpc, aes(x=Year))+
  geom_histogram(color="darkblue", fill="lightblue", binwidth = 1)+ 
  ylab("取引件数")+
  xlab("年") #輸入件数
ggsave("取引件数_CN.png")
print(china_case)

china_case <- ggplot(china_import_year, aes(x = Year)) +
  geom_line(aes(y = Quantity)) +
  labs(x = "年", y = "輸入件数") +
  theme_minimal()
print(china_case)

###中国実質GDP per capita (USドル)
gdp <- read.csv("C:\\Users\\mm_wi\\Documents\\research\\onehealth\\R\\gdp\\API_NY.GDP.PCAP.KD_DS2_en_csv_v2_6298445.csv", skip = 4)
china <- gdp %>% filter(Country.Name == "China") ## extract china
new_names <- gsub("X", "", names(china))
china <- setNames(china, new_names)
china <- gather(china, key = Year, value = "GDP")
china <- china[-c(1:34, 66:68),]
china <- sapply(china, as.numeric)
china <- as.data.frame(china)　#china GDPデータ

### GDP per capitaと件数
par(oma = c(0, 1, 0, 3))
china_case_pot <- plot(china_import_year$Year, y=china_import_year$Quantity,
                       xlab='Year', ylab='輸入件数', ylim = c(0,500), type="l")
axis(2)
par(new = T)
china_gdpp_case <- plot(china$Year, china$GDP, ylim = c(0,13000),
                        xlab='', ylab='',
                        type='l', lty='dotted', axes = F)
mtext('実質GDP per capita (USドル)', side = 4, line = 3)
axis(4)
legend("topleft", legend = c("輸入件数", "GDP per capita"),
       lty = c('solid', 'dotted'), bty="n", cex=1)

###中国GDP
gdp_p <- read.csv("C:\\Users\\mm_wi\\Documents\\research\\onehealth\\R\\gdeppercapita\\API_NY.GDP.MKTP.KD_DS2_en_csv_v2_6298496.csv", skip = 4)
china_p <- gdp_p %>% filter(Country.Name == "China") ## extract china_p
new_names <- gsub("X", "", names(china_p))
china_p <- setNames(china_p, new_names)
china_p <- gather(china_p, key = Year, value = "GDP")
china_p <- china_p[-c(1:34, 66:68),]
china_p <- sapply(china_p, as.numeric)
china_p <- as.data.frame(china_p)　#china_p GDPデータ

###GDPと件数
par(oma = c(0, 1, 0, 3))
plot(china_import_year$Year, y=china_import_year$Quantity,
     xlab='Year', ylab='輸入件数', ylim = c(0,500), type="l")
axis(2)
par(new = T)
plot(china_p$Year, china_p$GDP, ylim = c(0,15000000000000),
     xlab='', ylab='',
     type='l', lty='dotted', axes = F)
mtext('実質GDP (USドル)', side = 4, line = 3)
axis(4)
legend("topleft", legend = c("輸入件数", "GDP"),
       lty = c('solid', 'dotted'), bty="n", cex=1)

### 中国の国別輸入件数
cites_country_export<-count(citesimpc$Exporter)
write.csv(cites_country_export, "取引件数_国.csv")

### 中国の輸入動物
### 中国の動物別輸入件数
cites_taxon_import<-count(citesimpc$Family)
write.csv(cites_taxon_import, "cn_taxon.csv")

### イシガメ科
#### 国
china_Geoemydidae <- subset(citesimpc, Family %in% c("Geoemydidae"))
china_Geoemydidae <- count(china_Geoemydidae$Exporter)
write.csv(china_Geoemydidae, "china_Geoemydidae.csv")
#### 目的
china_Geoemydidae_p <- subset(citesimpc, Family %in% c("Geoemydidae"))
china_Geoemydidae_p <- count(china_Geoemydidae_p$Purpose)
write.csv(china_Geoemydidae_p, "china_Geoemydidae_p.csv")

### スッポン科
#### 国
china_Trionychidae <- subset(citesimpc, Family %in% c("Trionychidae"))
china_Trionychidae <- count(china_Trionychidae$Exporter)
write.csv(china_Trionychidae, "china_Trionychidae.csv")
#### 目的
china_Trionychidae_p <- subset(citesimpc, Family %in% c("Trionychidae"))
china_Trionychidae_p <- count(china_Trionychidae_p$Purpose)
write.csv(china_Trionychidae_p, "china_Trionychidae_p.csv")

###　インコ科
#### 国
china_Psittacidae <- subset(citesimpc, Family %in% c("Psittacidae"))
china_Psittacidae <- count(china_Psittacidae$Exporter)
write.csv(china_Psittacidae, "china_Psittacidae.csv")
#### 目的
china_Psittacidae_p <- subset(citesimpc, Family %in% c("Psittacidae"))
china_Psittacidae_p <- count(china_Psittacidae_p$Purpose)
write.csv(china_Psittacidae_p, "china_Psittacidae_p.csv")

###　尾長ざる科
#### 国
china_Cercopithecidae <- subset(citesimpc, Family %in% c("Cercopithecidae"))
china_Cercopithecidae <- count(china_Cercopithecidae$Exporter)
write.csv(china_Cercopithecidae, "china_Cercopithecidae.csv")
#### 目的
china_Cercopithecidae_p <- subset(citesimpc, Family %in% c("Cercopithecidae"))
china_Cercopithecidae_p <- count(china_Cercopithecidae_p$Purpose)
write.csv(china_Cercopithecidae_p, "china_Cercopithecidae_p.csv")

##タイ
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
thai_import_year <- aggregate( .~ Year, data = citesimpt, FUN = length)
thai_import_year <- subset(thai_import_year, select = c(Year, Quantity)) 
thai_import_year <- sapply(thai_import_year, as.numeric)
thai_import_year <- as.data.frame(thai_import_year)
print(thai_import_year) #輸入件数

thai_case <- ggplot(citesimpt, aes(x=Year))+
  geom_histogram(color="darkblue", fill="lightblue", binwidth = 1)+ 
  ylab("取引件数")+
  xlab("年") #輸入件数
ggsave("取引件数_CN.png")
print(thai_case)

thai_case <- ggplot(thai_import_year, aes(x = Year)) +
  geom_line(aes(y = Quantity)) +
  labs(x = "年", y = "輸入件数") +
  theme_minimal()
print(thai_case)

###タイ実質GDP per capita (USドル)
gdp <- read.csv("C:\\Users\\mm_wi\\Documents\\research\\onehealth\\R\\gdp\\API_NY.GDP.PCAP.KD_DS2_en_csv_v2_6298445.csv", skip = 4)
thai <- gdp %>% filter(Country.Name == "Thailand") ## extract thai
new_names <- gsub("X", "", names(thai))
thai <- setNames(thai, new_names)
thai <- gather(thai, key = Year, value = "GDP")
thai <- thai[-c(1:34, 66:68),]
thai <- sapply(thai, as.numeric)
thai <- as.data.frame(thai)　#thai GDPデータ

### GDP per capitaと件数
par(oma = c(0, 1, 0, 3))
thai_case_pot <- plot(thai_import_year$Year, y=thai_import_year$Quantity,
                      xlab='Year', ylab='輸入件数', ylim = c(0,200), type="l")
axis(2)
par(new = T)
thai_gdpp_case <- plot(thai$Year, thai$GDP, ylim = c(2000,7000),
                       xlab='', ylab='',
                       type='l', lty='dotted', axes = F)
mtext('実質GDP per capita (USドル)', side = 4, line = 3)
axis(4)
legend("topleft", legend = c("輸入件数", "GDP per capita"),
       lty = c('solid', 'dotted'), bty="n", cex=1)

###タイGDP
gdp_p <- read.csv("C:\\Users\\mm_wi\\Documents\\research\\onehealth\\R\\gdeppercapita\\API_NY.GDP.MKTP.KD_DS2_en_csv_v2_6298496.csv", skip = 4)
thai_p <- gdp_p %>% filter(Country.Name == "Thailand") ## extract thai_p
new_names <- gsub("X", "", names(thai_p))
thai_p <- setNames(thai_p, new_names)
thai_p <- gather(thai_p, key = Year, value = "GDP")
thai_p <- thai_p[-c(1:34, 66:68),]
thai_p <- sapply(thai_p, as.numeric)
thai_p <- as.data.frame(thai_p)　#thai_p GDPデータ

###GDPと件数
par(oma = c(0, 1, 0, 3))
plot(thai_import_year$Year, y=thai_import_year$Quantity,
     xlab='Year', ylab='輸入件数', ylim = c(0,200), type="l")
axis(2)
par(new = T)
plot(thai_p$Year, thai_p$GDP, ylim = c(0,450000000000),
     xlab='', ylab='',
     type='l', lty='dotted', axes = F)
mtext('実質GDP (USドル)', side = 4, line = 3)
axis(4)
legend("topleft", legend = c("輸入件数", "GDP"),
       lty = c('solid', 'dotted'), bty="n", cex=1)

### タイの動物別輸入件数
cites_taxon_import<-count(citesimpt$Family)
write.csv(cites_taxon_import, "th_taxon.csv")

###　インコ科
#### 国
thai_Psittacidae <- subset(citesimpt, Family %in% c("Psittacidae"))
thai_Psittacidae <- count(thai_Psittacidae$Exporter)
write.csv(thai_Psittacidae, "thai_Psittacidae.csv")
#### 目的
thai_Psittacidae_p <- subset(citesimpt, Family %in% c("Psittacidae"))
thai_Psittacidae_p <- count(thai_Psittacidae_p$Purpose)
write.csv(thai_Psittacidae_p, "thai_Psittacidae_p.csv")

###　カメレオン科
#### 国
thai_Chamaeleonidae <- subset(citesimpt, Family %in% c("Chamaeleonidae"))
thai_Chamaeleonidae <- count(thai_Chamaeleonidae$Exporter)
write.csv(thai_Chamaeleonidae, "thai_Chamaeleonidae.csv")
#### 目的
thai_Chamaeleonidae_p <- subset(citesimpt, Family %in% c("Chamaeleonidae"))
thai_Chamaeleonidae_p <- count(thai_Chamaeleonidae_p$Purpose)
write.csv(thai_Chamaeleonidae_p, "thai_Chamaeleonidae_p.csv")

###　オオハシ科
#### 国
thai_Ramphastidae <- subset(citesimpt, Family %in% c("Ramphastidae"))
thai_Ramphastidae <- count(thai_Ramphastidae$Exporter)
write.csv(thai_Ramphastidae, "thai_Ramphastidae.csv")
#### 目的
thai_Ramphastidae_p <- subset(citesimpt, Family %in% c("Ramphastidae"))
thai_Ramphastidae_p <- count(thai_Ramphastidae_p$Purpose)
write.csv(thai_Ramphastidae_p, "thai_Ramphastidae_p.csv")

###　オマキザル科
#### 国
thai_Cebidae <- subset(citesimpt, Family %in% c("Cebidae"))
thai_Cebidae <- count(thai_Cebidae$Exporter)
write.csv(thai_Cebidae, "thai_Cebidae.csv")
#### 目的
thai_Cebidae_p <- subset(citesimpt, Family %in% c("Cebidae"))
thai_Cebidae_p <- count(thai_Cebidae_p$Purpose)
write.csv(thai_Cebidae_p, "thai_Cebidae_p.csv")