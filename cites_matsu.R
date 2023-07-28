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

#### importer and exporting country information 

citesimpj <- subset(cites, cites$Importer=="JP"& cites$Class!="NA" & cites$Term=="live" & cites$Source=="W") #Importer is Japan, non-plant and term is live

# citesimpj <- subset(citesimpj, select = c(Year, Taxon,Importer, Exporter,Class,Purpose, Quantity))　# select some variables
citesimpj %>% glimpse()


citesimpj %>% subset(select = c(Class, Quantity)) %>%
  na.omit(Class) %>% subset(select = c(Quantity)) %>%
  colSums() #321047873
citesimpj %>% subset(select = c(Class, Quantity)) %>%
  group_by(Class) %>% summarise_each(funs(sum)) %>%
  na.omit(Class) %>%  subset(select = c(Quantity))%>% 
  colSums() #321047873 total number imported by Japan

saveRDS(citesimpj, "cites")

### EDA 
###　日本の年別輸入件数
ggplot(citesimpj, aes(x=Year))+
  geom_histogram(color="darkblue", fill="lightblue", binwidth = 1)+ 
  ylab("取引件数")+
  xlab("年")
  ggsave("取引件数.png")

### 日本の国別輸入件数
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

ggplot(citesimpj_id, aes(x=Family))+
  geom_histogram(color="darkblue", fill="lightblue")+ 
  ylab("取引件数")+
  xlab("学名")
ggsave("取引件数.png")

### アメリカと日本の取引実態
citesimpj_us <- subset(citesimpj, Exporter %in% c("US"))
cites_us_export<-count(citesimpj_us$Family)
write.csv(cites_us_export, "アメリカ_科.csv")

### オーストラリアと日本の取引実態
citesimpj_au <- subset(citesimpj, Exporter %in% c("AU"))
cites_au_export<-count(citesimpj_au$Family)
write.csv(cites_au_export, "au_科.csv")

### マレーシアと日本の取引実態
citesimpj_my <- subset(citesimpj, Exporter %in% c("MY"))
cites_my_export<-count(citesimpj_my$Family)
write.csv(cites_my_export, "my_科.csv")

### 中国と日本の取引実態
citesimpj_cn <- subset(citesimpj, Exporter %in% c("CN"))
cites_cn_export<-count(citesimpj_cn$Family)
write.csv(cites_cn_export, "cn_科.csv")
### heatmap
citesimpjheat <- subset(citesexpj, select = c(Class, Quantity, Importer)) %>%
  group_by(Importer, Class) %>% 
  na.omit(Class) %>%  summarise_each(funs(sum)) %>% 
  spread( Class, Quantity) 

citesimpjheat[is.na(citesimpjheat)] <- 0
citesimpjheat <- citesimpjheat[1:10]
levelplot( t(citesimpjheat[c(nrow(citesimpjheat):1) , ]),
           col.regions=heat.colors(10000))


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
