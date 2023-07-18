#####CITES, Trade database#####
#####Author: Matsu#############
#####Date:2023/07/19###########

setwd("---")

###install packages ###
pacman::p_load(tidyverse, plyr, dplyr, ggplot2, readr, here, data.table, parallel, ggridges, here, lattice)

### import and append multiple csv ###
mydir = "----" #ウェブサイト(https://trade.cites.org/)からダウンロードしたフォルダーの置き場所
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

cites %>% glimpse()

cites$Term %>% unique()

cites$Taxon %>% unique()

cites$Quantity %>% unique()

### EDA 
#### importer and exporting country infor 
cites$Importer %>% unique() #Importer

citesimpj <- subset(cites, cites$Importer=="JP") #Importer is Japan

citesimpj <- subset(citesj, select = c(Year, Taxon,Importer, Exporter,Class, Quantity))
citesimpj %>% glimpse()

citesimpj$Class %>% unique() #class

citesimpj %>% subset(select = c(Class, Quantity)) %>%
  na.omit(Class) %>% subset(select = c(Quantity)) %>%
  colSums() #321047873
citesimpj %>% subset(select = c(Class, Quantity)) %>%
  group_by(Class) %>% summarise_each(funs(sum)) %>%
  na.omit(Class) %>%  subset(select = c(Quantity))%>% 
  colSums() #321047873 total number imported by Japan

saveRDS(citesimpj, "cites")
### Quantity by year
ggplot(citesjsub, aes(x=Year))+geom_histogram(color="darkblue", fill="lightblue") # by year


### categorized by class and pie chart by class
citesimpj %>% subset(select = c(Class, Quantity)) %>%
  group_by(Class) %>% summarise_each(funs(sum)) %>%
  na.omit(Class) %>%
  ggplot( aes(x="", y=Quantity, fill=Class)) +
  geom_bar(stat="identity", width=1) +
  coord_polar(theta = "y") +
  geom_col(width = 1, color = 1) +
  geom_text(aes(label = paste0(100*Quantity/321047873, "%")), position = position_stack(vjust = 0.5))+
  labs(x = NULL, y = NULL, fill = NULL)+
  guides(fill = guide_legend(title = "Class"))+
  theme_void()


### categorized by countries exporting to Japan
a <- citesimpj %>% subset(select = c(Exporter, Quantity)) %>% 
  group_by(Exporter) %>% summarise_each(funs(sum)) %>%
  na.omit(Exporter) 
a <- a[with(a,order(-Quantity)),]
a <- a[1:10,]
ggplot(a,aes(x=Exporter, y=Quantity))+
  geom_bar(stat="identity")+
  coord_flip()

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
