
# import Data from taipei456_cluster.csv

Data %>%  mutate(用電 = log(戶均用電),
                   扶養比 = (青少年人口+老年人口)/壯年人口,
                   所得中位log = log10(中位數),
                   老年比例 = 每戶平均老年人口數.人./每戶平均人數,
                   單身率  = 1 - 有偶比例...) %>% 
  select(行政區域,分群,  X1人一宅宅數比例... ,X6人以上一宅宅數比例..., 房價中位數,大學以下比例, 扶養比,老年比例 ,單身率, 戶均用電) -> gogocluster
names(gogocluster) = c("Region","Cluster", "1_live_rate", "6_live_rate", "Med_housing_price", "Education", 
                       "Dependency_ratio","Eldery_one", "Single_rate", "Electricity")
### 階層式分群
centered.x <- scale(gogocluster[,-c(1,2)],center=TRUE,scale=TRUE)
x<-daisy(centered.x, stand=T)
agn<-agnes(x,metric="euclidean",method="ward")
plot(agn,which.plots = 2)
agn$ac

## scale 跟為一到0,1之間
gogocluster %>%
  mutate_each(funs(scale(.,center=TRUE,scale=TRUE)),-c(Region,Cluster))  %>% 
  gather('index','value',-c(Region,Cluster)) %>% 
  group_by(index) %>%
  mutate(value = (value-min(value))/(max(value)-min(value))) %>% 
  spread(index,value) -> data.h



data.h %>% 
  select(-1) %>% 
  group_by(Cluster) %>% 
  summarise_each(funs(median)) %>% 
  gather('index','value',-Cluster) %>%
  spread(Cluster,value) -> cluster_rader


cluster_rader$mean <- rowMeans(cluster_rader[,-1])
cluster_rader$med <- apply(cluster_rader[,-1],1,median)
cluster_rader %>% 
  mutate_each(funs(round(.,digits=3)),-index) -> cluster_rader

cluster_rader[which(cluster_rader$index ==  'Electricity'), ][2:8] = 
  cluster_rader[which(cluster_rader$index ==  'Electricity'), ][2:8] + 0.3

cluster_rader = cluster_rader[c(5,2,4,1,8,7,3),]

#write.csv(cluster_rader, "radar_plot.csv", fileEncoding = "utf8", row.names = F)

## color
col.raw <- c("#1d3156","#ff9c63","#7dbfc6","#00b1c9","#ea8ca7","#ffd2a0") 

## 推疊雷達圖 #####
highchart() %>% 
  hc_chart(polar = TRUE, type = "line") %>% 
  hc_title(text = "中位數") %>% 
  hc_xAxis(categories = cluster_rader$index,
           tickmarkPlacement = 'on',
           lineWidth = 0) %>% 
  hc_yAxis(gridLineInterpolation = 'polygon',
           lineWidth = 0,
           min = 0, max = 1) %>%
  hc_series(
    list(
      name = "cluster 1 ",
      data = cluster_rader$`第一群`,
      pointPlacement = 'on',color=col.raw[2]),
    list(
      name = "cluster 2 - ",
      data = cluster_rader$`第二群`,
      pointPlacement = 'on',color=col.raw[3]),
    list(
      name = "cluster 3 - ",
      data = cluster_rader$`第三群`,
      pointPlacement = 'on',color=col.raw[4]),
    list(
      name = "cluster 4 -",
      data = cluster_rader$`第四群`,
      pointPlacement = 'on',color=col.raw[5]),
    list(
      name = "cluster 5 ",
      data = cluster_rader$`第五群`,
      pointPlacement = 'on',color=col.raw[6]),
    list(
      name = "Total median",
      data = cluster_rader$med,
      pointPlacement = 'on',color= col.raw[1])
  )