library("tidyverse")
avengers=read_csv("avengers_pca.csv")
avengers_pca=avengers%>%filter(release_year>=2010)%>%select(c(actor,average_rating,watch_count,like_count,list_count))%>%
  group_by(actor)%>%summarise(mean_rating=mean(average_rating),
                              mean_watch_count=mean(watch_count),
                              mean_like_count=mean(like_count),
                              mean_list_count=mean(list_count))
avengePCA=prcomp(avengers_pca%>%select(-actor),scale=TRUE)
avengers_pca$PC1=avengePCA$x[,"PC1"]
avengers_pca$PC2=avengePCA$x[,"PC2"]

ggplot(data=avengers_pca,aes(x=PC1,y=PC2))+geom_point()+geom_text(label=avengers_pca$actor)
screeplot(avengePCA)
