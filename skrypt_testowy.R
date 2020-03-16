data<-read.csv("german_credit_data_weka_dataset.csv") #Source: https://www.mldata.io/dataset-details/german_credit_data/#customize_download

###Link do Gita: https://github.com/marcinnako/WUMPROJ1
###Link do OG-Gita: https://github.com/mini-pw/2020L-WUM

library(ggplot2)

p <- ggplot(data = data,aes(x = duration,y = credit_amount)) +
  geom_point()
p