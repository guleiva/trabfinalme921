library(tidyverse)
library(ggrepel)
library(ggimage)
library(rvest)
library(cluster)
library(factoextra)
library(NbClust)
library(fpc)



#### Extração dos dados ####

url = "https://terraria.fandom.com/wiki/Swords"

terraria_html = read_html(url) 

swords_raw = terraria_html %>%
  html_nodes("table") %>% 
  html_table() %>% 
  .[[1]] %>% 
  .[,-1]



names(swords_raw) = c("Name", "Damage", "Use time", 
                      "Knockback", "Crit", "Autoswing",
                      "Hardmode", "Rarity", "Sell", "Source")

list_imgs = terraria_html %>% 
  html_nodes("img") %>% 
  html_attr("alt") 

list_links = terraria_html %>% 
  html_nodes("img") %>% 
  html_attr("src")

img_links = tibble(Name = list_imgs, URL = list_links) %>% distinct()

swords = swords_raw %>%
  mutate(Name = str_remove_all(swords_raw$Name, "Internal")) %>% 
  separate(Name, c("Name", "ID"), sep = "Item ID:") %>% 
  separate(Damage, c("Damage"), sep = " ") %>% 
  separate(`Use time`, c("Use time"), sep = "\\(") %>% 
  separate(Knockback, c("Knockback"), sep = "\\(") %>% 
  separate(Crit, c("Crit"), sep = " ") %>% 
  mutate(Crit = as.numeric(str_remove(Crit, "\\%"))/100 ) %>% 
  mutate(Autoswing = factor(ifelse(Autoswing == "✓", 1, 0 )))  %>% 
  mutate(Hardmode = factor(ifelse(Hardmode == "✓", 1, 0 ))) %>% 
  mutate(Rarity = factor(Rarity, 
                         levels = c("00*", "01*", "02*", "03*", 
                                    "04*", "05*", "06*", "07*",
                                    "08*", "09*", "10*"))) %>% 
  mutate(ID = as.numeric(ID)) %>% 
  mutate(Damage = as.numeric(Damage)) %>% 
  mutate(`Use time` = as.numeric(`Use time`)) %>% 
  mutate(Knockback = as.numeric(Knockback)) %>% 
  mutate(Name = str_trim(Name)) %>% 
  dplyr::select(-Sell, -Source) %>% 
  left_join(img_links, by = "Name")




#### Análise Descritiva #####

swords %>% 
  ggplot(aes(y = Damage, x = `Use time`)) +
  geom_image(aes(image = URL)) +
  theme_minimal()

swords %>% 
  ggplot(aes(y = Knockback, x = Crit)) +
  geom_image(aes(image = URL)) +
  theme_minimal()

swords %>% 
  group_by(Hardmode) %>% 
  summarise(n = n(), 
            `Média` = round(mean(Damage,na.rm=T),2),
            `D.P.` = sd(Damage,na.rm = T),
            Q1 = quantile(Damage, .25),
            `Q2/Med` = quantile(Damage, .5),
            Q3 = quantile(Damage, .75),
            `Mín.` = min(Damage,na.rm=T),
            `Máx.` = max(Damage,na.rm=T))


#### Clustering ####

swords_df = swords %>% dplyr::select(-ID, -URL) %>% 
  data.frame(row.names = "Name")

gower.dist = daisy(swords_df, metric = "gower")

model1 = diana(gower.dist)
model1.h = as.hclust(model1) 

cstats.table <- function(dist, tree, k) {
  clust.assess <- c("cluster.number","n","within.cluster.ss","average.within","average.between",
                    "wb.ratio","dunn2","avg.silwidth")
  clust.size <- c("cluster.size")
  stats.names <- c()
  row.clust <- c()
  output.stats <- matrix(ncol = k, nrow = length(clust.assess))
  cluster.sizes <- matrix(ncol = k, nrow = k)
  for(i in c(1:k)){
    row.clust[i] <- paste("Cluster-", i, " size")
  }
  for(i in c(2:k)){
    stats.names[i] <- paste("Test", i-1)
    
    for(j in seq_along(clust.assess)){
      output.stats[j, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.assess])[j]
      
    }
    
    for(d in 1:k) {
      cluster.sizes[d, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.size])[d]
      dim(cluster.sizes[d, i]) <- c(length(cluster.sizes[i]), 1)
      cluster.sizes[d, i]
      
    }
  }
  output.stats.df <- data.frame(output.stats)
  cluster.sizes <- data.frame(cluster.sizes)
  cluster.sizes[is.na(cluster.sizes)] <- 0
  rows.all <- c(clust.assess, row.clust)
  # rownames(output.stats.df) <- clust.assess
  output <- rbind(output.stats.df, cluster.sizes)[ ,-1]
  colnames(output) <- stats.names[2:k]
  rownames(output) <- rows.all
  is.num <- sapply(output, is.numeric)
  output[is.num] <- lapply(output[is.num], round, 2)
  output
}

ggplot(data = data.frame(t(cstats.table(gower.dist, model1, 15))), 
       aes(x=cluster.number, y=within.cluster.ss)) + 
  geom_point()+
  geom_line()+
  ggtitle("Divisive clustering") +
  labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
  theme(plot.title = element_text(hjust = 0.5))



### Capturando a média das silhuetas para k = 2:20
silus = NULL
lista = 2:20
for (i in lista) {
  catri = cutree(model1, k = i)
  silus[i] = summary(silhouette(catri, dist = gower.dist))$avg.width
}
silus = silus[-1]

### Gráfico da média das silhuetas para k = 2:20
ggplot(tibble(silus, k = lista), aes(y = silus, x = k)) +
  geom_point() +
  geom_line() +
  labs(y = "Silhueta média", x = "Número de clusteres (k)") +
  scale_x_continuous(breaks = seq(0,20,by=2)) +
  theme_minimal()


plot(silhouette(cutree(model1, k = 14), dist = gower.dist))




fviz_dend(model1.h, k = 14, cex = .2, horiz = T,  
          main = NULL, rect = T, lower_rect = -.1)



