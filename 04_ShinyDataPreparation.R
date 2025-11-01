#######################################################################
##  Title: Semantic proximity in quantum and general scientific publications: 
## A BERTopic approach to mapping regional knowledge spaces
##  goal : Shiny Visualization
##  Made by: Keungoui Kim (Ph.D.) & Jisoo Hur (Ph.D.) 
##  Data set: WoS
##  Time Span: 
##  Variables
##      Input: 
##      Output:  
##  Methodology: 
##  Time-stamp:   
##  Notice :

library(dplyr)
library(magrittr)
library(maptools)
library(rgdal)
library(sf)
library('rmapshaper')
library(tmap)

############
# Data Conversion to .RData
############

qsp_pub_bertopic <- read.csv("../SemanticProximity_research/pub_bertopic_chatgpt.csv")
qsp_pub_bertopic %<>% rename(NUTS_ID=eu_nuts_id)
save(qsp_pub_bertopic, file="R file/qsp_pub_bertopic.RData")

qsp_quant_pub_bertopic <- read.csv("../SemanticProximity_research/quantum_pub_bertopic_chatgpt.csv")
qsp_quant_pub_bertopic %<>% rename(NUTS_ID=eu_nuts_id)
save(qsp_quant_pub_bertopic, file="R file/qsp_quant_pub_bertopic.RData")

qsp_bert_similarity <- read.csv("../SemanticProximity_research/similarity_bert.csv")
qsp_bert_similarity %<>% rename(NUTS_ID=eu_nuts_id)
save(qsp_bert_similarity, file="R file/qsp_bert_similarity.RData")

qsp_eu_pub_set <- read.csv("../SemanticProximity_research/num_publication_entire_regions.csv")
qsp_eu_pub_set %<>% rename(NUTS_ID=eu_nuts_id)
save(qsp_eu_pub_set, file="R file/qsp_eu_pub_set.RData")

############
# Shiny: Map
############

EU_NUTS <- 
  readOGR(dsn = "./NUTS_2013_01M_SH/data", layer = "NUTS_RG_01M_2013")
# filtering only NUTS-3
EU_NUTS <- EU_NUTS[EU_NUTS@data$STAT_LEVL_!=0, ]
EU_NUTS <- EU_NUTS[EU_NUTS@data$STAT_LEVL_>=3, ]

qsp_eu_pub_set %>% head

ind_sf <- st_as_sf(EU_NUTS) %>% 
  left_join(quant_pub_bertopic %>% select(NUTS_ID) %>% 
              unique %>% mutate(quant.region=TRUE)) %>%
  left_join(qsp_eu_pub_set %>% group_by(NUTS_ID) %>% 
              # summarize(across(everything(), ~ mean(log(. + 1), na.rm = TRUE), .names = "log_mean_{col}")) %>%
              summarize(num_publication=mean(num_publication),
                        num_quant_publication=mean(num_quant_publication),
                        ratio_quant=mean(ratio_quant),
                        lnum_publication=mean(log(num_publication+1)),
                        lnum_quant_publication=mean(log(num_quant_publication+1)),
                        lratio_quant=mean(log(ratio_quant+1))) %>% ungroup)
save(ind_sf, file="R file/ind_sf.RData")

# Map Visualization
load(file="R file/ind_sf.RData")
eu.reg.map <- 
  tm_shape(ind_sf %>% 
             filter(substr(NUTS_ID,1,3)!="ES7" & 
                      substr(NUTS_ID,1,3)!="FRA" & 
                      substr(NUTS_ID,1,3)!="PT2" & 
                      substr(NUTS_ID,1,3)!="PT3")) + 
  tm_fill("lightgrey") + tm_borders("grey25", alpha=.5) +
  tm_shape(ind_sf %>% filter(quant.region==TRUE)) + 
  tm_fill("lightblue") + tm_borders("grey25", alpha=.5) 
eu.reg.map
save(eu.reg.map, file="R file/eu.reg.map.RData")

### Map presentations
tm_shape(ind_sf %>% 
           filter(substr(NUTS_ID,1,3)!="ES7" & 
                    substr(NUTS_ID,1,3)!="FRA" & 
                    substr(NUTS_ID,1,3)!="PT2" & 
                    substr(NUTS_ID,1,3)!="PT3")) + 
  tm_polygons("num_publication", 
              title = "Publications") +
  tm_layout(legend.outside = TRUE)

tm_shape(ind_sf %>% 
           filter(substr(NUTS_ID,1,3)!="ES7" & 
                    substr(NUTS_ID,1,3)!="FRA" & 
                    substr(NUTS_ID,1,3)!="PT2" & 
                    substr(NUTS_ID,1,3)!="PT3")) + 
  tm_polygons("lnum_publication", 
              title = "(log-scale) Publications") +
  tm_layout(legend.outside = TRUE)

tm_shape(ind_sf %>% 
           filter(substr(NUTS_ID,1,3)!="ES7" & 
                    substr(NUTS_ID,1,3)!="FRA" & 
                    substr(NUTS_ID,1,3)!="PT2" & 
                    substr(NUTS_ID,1,3)!="PT3")) + 
  tm_polygons("num_quant_publication", 
              title = "Quantum Publications") +
  tm_layout(legend.outside = TRUE)

tm_shape(ind_sf %>% 
           filter(substr(NUTS_ID,1,3)!="ES7" & 
                    substr(NUTS_ID,1,3)!="FRA" & 
                    substr(NUTS_ID,1,3)!="PT2" & 
                    substr(NUTS_ID,1,3)!="PT3")) + 
  tm_polygons("ratio_quant", 
              title = "Quantum Publication Ratio") +
  tm_layout(legend.outside = TRUE)
  

############
# Shiny: Data Preparation
############

load(file="R file/qsp_pub_bertopic.RData")
load(file="R file/qsp_quant_pub_bertopic.RData")
load(file="R file/qsp_bert_similarity.RData")
load(file="R file/ind_sf.RData")
load(file="R file/metro.map.RData")

head(qsp_pub_bertopic)
head(qsp_quant_pub_bertopic)
head(qsp_bert_similarity)

library(ggplot2)
qsp_bert_similarity %>%
  filter(NUTS_ID=="UKM25") %>%
  ggplot(aes(x=period, y=bert_similarity, group=1)) +
  geom_line() + 
  labs(title = "UKM25", x = "Period", y = "BERT Similarities") +
  theme_bw()



