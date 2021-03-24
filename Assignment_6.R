# Knowledge Mining: Text mining
# File: textdata_mining02.R
# Theme: Download Twitter data for network and sentiment analyses

## Install packages need for Twitter data download

install.packages(c("rtweet","igraph","tidyverse","ggraph","data.table"), repos = "https://cran.r-project.org")

## Load packages

library(rtweet)
library(igraph)
library(tidyverse)
library(ggraph)
library(data.table)

## Acquire API key and token from Twitter developer website
# Check https://datageneration.org/adp/twitter/ for detail

# Create token for direct authentication to access Twitter data
# Enter key and tokens from Twitter developer account 
# (Developer Portal--> Projects & Apps --> under Apps, Keys and tokens)
twitter_token <- rtweet::create_token(
app="Data methods Web data",
consumer_key <- "3aBeG3yYXjdafXgUBGt5RDK7N",
consumer_secret <- "QMBowAbJgRVGbYr1kuMuZIf2viqIwpanh5GoAzGqk3JgxMORcy",
access_token_key <- "785910745506521088-xmkYhshGvP5CBSnAGGhWWTqpL225Ssu",
access_secret <- "32AIv4dEkzNFpSGjsUWsDWzdOeElRZgMvwQz5Nme71gpL")

## search for 1000 tweets by keyword in English
jb <- rtweet::search_tweets(q = "JoeBiden", n = 1000, lang = "en", token = twitter_token)

## preview users data
users_data(jb)

# Get tweets by userid
jb_timeline = get_timelines("JoeBiden", n = 1000)

## plot time series of tweets frequency
ts_plot(jb_timeline, by = "days") + theme_bw()

## Boolean search for large quantity of tweets (which could take a while)
jb1 <- rtweet::search_tweets("JoeBiden OR president OR potus", n = 100,
                             retryonratelimit = TRUE)

# Graphing retweet connections
## Warning: could take some time to create the igraph
## Suggestion: start from a smaller data file
## Credit: Russell, Matthew. 2018. 21 Recipes for Mining Twitter Data with rtweet
## https://rud.is/books/21-recipes/visualizing-a-graph-of-retweet-relationships.html


## Create igraph object from Twitter data using user id and mentioned id.
## ggraph draws the network graph in different layouts (12). 

filter(jb, retweet_count > 0 ) %>% 
  select(screen_name, mentions_screen_name) %>%
  unnest(mentions_screen_name) %>% 
  filter(!is.na(mentions_screen_name)) %>% 
  graph_from_data_frame() -> jb_g
V(jb_g)$node_label <- unname(ifelse(degree(jb_g)[V(jb_g)] > 20, names(V(jb_g)), "")) 
V(jb_g)$node_size <- unname(ifelse(degree(jb_g)[V(jb_g)] > 20, degree(jb_g), 0)) 

# ggraph layouts: 'star', 'circle', 'gem', 'dh', 'graphopt', 'grid', 'mds', 
# 'randomly', 'fr', 'kk', 'drl', 'lgl'
# Davidson-Harel algorithm
# Try also fr (fruchterman reingold)

ggraph(jb_g, layout = 'dh') + 
  geom_edge_arc(edge_width=0.1, aes(alpha=..index..)) +
  geom_node_label(aes(label=node_label, size=node_size),
                  label.size=0, fill="#ffffff66", segment.colour="light blue",
                  color="red", repel=TRUE) +
  coord_fixed() +
  scale_size_area(trans="sqrt") +
  labs(title="Joe Biden Twitter Plot", subtitle="Edges=volume of retweets. Screenname size=influence") +
  theme_bw() +
  theme(legend.position="none") 

### Kamala Harris ###

## search for 100 tweets by keyword in English
kh <- rtweet::search_tweets(q = "KamalaHarris", n = 1000, lang = "en", token = twitter_token)

## preview users data
users_data(kh)

# Get tweets by userid
kh_timeline = get_timelines("KamalaHarris", n = 100)

## plot time series of tweets frequency
ts_plot(kh_timeline, by = "days") + theme_bw()


# Graphing retweet connections
## Create igraph object from Twitter data using user id and mentioned id.
## ggraph draws the network graph in different layouts (12). 

filter(kh, retweet_count > 0 ) %>% 
  select(screen_name, mentions_screen_name) %>%
  unnest(mentions_screen_name) %>% 
  filter(!is.na(mentions_screen_name)) %>% 
  graph_from_data_frame() -> kh_g
V(kh_g)$node_label <- unname(ifelse(degree(kh_g)[V(kh_g)] > 20, names(V(kh_g)), "")) 
V(kh_g)$node_size <- unname(ifelse(degree(kh_g)[V(kh_g)] > 20, degree(kh_g), 0)) 

# ggraph layouts: 'star', 'circle', 'gem', 'dh', 'graphopt', 'grid', 'mds', 
# 'randomly', 'fr', 'kk', 'drl', 'lgl'
# Davidson-Harel algorithm
# Try also fr (fruchterman reingold)

ggraph(kh_g, layout = 'dh') + 
  geom_edge_arc(edge_width=0.1, aes(alpha=..index..)) +
  geom_node_label(aes(label=node_label, size=node_size),
                  label.size=0, fill="#ffffff66", segment.colour="light blue",
                  color="red", repel=TRUE) +
  coord_fixed() +
  scale_size_area(trans="sqrt") +
  labs(title="Kamala Harris Twitter Plot", subtitle="Edges=volume of retweets. Screenname size=influence") +
  theme_bw() +
  theme(legend.position="none")
