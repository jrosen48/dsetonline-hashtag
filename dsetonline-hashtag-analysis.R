library(tidyverse)
library(lubridate)
library(GGally)
library(igraph)
library(hrbrthemes)
library(cowplot)

df <- read_csv("tweets.csv")

df$date <- parse_date_time(df$date, "%a %b! %d! %H! %M! %S! %z! %y!")
df$date <- with_tz(df$date, "America/Los_Angeles")
df$wday <- wday(df$date, label = T)
df$hour <- hour(df$date)

p1 <- df %>% 
    count(wday) %>% 
    ggplot(aes(x = wday, y = n)) +
    geom_col() +
    theme_ipsum_rc() +
    xlab("Weekday") +
    ylab("Number of Tweets")

p2 <- df %>% 
    count(hour) %>% 
    ggplot(aes(x = hour, y = n)) +
    geom_col() +
    theme_ipsum_rc() +
    xlab("Hour") +
    ylab("Number of Tweets") +

cowplot::plot_grid(p1, p2)

ggsave("dset_time.png")

create_the_edgelist <- function(df, sender_col, receiver_col){
    
    df_ss <- filter(df, type == "ORIG" | type == "REPLY")
    
    receiver <- select_(df_ss, receiver_col)
    receiver <- collect(select_(receiver, receiver_col))[[1]]
    
    df_for_sender <- select_(df_ss, sender_col)
    df_for_sender <- collect(select_(df_for_sender, sender_col))[[1]]
    
    sender <- stringr::str_split(df_for_sender, "\\*")
    
    tmp = stack(setNames(sender, receiver))[, 2:1]
    names(tmp) <- c("receiver", "sender")
    
    tmp$sender <- tolower(tmp$sender)
    tmp$receiver <- tolower(tmp$receiver)
    
    tmp <- tmp %>% dplyr::mutate(var = sender_col)
    # tmp <- filter(tmp, !is.na(sender))
    tmp <- tbl_df(tmp)
    
    return(tmp)
}

favorites <- create_the_edgelist(df, "favNames", "screen_name")
mentions <- create_the_edgelist(df, "non_reply_mentions", "screen_name")
retweets <- create_the_edgelist(df, "rtNames", "screen_name")
replies <- create_the_edgelist(df, "reply_user_sn", "screen_name")

all_df <- bind_rows(favorites, mentions, retweets, replies)

all_df$var <- ifelse(all_df$var == "favNames", "Favorites",
                     ifelse(all_df$var == "non_reply_mentions", "Mentions",
                            ifelse(all_df$var == "rtNames", "Retweets",
                                   ifelse(all_df$var == "reply_user_sn", "Replies", NA))))

# Plot

all_df$receiver <- tolower(all_df$receiver)
all_df$sender <- tolower(all_df$sender)
all_df_ss <- select(all_df, sender, receiver)
all_df_ss <- all_df_ss[complete.cases(all_df_ss), ]
g <- igraph::graph_from_data_frame(all_df_ss, directed = T)
g <- igraph::set_edge_attr(g, "weight", value = 1)

g <- igraph::simplify(g, remove.multiple = T, remove.loops = T, edge.attr.comb = list(weight = "sum"))
E(g)$weight <- sqrt(E(g)$weight) * .85

g_p <- intergraph::asNetwork(g)

GGally::ggnet2(g,
               label = T,
               # size = "degree",
               # edge.size = "weight",
               arrow.gap = 0.02,
               arrow.size = 6,
               palette = 2,
               label.size = 3
               ) + theme(element_text )