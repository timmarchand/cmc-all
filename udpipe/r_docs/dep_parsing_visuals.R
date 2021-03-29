### dependency parsing visualistaion


## libraries ----
suppressMessages({
  library(igraph)
  library(ggraph)
  library(ggplot2)
  library(here)
  library(tidyverse)
  library(udpipe)
  library(stringr)
})


## functions -----


plot_annotation <- function(x, size = 3){
  stopifnot(is.data.frame(x) & all(c("sentence_id", "token_id", "head_token_id", "dep_rel",
                                     "token_id", "token", "lemma", "upos", "xpos", "feats") %in% colnames(x)))
  x <- x[!is.na(x$head_token_id), ]
  x <- x[x$sentence_id %in% min(x$sentence_id), ]
  edges <- x[x$head_token_id != 0, c("token_id", "head_token_id", "dep_rel")]
  edges$label <- edges$dep_rel
  g <- graph_from_data_frame(edges,
                             vertices = x[, c("token_id", "token", "lemma", "upos", "xpos", "feats")],
                             directed = TRUE)
  ggraph(g, layout = "dendogram") +
    geom_edge_arc(ggplot2::aes(label = dep_rel, vjust = -0.20),
                  arrow = grid::arrow(length = unit(4, 'mm'), ends = "last", type = "closed"),
                  end_cap = ggraph::label_rect("wordswordswords"),
                  label_colour = "red", check_overlap = TRUE, label_size = size) +
    geom_node_label(ggplot2::aes(label = token), col = "darkgreen", size = size, fontface = "bold") +
    geom_node_text(ggplot2::aes(label = upos), nudge_y = -0.35, size = size) +
    theme_graph(base_family = "Arial Narrow") +
    labs(title = "udpipe output", subtitle = "tokenisation, parts of speech tagging & dependency relations")
}


## load an example

## get the datafram
df <- readRDS(here("BNC_clean","data","bnc_df.rds"))
## load the model
mdl <- udpipe_load_model(file = here("BNC_clean", "data", "english-ewt-ud-2.5-191206.udpipe"))

## name of columns to be converted to integer
ints <- c("paragraph_id", "sentence_id", "start", "end", "term_id", "token_id", "head_token_id")


ud_mdl <- df %>% filter(id == "news_K5E_4460") %>% 
  mutate(model = map(text, ~udpipe(.x, object = mdl, parrallel.cores = 1L)))  %>%
  select(id,model) %>% unnest(model) #%>%  
  mutate(across(all_of(ints), as.integer))

ud_mdl %>% filter(doc_id == "doc1" & sentence_id == "1") %>% select(-id)  %>% 
  plot_annotation()

class(ud_mdl
      )


x <- udpipe("His speech about marshmallows in New York is utter bullshit", "english")

plot_annotation(x)
