pacman::p_load(ggeasy,parameters,scales,nFactors,gridExtra,magrittr,tidyverse)

C7_tops %>% pluck("for_efa",1)
## from the C7_tops for_efa df, we don't require the corpus or type columns when making the matric
make_matrix <- function(x){
  x %>% select(-c(1,3))  %>% column_to_rownames(var = "id") %>% select(where(~ any(. != 0)))
}

beta <- C7_tops

beta %>% pluck("for_efa",1) %>% count(corpus)

beta <- beta  %>% #slice(10) %>% 
  mutate(matrix = map(for_efa, ~make_matrix(.x))) %>%
  mutate(matrix = set_names(matrix, setting))%>% 
  mutate(cor_mat = map(.x = matrix, .f = ~.x %>% cor)) %>% 
  mutate(determinant = map_dbl(.x = matrix, .f = ~.x %>% cor %>% det)) %>% 
  mutate(symnum = map(.x = cor_mat, .f = ~ .x %>% symnum)) %>% 
  mutate(n_factors = map(.x = matrix, .f = ~parameters::n_factors(.x, rotation = "oblimin", algorithm = "pa" , , package = "nFactors" ))) %>% 
  mutate(n_factors_df = map(.x = n_factors, .f = ~tibble(.x)))  
 
## separate the setting column into separate columns
  beta %<>% separate(setting,into = c("top","posgram","cut"),sep = "_",remove = FALSE) 
(aes(x=reorder(carrier,speed,na.rm = TRUE)
## plotting
  p1 <- 
  beta %>%   mutate(determinant = log10(determinant)) %>% 
   mutate(posgram = fct_reorder(posgram, determinant)) %>% 
  ggplot(aes(x=determinant, fill = posgram)) + geom_boxplot() + 
  scale_y_continuous(labels = scales::scientific) + 
  geom_vline(aes(xintercept = log10(0.00001)), colour = "red", linetype = 2) +
  theme_minimal() +  
  easy_remove_y_axis() + 
  easy_move_legend("top") +
  easy_remove_legend_title()

p2 <- beta %>% 
  mutate(determinant = log10(determinant)) %>% 
   mutate(top = fct_reorder(top, determinant)) %>% 
  ggplot(aes(determinant, fill = top)) + geom_boxplot() + scale_x_continuous(labels = scales::scientific) + geom_vline(aes(xintercept = log10(0.00001)), colour = "red", linetype = 2) + theme_minimal() + easy_remove_y_axis() + easy_move_legend("top")

p3 <- beta %>%
  mutate(determinant = log10(determinant)) %>% 
   mutate(cut = fct_reorder(cut, determinant)) %>% 
  ggplot(aes(determinant, fill = cut)) + geom_boxplot() + scale_x_continuous(labels = scales::scientific) + geom_vline(aes(xintercept = log10(0.00001)), colour = "red", linetype = 2) + theme_minimal() + easy_remove_y_axis() + easy_move_legend("top")


gridExtra::grid.arrange(p1,p2,p3, nrow = 3)

beta %>% filter(posgram == "bigram",
                top == "top20",
                cut == "500") %>% pluck("for_efa",1)



# factor analysis ---------------------------------------------------------


master_matrix <- beta %>% filter(posgram == "bigram",
                top == "top50",
                cut == "500") %>% pluck("matrix",1)


tbl <-  nest(master_matrix,data = everything()) %>% 
  # duplicate data for maximum number of factors 
  slice(rep(1, each=4)) %>% 
  # add factors column
  rowid_to_column(var = "factors") %>% 
  # create list column with fa results for each factor
  mutate(model = map2(.x = .$factors, .y = .$data, 
                      .f = ~fa(.y, nfactors = .x, 
                               rotate = "oblimin", fm = "ml"))) %>% 
  # add BIC information for each model
  mutate(BIC = map_dbl(.x = model, .f = ~get_BIC(.x))) %>% 
  # add loadings table
  mutate(loadings = map(.x = model, .f = ~get_loadings(.x, cut = 0))) %>% 
  # add factor correlations table
  mutate(fa_corrs = map(.x = model, .f = ~get_fa_cors(.x))) %>% 
  # get cfi scores
  mutate(cfi = map_dbl(.x = model, .f = ~get_cfi(.x))) #%>% 
  mutate(RMSEA = map_dbl(.x = model, .f = ~pluck(1,"RMSEA")))
 
tbl %>% pluck("model",4)
