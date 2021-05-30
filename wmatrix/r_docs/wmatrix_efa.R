beta %>% pluck("for_efa",1)

make_matrix <- function(x){
  x %>% select(-c(1,3))  %>% column_to_rownames(var = "id") %>% select(where(~ any(. != 0)))
}

library(ggeasy)

master_nest %>% pluck("data", 1)


beta <- beta  %>% #slice(10) %>% 
  mutate(matrix = map(for_efa, ~make_matrix(.x))) %>%
  mutate(matrix = set_names(matrix, setting))%>% 
  mutate(cor_mat = map(.x = matrix, .f = ~.x %>% cor)) %>% 
  mutate(determinant = map_dbl(.x = matrix, .f = ~.x %>% cor %>% det)) %>% 
  mutate(symnum = map(.x = cor_mat, .f = ~ .x %>% symnum)) %>% 
  mutate(n_factors = map(.x = matrix, .f = ~parameters::n_factors(.x, rotation = "oblimin", algorithm = "pa" , , package = "nFactors" ))) %>% 
  mutate(n_factors_df = map(.x = n_factors, .f = ~tibble(.x))) %>% 
  mutate(cor_mat = map(.x = matrix, .f = ~.x %>% cor)) %>% 
  mutate(determinant = map_dbl(.x = matrix, .f = ~.x %>% cor %>% det)) %>% 
  mutate(symnum = map(.x = cor_mat, .f = ~ .x %>% symnum)) %>% 
  mutate(n_factors = map(.x = matrix, .f = ~parameters::n_factors(.x, rotation = "oblimin", algorithm = "pa" , , package = "nFactors" ))) %>% 
  mutate(n_factors_df = map(.x = n_factors, .f = ~tibble(.x)))

p1 <- 
  beta %>%   mutate(determinant = log10(determinant)) %>% 
  mutate(posgram = str_replace(setting,".+\\d+_(\\w+)_\\d+","\\1")) %>%
  ggplot(aes(determinant, fill = posgram)) + geom_boxplot() + scale_x_continuous(labels = scales::scientific) + geom_vline(aes(xintercept = log10(0.00001)), colour = "red", linetype = 2) + theme_minimal() +  easy_remove_y_axis() + easy_move_legend("top")




beta %>% 
  mutate(posgram = str_replace(setting,".+\\d+_(\\w+)_\\d+","\\1")) %>% 
mutate(top = str_extract(setting,"top_\\d+") %>% parse_number) %>% 
  mutate(cut = str_extract(setting,"\\d+$")) %>% 
  select(cut)

beta %>% separate(setting,into = c("junk","top","posgram","cut"),sep = "_") %>% 
  
  
  beta %>% select(setting)
