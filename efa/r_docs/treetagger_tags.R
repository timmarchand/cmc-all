master_nest %>% #slice(13) %>% 
mutate(matrix = map(data, ~make_matrix(.x)))  %>%
mutate(cor_mat = map(.x = matrix, .f = ~.x %>% cor)) %>% 
mutate(determinant = map_dbl(.x = matrix, .f = ~.x %>% cor %>% det)) %>% 
mutate(symnum = map(.x = cor_mat, .f = ~ .x %>% symnum)) %>% 
mutate(n_factors = map(.x = matrix, .f = ~parameters::n_factors(.x) %>% tibble))


do_fa.parallel <- function(x){
fp <- fa.parallel(x,  fm = "pa", fa = "fa")
print(fp$nfacts)
} 




beta <- master_nest %>% slice(13) %>% 
  mutate(matrix = map(output_50, ~make_matrix(.x)))  %>%
  mutate(no_facts = map(.x = matrix, ~do_fa.parallel(.x)))

beta %>% pluck("no_facts")

  master_table %>% pluck("data_efa_50",6
                         )
  
  master_nest %>% 
    mutate(matrix = map(output_50, ~make_matrix(.x)))  %>%
    slice(18) %>% 
    mutate(n_factors = map(.x = matrix, .f = ~parameters::n_factors(.x) %>% tibble))

  
  tags <- tibble::tribble(
                          ~ID,    ~TAG,                  ~DESCRITPION,
                          1,    "CC",                   "Coordinating.conjunction",
                           2,   "CD",                          "Cardinal number",
                           3,   "DT",                               "Determiner",
                           4,   "EX",                        "Existential there",
                           5,   "FW",                             "Foreign word",
                           6,   "IN", "Preposition or subordinating conjunction",
                           7,   "JJ",                                "Adjective",
                           8,  "JJR",                   "Adjective, comparative",
                           9,  "JJS",                   "Adjective, superlative",
                          10,   "LS",                         "List item marker",
                          11,   "MD",                                    "Modal",
                          12,   "NN",                   "Noun, singular or mass",
                          13,  "NNS",                             "Noun, plural",
                          14,  "NNP",                    "Proper noun, singular",
                          15, "NNPS",                      "Proper noun, plural",
                          16,  "PDT",                            "Predeterminer",
                          17,  "POS",                        "Possessive ending",
                          18,  "PRP",                         "Personal pronoun",
                          19, "PRP$",                       "Possessive pronoun",
                          20,   "RB",                                   "Adverb",
                          21,  "RBR",                      "Adverb, comparative",
                          22,  "RBS",                      "Adverb, superlative",
                          23,   "RP",                                 "Particle",
                          24,  "SYM",                                   "Symbol",
                          25,   "TO",                                       "to",
                          26,   "UH",                             "Interjection",
                          27,   "VB",                          "Verb, base form",
                          28,  "VBD",                         "Verb, past tense",
                          29,  "VBG",       "Verb, gerund or present participle",
                          30,  "VBN",                    "Verb, past participle",
                          31,  "VBP",    "Verb, non-3rd person singular present",
                          32,  "VBZ",        "Verb, 3rd person singular present",
                          33,  "WDT",                            "Wh-determiner",
                          34,   "WP",                               "Wh-pronoun",
                          35,  "WP$",                    "Possessive wh-pronoun",
                          36,  "WRB",                                "Wh-adverb"
                         )
  
  tags <- tags %>% mutate(TAG = tolower(TAG))
    
  