## binomial regression with categories -----

set.seed(4)

dat <- thread_stance_count %>% 
  distinct(corpus,thread,category,category_per_10k) %>% na.omit %>% 
  mutate(corpus = factor(corpus))


dat %<>% pivot_wider(names_from = category, values_from = category_per_10k,values_fill = 0) %>% janitor::clean_names()


dat %<>% mutate(sample = sample(size = nrow(dat),replace = FALSE,1:nrow(dat)),
               .before = corpus) %>% arrange(sample)

dat

train <- dat %>% filter(sample < .75*nrow(.)) %>% select(-c(1,3))
test <- dat %>% anti_join(train)

mdl1 <- glm(corpus ~ ., data = train, family = "binomial")

drop1(mdl1, test = "LRT")

summary(mdl2 <- update(mdl1, ~ . - stance_adjective_that_clause))

drop1(mdl2, test = "LRT")



train %<>% mutate(num_preds = predict(mdl2, type = "response")) %>% 
  mutate(cat_preds = factor(ifelse(num_preds > 0.5, "JOC", "HOC")))

test_preds <- test %>% select(-c(sample,corpus,thread,stance_adjective_that_clause))

test %<>% mutate(num_preds = predict(mdl2, test_preds ,type = "response")) %>% 
  mutate(cat_preds = factor(ifelse(num_preds > 0.5, "JOC", "HOC")))

caret::confusionMatrix(table(test$cat_preds,
                             test$corpus))

two_class_cm <- caret::confusionMatrix(
  test$cat_preds,
  test$corpus
)

test %>% names

plot(effects::allEffects(mdl2), ylim = c(0,1), type = "response", grid = FALSE)


mat <- matrix(c(661,36,246,207), nrow=2)




plot(effects::Effect(focal.predictors = "stance_verb_that_clause", type ="response",  mdl2, grid = TRUE))



test %>% filter(cat_preds == "HOC", corpus == "JOC")

# logistic model on pattern -----------------------------------------------

dat <- thread_stance_count %>% 
  distinct(corpus,thread,pattern,pattern_per_10k) %>% na.omit %>% 
  mutate(corpus = factor(corpus))

dat %<>% filter(pattern %in% model_cols) %>% 
  pivot_wider(names_from = pattern, values_from = pattern_per_10k,values_fill = 0) %>% janitor::clean_names()

dat %<>% mutate(sample = sample(size = nrow(dat),replace = FALSE,1:nrow(dat)),
                .before = corpus) %>% arrange(sample)

dat %>% count(pattern)

train <- dat %>% filter(sample < .75*nrow(.)) %>% select(-c(1,3))
test <- dat %>% anti_join(train)

mdl1 <- glm(corpus ~ ., data = train, family = "binomial")

summary(mdl1)
drop1(mdl1, test = "LRT")

summary(mdl2 <- update(mdl1, ~ . - astonishingly))
drop1(mdl2, test = "LRT")

summary(mdl3 <- update(mdl2, ~ . - remind_to))
drop1(mdl3, test = "LRT")

summary(mdl4 <- update(mdl3, ~ . - conceivable_that))
drop1(mdl4, test = "LRT")

summary(mdl5 <- update(mdl4, ~ . - would))
drop1(mdl5, test = "LRT")

summary(mdl6 <- update(mdl5, ~ . - odd_that))
drop1(mdl6, test = "LRT")

summary(mdl7 <- update(mdl6, ~ . - predict_that))
drop1(mdl7, test = "LRT")

summary(mdl8 <- update(mdl7, ~ . - evidently))
drop1(mdl8, test = "LRT")

summary(mdl9 <- update(mdl8, ~ . - typically))
drop1(mdl9, test = "LRT")

test_preds <-  test %>% select(c(think_that,agree_that,realise_that,recognise_that,claim_to))

test %<>% mutate(num_preds = predict(mdl9, test_preds ,type = "response")) %>% 
  mutate(cat_preds = factor(ifelse(num_preds > 0.5, "JOC", "HOC")))

caret::confusionMatrix(table(test$cat_preds,
                             test$corpus))





