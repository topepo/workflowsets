library(tidymodels)
library(workflowsets)

# ------------------------------------------------------------------------------

data(two_class_dat, package = "modeldata")

set.seed(1)
folds <- vfold_cv(two_class_dat, v = 5)

# ------------------------------------------------------------------------------

decision_tree_rpart_spec <-
  decision_tree(min_n = tune(), cost_complexity = tune()) %>%
  set_engine('rpart') %>%
  set_mode('classification')

logistic_reg_glm_spec <-
  logistic_reg() %>%
  set_engine('glm')

mars_earth_spec <-
  mars(prod_degree = tune()) %>%
  set_engine('earth') %>%
  set_mode('classification')

# ------------------------------------------------------------------------------

yj_recipe <-
   recipe(Class ~ ., data = two_class_dat) %>%
   step_YeoJohnson(A, B)

# ------------------------------------------------------------------------------

two_class_set <-
   workflow_set(
      preproc = list(none = Class ~ A + B, yj_trans = yj_recipe),
      models = list(cart = decision_tree_rpart_spec, glm = logistic_reg_glm_spec,
                    mars = mars_earth_spec)
   )

# ------------------------------------------------------------------------------

two_class_res <-
  two_class_set %>%
  workflow_map(
    resamples = folds,
    grid = 10,
    seed = 2,
    verbose = TRUE,
    control = control_grid(save_workflow = TRUE)
  )

# ------------------------------------------------------------------------------

save(two_class_set, two_class_res, file = "data/two_class_set.rda",
     compress = "xz", version = 2)

