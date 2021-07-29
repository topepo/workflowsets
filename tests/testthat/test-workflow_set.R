library(parsnip)
library(rsample)
library(rlang)

lr_spec <- linear_reg() %>% set_engine("lm")
knn_spec <- nearest_neighbor() %>% set_engine("kknn") %>% set_mode("regression")

# ------------------------------------------------------------------------------

test_that("creating workflow sets", {
   expect_error({
      set.seed(1)
      car_set_1 <-
         workflow_set(
            list(reg = mpg ~ ., nonlin = mpg ~ wt + 1/sqrt(disp)),
            list(lm = lr_spec)
         ) %>%
         workflow_map("fit_resamples", resamples = vfold_cv(mtcars, v = 3),
                      control = tune::control_resamples(save_pred = TRUE, save_workflow = TRUE))
   },
   regex = NA
   )

   expect_s3_class(car_set_1, c("workflow_set", "tbl_df", "tbl", "data.frame"))
   expect_equal(
      names(car_set_1),
      c("wflow_id", "info", "option", "result")
   )

   expect_true(
      all(purrr::map_lgl(car_set_1$wflow_id, is.character))
   )
   expect_true(
      all(purrr::map_lgl(car_set_1$info, tibble::is_tibble))
   )

   expect_true(
      all(purrr::map_lgl(car_set_1$option, is.list))
   )
   expect_true(
      all(purrr::map_lgl(car_set_1$result, is.list))
   )

   expect_true(
      all(purrr::map_lgl(car_set_1$info, ~ inherits(.x$workflow[[1]], "workflow")))
   )
   expect_true(
      all(purrr::map_lgl(car_set_1$option, ~ inherits(.x, "list")))
   )
   expect_true(
      all(purrr::map_lgl(car_set_1$result, ~ inherits(.x, "resample_results")))
   )
   expect_true(
      all(purrr::map_lgl(car_set_1$result, ~ any(names(.x) == ".predictions")))
   )

   # ---------------------------------------------------------------------------

   model_list <- car_set_1$result
   names(model_list) <- car_set_1$wflow_id

   wflow_list <- purrr::map(car_set_1$result, extract_workflow)
   names(wflow_list) <- car_set_1$wflow_id

   mixed_list <- model_list
   mixed_list[[2]] <- wflow_list[[2]]

   expect_error(
      car_set_2 <- as_workflow_set(!!!model_list),
      regex = NA
   )

   expect_true(
      all(purrr::map_lgl(car_set_2$wflow_id, is.character))
   )
   expect_true(
      all(purrr::map_lgl(car_set_2$info, tibble::is_tibble))
   )

   expect_true(
      all(purrr::map_lgl(car_set_2$option, is.list))
   )
   expect_true(
      all(purrr::map_lgl(car_set_2$result, is.list))
   )

   expect_true(
      all(purrr::map_lgl(car_set_2$info, ~ inherits(.x$workflow[[1]], "workflow")))
   )
   expect_true(
      all(purrr::map_lgl(car_set_2$option, ~ inherits(.x, "list")))
   )
   expect_true(
      all(purrr::map_lgl(car_set_2$result, ~ inherits(.x, "resample_results")))
   )
   expect_true(
      all(purrr::map_lgl(car_set_2$result, ~ any(names(.x) == ".predictions")))
   )

   # ------------------------------------------------------------------------------
   # workflows as inputs

   expect_error(
      car_set_3 <- as_workflow_set(!!!wflow_list),
      regex = NA
   )

   expect_true(
      all(purrr::map_lgl(car_set_3$wflow_id, is.character))
   )
   expect_true(
      all(purrr::map_lgl(car_set_3$info, tibble::is_tibble))
   )


   # ------------------------------------------------------------------------------
   # mixed inputs
})

test_that("correct object type and resamples", {
   pp <- list(
      cyl = mpg ~ disp + hp + drat + wt + qsec + vs + am + gear + carb,
      disp = mpg ~ cyl + hp + drat + wt + qsec + vs + am + gear + carb,
      # hp = mpg ~ cyl + disp + drat + wt + qsec + vs + am + gear + carb,
      # drat = mpg ~ cyl + disp + hp + wt + qsec + vs + am + gear + carb,
      # wt = mpg ~ cyl + disp + hp + drat + qsec + vs + am + gear + carb,
      # qsec = mpg ~ cyl + disp + hp + drat + wt + vs + am + gear + carb,
      # vs = mpg ~ cyl + disp + hp + drat + wt + qsec + am + gear + carb,
      # am = mpg ~ cyl + disp + hp + drat + wt + qsec + vs + gear + carb,
      # gear = mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am + carb,
      carb = mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear
   )

   set_1 <- workflow_set(pp, list(lm = lr_spec))

   # same resamples since the seed is set
   expect_error(
      res_1 <- workflow_map(set_1, "fit_resamples", resamples = bootstraps(mtcars, 3)),
      regex = NA
   )
   res_1$result[[1]] <- lm(pp[[1]], data = mtcars)
   expect_identical(
      workflowsets:::has_valid_column_result_inner_types(res_1),
      FALSE
   )

   res_2 <- set_1
   res_2$result <-
      map(res_2$wflow_id, ~ extract_workflow(res_2, id = .x)) %>%
      purrr::map(~ fit_resamples(.x, resamples = bootstraps(mtcars, 3)))
   expect_identical(
      workflowsets:::has_valid_column_result_inner_types(res_2),
      TRUE
   )
   expect_identical(
      workflowsets:::has_valid_column_result_fingerprints(res_2),
      FALSE
   )
})



# ------------------------------------------------------------------------------

test_that("crossing", {
   expect_equal(
      nrow(
         workflow_set(
            list(reg = mpg ~ ., nonlin = mpg ~ wt + 1/sqrt(disp)),
            list(lm = lr_spec, knn = knn_spec), cross = FALSE
         )
      ),
      2
   )
   expect_equal(
      nrow(
         workflow_set(
            list(reg = mpg ~ ., nonlin = mpg ~ wt + 1/sqrt(disp)),
            list(lm = lr_spec, knn = knn_spec), cross = TRUE
         )
      ),
      4
   )
   expect_equal(
      nrow(
         workflow_set(
            list(reg = mpg ~ ., nonlin = mpg ~ wt + 1/sqrt(disp)),
            list(lm = lr_spec),
         )
      ),
      2
   )
   expect_equal(
      nrow(
         workflow_set(
            list(reg = mpg ~ .),
            list(lm = lr_spec, knn = knn_spec)
         )
      ),
      2
   )
   expect_error(
      nrow(
         workflow_set(
            list(reg = mpg ~ ., nonlin = mpg ~ wt + 1/sqrt(disp), two = mpg ~ wt + disp),
            list(lm = lr_spec, knn = knn_spec), cross = FALSE
         )
      ),
      "The lengths of 'preproc' and 'models' are different"
   )
})


# ------------------------------------------------------------------------------

test_that("checking resamples", {
   library(workflows)
   ctrl <- control_resamples(save_workflow = TRUE)
   set.seed(1)
   cv_1 <- vfold_cv(mtcars, v = 5)
   f_1 <- lr_spec %>% fit_resamples(mpg ~ wt, resamples = cv_1, control = ctrl)
   set.seed(2)
   cv_2 <- vfold_cv(mtcars, v = 5)
   f_2 <- lr_spec %>% fit_resamples(mpg ~ disp, resamples = cv_2, control = ctrl)
   expect_error(
      as_workflow_set(wt = f_1, disp = f_2),
      "Different resamples were used in the workflow 'result's"
   )

   # Emulate old rset objects
   attr(cv_2, "fingerprint") <- NULL
   f_3 <- lr_spec %>% fit_resamples(mpg ~ disp, resamples = cv_2, control = ctrl)
   expect_error(
      as_workflow_set(wt = f_1, disp = f_3),
      regexp = NA
   )
})

# ------------------------------------------------------------------------------

test_that("constructor", {

   set.seed(1)
   car_set_1 <-
      workflow_set(
         list(reg = mpg ~ ., nonlin = mpg ~ wt + 1/sqrt(disp)),
         list(lm = lr_spec)
      ) %>%
      workflow_map("fit_resamples", resamples = vfold_cv(mtcars, v = 3),
                   control = tune::control_resamples(save_pred = TRUE, save_workflow = TRUE))

   expect_error(
      workflowsets:::new_workflow_set(car_set_1 %>% dplyr::select(-info)),
      "The object should have columns"
   )

   expect_error(
      workflowsets:::new_workflow_set(car_set_1 %>% mutate(info = "a")),
      "The 'info' column should be a list."
   )
   expect_error(
      workflowsets:::new_workflow_set(car_set_1 %>% mutate(result = "a")),
      "The 'result' column should be a list."
   )
   expect_error(
      workflowsets:::new_workflow_set(car_set_1 %>% mutate(option = "a")),
      "The 'option' column should be a list."
   )
   expect_error(
      workflowsets:::new_workflow_set(car_set_1 %>% mutate(wflow_id = 1)),
      "The 'wflow_id' column should be character."
   )
   expect_error(
      workflowsets:::new_workflow_set(car_set_1 %>% mutate(wflow_id = "a")),
      "The 'wflow_id' column should contain unique, non-missing character strings"
   )

})

# ------------------------------------------------------------------------------

test_that("pillar formatting", {
   expect_snapshot_output(print(chi_features_set))
   expect_snapshot_output(print(chi_features_res))
})
