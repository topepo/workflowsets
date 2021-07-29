test_that("autoplot with error bars", {
  p_1 <- autoplot(two_class_res, metric = "roc_auc")
  expect_s3_class(p_1, "ggplot")
  expect_equal(
     names(p_1$data),
     c("wflow_id", ".config", ".metric", "mean", "std_err", "n",
       "preprocessor", "model", "rank")
  )
  expect_equal(rlang::get_expr(p_1$mapping$x), expr(rank))
  expect_equal(rlang::get_expr(p_1$mapping$y), expr(mean))
  expect_equal(rlang::get_expr(p_1$mapping$colour), expr(model))
  expect_equal(as.list(p_1$facet)$params, list())
  expect_equal(
     rlang::get_expr(as.list(p_1$layers[[2]])$mapping$ymin),
     expr(mean - std_errs * std_err)
  )
  expect_equal(
     rlang::get_expr(as.list(p_1$layers[[2]])$mapping$ymax),
     expr(mean + std_errs * std_err)
  )
  expect_equal(as.character(p_1$labels$y), "roc_auc")
  expect_equal(as.character(p_1$labels$x), "Workflow Rank")
})

test_that("autoplot with without error bars", {
   p_2 <- autoplot(chi_features_res)
   expect_s3_class(p_2, "ggplot")
   expect_equal(
      names(p_2$data),
      c("wflow_id", ".config", ".metric", "mean", "std_err", "n",
        "preprocessor", "model", "rank")
   )
   expect_equal(rlang::get_expr(p_2$mapping$x), expr(rank))
   expect_equal(rlang::get_expr(p_2$mapping$y), expr(mean))
   expect_equal(rlang::get_expr(p_2$mapping$colour), expr(model))
   expect_equal(length(p_2$layers), 1)
   expect_equal(names(as.list(p_2$facet)$params$facet), ".metric")
   expect_equal(p_2$labels$y, "Metric")
   expect_equal(p_2$labels$x, "Workflow Rank")
})

test_that("autoplot for specific workflow result", {
   p_3 <- autoplot(chi_features_res, id = "plus_pca_lm")
   p_4 <- autoplot(
      extract_workflow_set_result(
         chi_features_res, id = "plus_pca_lm"
      )
   )
   expect_equal(p_3$data, p_4$data)
   expect_equal(p_3$labels, p_4$labels)
   expect_equal(
      purrr::map(as.list(p_3$mapping), rlang::get_expr),
      purrr::map(as.list(p_4$mapping), rlang::get_expr)
   )

})

test_that("automatic selection of rank metric", {
   expect_equal(
      workflowsets:::pick_metric(two_class_res, NULL, NULL),
      list(metric = "roc_auc", direction = "maximize")
   )
   expect_equal(
      workflowsets:::pick_metric(two_class_res, NULL, "accuracy"),
      list(metric = "accuracy", direction = "maximize")
   )
   expect_equal(
      workflowsets:::pick_metric(two_class_res, "accuracy"),
      list(metric = "accuracy", direction = "maximize")
   )
   expect_equal(
      workflowsets:::pick_metric(two_class_res, "roc_auc"),
      list(metric = "roc_auc", direction = "maximize")
   )
   expect_error(
      workflowsets:::pick_metric(two_class_res, "roc_auc", "accuracy"),
      "was not in the results"
   )
})

