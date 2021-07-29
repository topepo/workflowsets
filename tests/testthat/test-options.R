
# ------------------------------------------------------------------------------

test_that("option management", {
   expect_error(
      set_1 <- two_class_set %>% option_add(a = 1),
      regex = NA
   )
   for (i in 1:nrow(set_1)) {
      expect_equal(unclass(set_1$option[[i]]), list(a = 1))
   }
   expect_error(
      set_2 <- two_class_set %>% option_remove(a),
      regex = NA
   )
   for (i in 1:nrow(set_2)) {
      expect_equal(unclass(set_2$option[[i]]), list())
   }
   expect_error(
      set_3 <- two_class_set %>% option_add(a = 1, id = "none_cart"),
      regex = NA
   )
   expect_equal(unclass(set_3$option[[1]]), list(a = 1))
   for (i in 2:nrow(set_3)) {
      expect_equal(unclass(set_3$option[[i]]), list())
   }
   expect_error(
      set_4 <- two_class_set %>% option_add_parameters(),
      regex = NA
   )
   for (i in which(!grepl("glm", set_4$wflow_id))) {
      expect_true(all(names(set_4$option[[i]]) == "param_info"))
      expect_true(inherits(set_4$option[[i]]$param_info, "parameters"))
   }
   for (i in which(grepl("glm", set_4$wflow_id))) {
      expect_equal(unclass(set_4$option[[i]]), list())
   }
   expect_error(
      set_5 <- two_class_set %>% option_add_parameters(id = "none_cart"),
      regex = NA
   )
   expect_true(all(names(set_5$option[[1]]) == "param_info"))
   expect_true(inherits(set_5$option[[1]]$param_info, "parameters"))
   for (i in 2:nrow(set_5)) {
      expect_equal(unclass(set_5$option[[i]]), list())
   }
})



test_that("option printing", {

   expect_output(
      print(two_class_res$option[[1]]),
      "a list of options with names:  'resamples', 'grid'"
   )
   expect_equal(
      tibble::type_sum(two_class_res$option[[1]]),
      "opts[3]"
   )
})
