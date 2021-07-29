#' Convert existing objects to a workflow set
#'
#' Use existing objects to create a workflow set. A list of objects that are
#' either simple workflows or objects that have class `"tune_results` is
#' converted into a workflow set.
#' @param ... One or more named objects. Names should be unique and the
#' objects should have at least one of the following classes: `workflow`,
#' `iteration_results`, `tune_results`, `resample_results`, or `tune_race`. Each
#' `tune_results` element should also contain the original workflow
#' (accomplished using the `save_workflow` option in the control function).
#' @return A workflow set. Note that the `option` column will not reflect the
#' options that were used to create each object.
#' @examples
#'
#' # ------------------------------------------------------------------------------
#' # Existing results
#'
#' # Use the already worked example to show how to add tuned
#' # objects to a workflow set
#'
#' results <-  two_class_res %>% purrr::pluck("result")
#' names(results) <- two_class_res$wflow_id
#'
#' # These are all objects that have been resampled or tuned:
#' purrr::map_chr(results, ~ class(.x)[1])
#'
#' # Use rlang's !!! operator to splice in the elements of the list
#' new_set <- as_workflow_set(!!!results)
#'
#' # ------------------------------------------------------------------------------
#' # Make a set from unfit workflows
#'
#' library(parsnip)
#' library(workflows)
#'
#' lr_spec <- logistic_reg()
#'
#' main_effects <-
#'    workflow() %>%
#'    add_model(lr_spec) %>%
#'    add_formula(Class ~ .)
#'
#' interactions <-
#'    workflow() %>%
#'    add_model(lr_spec) %>%
#'    add_formula(Class ~ (.)^2)
#'
#' two_workflows <- list(main = main_effects, int = interactions)
#' as_workflow_set(!!!two_workflows)
#'
#' @export
as_workflow_set <- function(...) {
   object <- rlang::list2(...)

   is_workflow <- purrr::map_lgl(object, ~ inherits(.x, "workflow"))
   tune_index <- which(!is_workflow)

   if (all(!is_workflow)) {
      wflows <- purrr::map(object, tune::.get_tune_workflow)
   } else if (any(!is_workflow)) {
      wflows <- object
      wflows[tune_index] <- purrr::map(object[tune_index], tune::.get_tune_workflow)
   } else {
      wflows <- object
   }

   check_names(wflows)
   check_for_workflow(wflows[])

   res <- tibble::tibble(wflow_id = names(wflows))
   res <-
      res %>%
      dplyr::mutate(
         workflow  = unname(wflows),
         info = purrr::map(workflow, get_info),
         option    = purrr::map(1:nrow(res), ~ new_workflow_set_options())
      )
   res$result <- vector(mode = "list", length = nrow(res))
   if (length(tune_index) > 1) {
      res$result[tune_index] <- unname(object[tune_index])
   }

   res %>%
      dplyr::select(wflow_id, info, option, result) %>%
      new_workflow_set()
}
