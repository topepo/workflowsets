
check_consistent_metrics <- function(x, fail = TRUE) {
   metric_info <-
      dplyr::distinct(x, .metric, wflow_id) %>%
      dplyr::mutate(has = TRUE) %>%
      tidyr::pivot_wider(names_from = ".metric", values_from = "has", values_fill = FALSE) %>%
      dplyr::select(-wflow_id) %>%
      purrr::map_dbl(~ sum(!.x))

   if (any(metric_info > 0)) {
      incp_metrics <- names(metric_info)[metric_info > 0]
      msg <- paste("Some metrics were not used in all workflows:",
                   paste(incp_metrics, collapse = ", "))

      if (fail) {
         halt(msg)
      } else {
         rlang::warn(msg)
      }
   }
   invisible(NULL)
}

check_incompete <- function(x, fail = TRUE) {
   empty_res <- purrr::map_lgl(x$result, ~ identical(.x, list()))
   failed_res <- purrr::map_lgl(x$result, ~ inherits(.x, "try-error"))

   n_empty <- sum(empty_res | failed_res)
   if (n_empty > 0) {
      msg <- paste("There were", n_empty, "workflows that had no results.")
      if (fail) {
         halt(msg)
      } else {
         rlang::warn(msg)
      }
   }
   invisible(NULL)
}

# TODO check for consistent resamples


# if global in local, overwrite or fail?

common_options <- function(model, global) {
   old_names <- names(model)
   new_names <- names(global)
   common_names <- intersect(old_names, new_names)
   if (length(common_names) > 0) {
      res <- paste0("'", common_names, "'", collapse = ", ")
   } else {
      res <- ""
   }
   res
}

check_options <- function(model, id, global, action = "fail") {
   res <- purrr::map_chr(model, common_options, global)
   flag <- nchar(res) > 0
   if (any(flag)) {
      msg <- "There are existing options that are being modified\n"
      msg <- paste0(msg, paste0("\t", id[flag], ": ", res[flag], collapse = "\n"))
      if (action == "fail") {
         halt(msg)
      }
      if (action == "warn") {
         rlang::warn(msg)
      }
   }
   invisible(NULL)
}


# in case there are no tuning parameters, we can avoid warnings

recheck_options <- function(opts, .fn) {
   if (.fn == "fit_resamples") {
      allowed <- c("object", "resamples", "metrics", "control")
      nms <- names(opts)
      disallowed <- !(nms %in% allowed)
      if (any(disallowed)) {
         opts <- opts[!disallowed]
      }
   }
   opts
}


check_fn <- function(fn, x, verbose) {
   has_tune <- nrow(tune::tune_args(x)) > 0
   if (!has_tune & fn != "fit_resamples") {
      fn <- "fit_resamples"
      if (verbose) {
         cols <- tune::get_tune_colors()
         msg <- "No tuning parameters. `fit_resamples()` will be attempted"
         message(cols$symbol$info("i"), "\t", cols$message$info(msg))
      }
   }
   fn
}

check_names <- function(x) {
   nms <- names(x)
   if (any(nms == "")) {
      bad <- which(nms == "")
      msg <- "Objects in these positions are not named:"
      msg <- paste(msg, paste0(bad, collapse = ", "))
      halt(msg)
   }
   xtab <- table(nms)
   if (any(xtab > 1)) {
      msg <- "The workflow names should be unique:"
      msg <- paste(msg, paste0("'", names(xtab)[xtab > 1], "'", collapse = ", "))
      halt(msg)
   }
   invisible(NULL)
}

check_for_workflow <- function(x) {
   no_wflow <- purrr::map_lgl(x, ~ !inherits(.x, "workflow"))
   if (any(no_wflow)) {
      bad <- names(no_wflow)[no_wflow]
      msg <- "Some objects do not have workflows:"
      msg <- paste(msg, paste0("'", bad, "'", collapse = ", "))
      msg <- paste0(msg, ". Use the control option `save_workflow` and re-run.")
      halt(msg)
   }
   invisible(NULL)
}


has_required_container_type <- function(x) {
   rlang::is_list(x)
}
has_required_container_columns <- function(x) {
   columns <- required_container_columns()
   ok <- all(columns %in% names(x))
   ok
}
required_container_columns <- function() {
   c("wflow_id", "info", "option", "result")
}


has_valid_column_info_structure <- function(x) {
   info <- x$info
   rlang::is_list(info)
}
has_valid_column_info_inner_types <- function(x) {
   info <- x$info
   is_tibble_indicator <- purrr::map_lgl(info, tibble::is_tibble)
   all(is_tibble_indicator)
}
has_valid_column_info_inner_names <- function(x) {
   columns <- required_info_inner_names()
   info <- x$info
   list_of_names <- purrr::map(info, names)
   has_names_indicator <- purrr::map_lgl(list_of_names, identical, y = columns)
   all(has_names_indicator)
}
required_info_inner_names <- function() {
   c("workflow", "preproc", "model", "comment")
}


has_valid_column_result_structure <- function(x) {
   result <- x$result
   rlang::is_list(result)
}
has_valid_column_result_inner_types <- function(x) {
   result <- x$result
   valid_indicator <- purrr::map_lgl(result, is_valid_result_inner_type)
   all(valid_indicator)
}
has_valid_column_result_fingerprints <- function(x) {
   result <- x$result

   # Drop default results
   default_indicator <- purrr::map_lgl(result, is_default_result_element)
   result <- result[!default_indicator]

   # Not sure how to fingerprint racing objects just yet. See
   # https://github.com/tidymodels/rsample/issues/212
   racing_indicator <- purrr::map_lgl(result, inherits, "tune_race")
   result <- result[!racing_indicator]

   tune_indicator <- purrr::map_lgl(result, inherits, "tune_results")
   result <- result[tune_indicator]
   if (length(result) > 0) {
      hashes <- purrr::map_chr(result, rsample::.get_fingerprint)
   } else {
      hashes <- NA_character_
   }

   # Drop NAs for results created before rsample 0.1.0, which won't have a hash
   pre_0.1.0 <- is.na(hashes)
   hashes <- hashes[!pre_0.1.0]
   result <- result[!pre_0.1.0]

   if (rlang::is_empty(hashes)) {
      # No hashes to check
      TRUE
   } else {
      # Should collapse to a single hash value if all resamples are the same
      uniques <- unique(hashes)
      length(uniques) == 1L
   }
}
is_valid_result_inner_type <- function(x) {
   if (is_default_result_element(x)) {
      # Default, before any results are filled
      return(TRUE)
   }
   is.null(x) || inherits(x, "tune_results") | inherits(x, "try-error")
}
is_default_result_element <- function(x) {
   identical(x, list())
}

has_valid_column_option_structure <- function(x) {
   option <- x$option
   rlang::is_list(option)
}
has_valid_column_option_inner_types <- function(x) {
   option <- x$option
   valid_options_indicator <- purrr::map_lgl(option, inherits, "workflow_set_options")
   all(valid_options_indicator)
}


has_valid_column_wflow_id_structure <- function(x) {
   wflow_id <- x$wflow_id
   rlang::is_character(wflow_id)
}
has_valid_column_wflow_id_strings <- function(x) {
   wflow_id <- x$wflow_id
   uniques <- unique(wflow_id)

   if (length(wflow_id) != length(uniques)) {
      return(FALSE)
   }
   if (any(is.na(wflow_id))) {
      return(FALSE)
   }
   if (any(wflow_id == "")) {
      return(FALSE)
   }

   TRUE
}

has_all_pkgs <- function(w) {
   pkgs <- generics::required_pkgs(w, infra = FALSE)
   if (length(pkgs) > 0 ) {
      is_inst <- purrr::map_lgl(pkgs, ~ rlang::is_true(requireNamespace(.x, quietly = TRUE)))
      if (!all(is_inst)) {
         cols <- tune::get_tune_colors()
         msg <- paste0("The workflow requires packages that are not installed: ",
                       paste0("'", cols$message$danger(pkgs[!is_inst]), "'", collapse = ", "),
                       ". Skipping this workflow.")
         message(
            cols$symbol$danger(cli::symbol$cross), " ",
            cols$message$warning(msg)
         )
         res <- FALSE
      } else {
         res <- TRUE
      }
   } else {
      res <- TRUE
   }
   res
}


