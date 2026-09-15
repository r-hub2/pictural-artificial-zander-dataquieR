patch_legacy_fortests_repeated_measurements <- function() {
  # Temporary compatibility patch for the external fortests workbooks. Keep the
  # public fixtures stable until old package releases no longer rely on them.
  patch_cache <- function(names, x) {
    if (!is.data.frame(x)) {
      return(invisible(FALSE))
    }
    for (name in names) {
      suppressMessages(prep_remove_from_cache(name))
      suppressMessages(prep_add_data_frames(
        data_frame_list = stats::setNames(list(x), name)
      ))
    }
    invisible(TRUE)
  }

  item_level_names <- c(
    "item_level",
    "meta_data_v2.xlsx|item_level",
    "meta_data_v2|item_level",
    "ship_meta_v2.xlsx|item_level",
    "ship_meta_v2|item_level"
  )
  if (exists("item_level", envir = dataquieR:::.dataframe_environment())) {
    item_level <- prep_get_data_frame("item_level")
    item_level[["REPEATED_MEASURES_GOLDSTANDARD"]] <- NULL
    patch_cache(item_level_names, item_level)
  }

  cross_item_level_names <- c(
    "cross-item_level",
    "meta_data_v2.xlsx|cross-item_level",
    "meta_data_v2|cross-item_level",
    "ship_meta_v2.xlsx|cross-item_level",
    "ship_meta_v2|cross-item_level"
  )
  if (exists("cross-item_level", envir = dataquieR:::.dataframe_environment())) { # nolint: line_length_linter.
    cross_item_level <- prep_get_data_frame("cross-item_level")
    names(cross_item_level)[names(cross_item_level) == "REL_VAL"] <-
      "REPEATED_MEASURES_METRIC"
    names(cross_item_level)[names(cross_item_level) == "GOLDSTANDARD"] <-
      "REPEATED_MEASURES_REFERENCE"
    patch_cache(cross_item_level_names, cross_item_level)
  }

  invisible(TRUE)
}
