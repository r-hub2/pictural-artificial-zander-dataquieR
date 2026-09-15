with_interactive_dataquieR_wrapper <- function(code) {
  withr::with_options(
    list(
      dataquieR.test_decorator = TRUE,
      dataquieR.test_interactive_wrapper = TRUE
    ),
    code = code
  )
}
