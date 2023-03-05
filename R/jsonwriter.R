require("jsonlite")

call_to_string <- function(call) {
    paste(
        trimws(
            deparse(call)
        ),
        collapse = " "
    )
}

#' Write regression results to a JSON file.
#'
#' @param model Object that contains the regression results.  Currently
#'              only `fixest` is supported.
#' @param output_filename Filename to write the JSON output to.
#' @param ... Additional arguments passed on to the method.
#'
#' @return Nothing.
#' @export
write_json <- function(model, output_filename, ...) {
    UseMethod("write_json")
}

#' @export
write_json.default <- function(model, output_filename, ...) NULL

#' Write a `fixest` object to a JSON file.
#'
#' @param model A `fixest` object that contains the regression results.
#' @param output_filename Filename to write the JSON output to.
#' @param verbose Flag to indicate whether to write console messages.
#' @param extra_fields A `list` object with extra information to add to
#'                     the output.
#' @param ... Additional arguments.  Ignored.
#' @export
write_json.fixest <- function(model,
                              output_filename,
                              verbose = FALSE,
                              extra_fields = NULL,
                              ...) {
    output_object <- list(
        call      = call_to_string(model$call),
        r_squared = 1 - model$ssr / model$ssr_null,
        coef      = sapply(
            names(model$coefficients),
            function(coef_name) {
                p_value <- model$coeftable[coef_name, "Pr(>|t|)"]

                list(
                    est = model$coefficients[coef_name],
                    se = model$se[coef_name],
                    t = model$coeftable[coef_name, "t value"],
                    p = p_value,
                    stars = dplyr::case_when(
                        p_value < .01 ~ "***",
                        p_value < .05 ~ "**",
                        p_value < .10 ~ "*",
                        TRUE          ~ ""
                    )
                )
            },
            USE.NAMES = TRUE,
            simplify = FALSE
        ),
        nobs      = model$nobs
    )

    if (!is.null(extra_fields)) {
        if (!is.list(extra_fields)) {
            stop(paste("If 'extra_fields' is specified,",
                       "then it must be a list."))
        }

        output_object <- c(output_object, extra_fields)
    }

    if (verbose) {
        cat(sprintf("Writing to '%s'.\n", output_filename))
    }

    write(jsonlite::toJSON(output_object,
                           digits = 8,
                           auto_unbox = TRUE),
          file = output_filename)
}
