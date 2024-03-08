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
#'              only 'fixest' is supported.
#' @param output_filename Filename to write the JSON output to.
#' @param verbose Flag to indicate whether to write console messages.
#'                (Optional.)
#' @param extra_fields A 'list' object with extra information to add to
#'                     the output.  (Optional.)
#' @param ... Additional arguments passed on to the method.
#'
#' @return \value{No return value, called for side effects.}
#' @examples
#' \dontrun{# The following example fits a regression on the built-in
#' # 'quakes' data set and writes the results to a file called
#' # "jsonwriter_example.json" in the current working directory.
#' #
#' library(fixest)
#' data(quakes)
#'
#' write_json(
#'     feols(depth ~ mag, quakes),
#'     "jsonwriter_example.json",
#'     extra_fields = list(hello = "world"))
#' }
#' @export
write_json <- function(model,
                       output_filename,
                       verbose = FALSE,
                       extra_fields = NULL,
                       ...) {
    UseMethod("write_json")
}

#' @export
write_json.default <- function(model,
                               output_filename,
                               verbose = FALSE,
                               extra_fields = NULL,
                               ...) {
    NULL
}

#' Write a 'fixest' object to a JSON file.
#'
#' @param model A 'fixest' object that contains the regression results.
#' @param output_filename Filename to write the JSON output to.
#' @param verbose Flag to indicate whether to write console messages.
#'                (Optional.)
#' @param extra_fields A 'list' object with extra information to add to
#'                     the output.  (Optional.)
#' @param vcov Standard error specification to pass to
#'             'fixest::coeftable'.  (Optional.)
#' @param ... Additional arguments.  Ignored.
#' @return \value{No return value, called for side effects.}
#' @export
write_json.fixest <- function(model,
                              output_filename,
                              verbose = FALSE,
                              extra_fields = NULL,
                              vcov = NULL,
                              ...) {
    model_coeftable <- fixest::coeftable(model, vcov = vcov)

    output_object <- list(
        call      = call_to_string(model$call),
        coef      = sapply(
            names(model$coefficients),
            function(coef_name) {
                coef <- list(
                    est = model_coeftable[coef_name, "Estimate"],
                    se = model_coeftable[coef_name, "Std. Error"]
                )

                if ("Pr(>|t|)" %in% colnames(model_coeftable)) {
                    coef$t <- model_coeftable[coef_name, "t value"]
                    coef$p <- model_coeftable[coef_name, "Pr(>|t|)"]
                } else if ("Pr(>|z|)" %in% colnames(model_coeftable)) {
                    coef$z <- model_coeftable[coef_name, "z value"]
                    coef$p <- model_coeftable[coef_name, "Pr(>|z|)"]
                } else {
                    warning(sprintf(
                        "No t-value or z-value and no p-value found for '%s'.",
                        output_filename
                    ))
                }

                if (!is.null(coef$p)) {
                    coef$stars <- dplyr::case_when(
                                             coef$p < .01 ~ "***",
                                             coef$p < .05 ~ "**",
                                             coef$p < .10 ~ "*",
                                             TRUE         ~ ""
                                         )
                }

                coef
            },
            USE.NAMES = TRUE,
            simplify = FALSE
        ),
        se_type   = attributes(model_coeftable)$type,
        nobs      = model$nobs
    )

    if (!is.null(model$ssr)) {
        output_object$r_squared <- 1 - model$ssr / model$ssr_null
    } else if (!is.null(model$pseudo_r2)) {
        output_object$pseudo_r_squared <- model$pseudo_r2
    }

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
