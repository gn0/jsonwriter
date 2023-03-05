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
#' @param output_filename
#'
#' @return Nothing.
#' @export
write_json <- function(model, output_filename, ...) {
    UseMethod("write_json")
}

#' @export
write_json.default <- function(model, output_filename, ...) NULL

#' @export
write_json.fixest <- function(model,
                              output_filename,
                              verbose = FALSE) {
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

    if (verbose) {
        cat(sprintf("Writing to '%s'.\n", output_filename))
    }

    write(jsonlite::toJSON(output_object,
                           digits = 8,
                           auto_unbox = TRUE),
          file = output_filename)
}
