#' Fuzzy match (single term).
#'
#' Checks term against term vector for fuzzy matches.
#'
#' Takes a character string (term) and a character vector (population of terms)
#' as inputs. Uses the base R agrepl function to perform fuzzy string matching
#' (see optional parameters for how this behavior can be tweaked). Returns a
#' list including a vector of any terms "matching" the target term and vector of
#' boolean values that can be used to subset the population of terms for the
#' same values.
#'
#' @param term Character string (term to be evaluated for matches).
#' @param term_vector Character vector (for term to be evaluated against).
#' @param max_dist Numeric from 0 to 1. Sets threshold for no match. See agrepl.
#' @param min_test_length Integer. Sets minimum length for term to be evaluated
#'   at all.
#' @param skip_pure_digit Boolean. If TRUE, a term that consists only of digits
#'   will not be evaluated at all.
#'
#' @return Returns a list with two elements, "match_logic" and "matches".
#'   "match_logic" is a vector of booleans that could be used to subset the term
#'   population to extract matched values. "matches" is a character vector with
#'   the matched values.
#'
#' @export
fuzzy_match <- function(term, term_vector, max_dist = 0.1,
                        min_test_length = NA, skip_pure_digit = FALSE,
                        dist_method = "jw", jw_penalty = 0) {

    # If requested, minimum test length observed?
    if(!is.na(min_test_length)) {
        if(nchar(term) < min_test_length) {
            return(list(
                "match_logic" = FALSE,
                "matches" = NA
            ))
        }
    }

    # If requested, characters other than digits?
    if(skip_pure_digit) {
        if(grepl("^[[:digit:]]*$", term)) {
            return(list(
                "match_logic" = FALSE,
                "matches" = NA
            ))
        }
    }

    # Perform the fuzzy string evaluation.
    match_test <- stringdist(term, term_vector,
                             method = dist_method,
                             p = jw_penalty)
    match_test <- match_test <= max_dist & !is.na(match_test)

    # Extract any values that matched under the string evaluation conditions.
    match_vector <- term_vector[match_test]

    # Were any matches found?
    if(length(match_vector) == 0) {
        result <- list(
            "match_logic" = FALSE,
            "matches" = NA
        )
    } else {
        result <- list(
            "match_logic" = match_test,
            "matches" = match_vector
        )
    }

    # Return the result.
    return(result)
}
