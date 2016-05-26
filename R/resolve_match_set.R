#' Resolve match (source and all matches).
#'
#' Evaluates a suggested source term and its collection of matches.
#'
#' Takes a match-set - as defined by the fuzzy match function - and evaluates
#' all the pairings. Attempts to choose the best source and matches and then
#' evaluates whether it is appropriate to consider the set auto-resolved.
#'
#' @param match_set
#'
#' @return Returns a list with the same components as resolve_match ("source",
#' "match", "auto_accept"). However, "match" now contains all retained matches
#' and "auto_accept" is an evaluation of the set rather than a single pair.
#'
#' @export
resolve_match_set <- function(match_set) {
    # Capture the key pieces of the match set.
    source_term <- names(match_set)
    match_terms <- match_set[[1]]

    # Do a quick evaluation to see if there are actually any source-match pairs
    # to evaluate. If none, return a list of appropriate form from
    # resolve_match.
    if(all(is.na(match_terms))) {
        return(resolve_match(source_term, NA))
    }

    # Define the elements we will build as we evaluate the set.
    match_results <- c()
    auto_accepts <- c()

    # Contrast each of the match terms against the source term, updating the
    # source term in cases where a match term proves superior.
    for(match_term in match_terms) {
        # Do the pairwise evaluation.
        result <- resolve_match(source_term, match_term)

        # Update source_term to match whichever term was selected as the best
        # source by resolve_match.
        source_term <- result[["source"]]

        # Add the selected match term to our set.
        match_results <- c(match_results, result[["match"]])

        # Add the auto-accept evaluation to our set.
        auto_accepts <- c(auto_accepts, result[["auto_accept"]])
    }

    # Definite non-matches get returned as NAs. If the above process evaluated
    # all possible match terms as non-matches, return a list of appropriate form
    # from resolve_match.
    if(all(is.na(match_results))) {
        return(resolve_match(names(match_set), NA))
    }

    # Clean out any definite non-matches and their evaluation results.
    clean_match_results <- match_results[!is.na(match_results)]
    clean_auto_accepts <- auto_accepts[!is.na(match_results)]

    # If all remaining match terms are definite matches, define our new match
    # set as the winning source term and the remaining match terms.
    if(all(clean_auto_accepts)) {
        new_set <- list(
            "source" = source_term,
            "match" = clean_match_results,
            "auto_accept" = TRUE
        )

        return(new_set)
    } else {
        # Otherwise, return the original result set and request flag the set for
        # manual evaluation.
        new_set <- list(
            "source" = names(match_set),
            "match" = match_set[[1]],
            "auto_accept" = FALSE
        )

        return(new_set)
    }
}
