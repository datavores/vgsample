#' Resolve match (list of source-match sets).
#'
#' Attempts to resolve each of a collection of match sets.
#'
#' Takes a list of match sets, applies resolve_match_set to each, and gathers
#' the results up in a list whose features can be fed to the
#' process_fuzzy_matches function.
#'
#' @param match_set_list
#'
#' @return Returns a list with two elements. "new_set_list" is a cleaned version
#' of match_set_list. It contains the same number of elements but with the
#' sources and match terms selected by resolve_match_set. "auto_accept_index" is
#' a Boolean index indicating which of the elements in new_set_index can be
#' assumed to be correct matches (and thus do not need manual user review).
#'
#' @export
resolve_all_match_sets <- function(match_set_list) {
    # Define major buckets we want to populate: a cleaned up version of
    # match_set_list and the corresponding collection of auto_accept results.
    new_set_list <- list()
    auto_accept_index <- c()

    # For each element (i.e., match set) in match_set_list...
    for(match_index in 1:length(match_set_list)) {
        # Evaluate the match set via resolve_match.
        result <- resolve_match_set(match_set_list[match_index])

        # Add the selected source term and matches to our cleaned match set list
        # bucket.
        new_set_list[[result[["source"]]]] <- result[["match"]]

        # And the auto_accept result.
        auto_accept_index <- c(auto_accept_index, result[["auto_accept"]])
    }

    # Gather and return the cleaned up match sets and corresponding
    # auto-evaluation results.
    resolved_results <- list(
        "match_set_list" = new_set_list,
        "auto_accept_index" = auto_accept_index
    )

    return(resolved_results)
}
