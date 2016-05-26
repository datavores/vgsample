#' Fuzzy match (term vector).
#'
#' Checks all terms in a term vector for fuzzy matches.
#'
#' Takes a character vector (population of terms) as input. Uses fuzzy_match to
#' evaluate each term against the rest of the terms. Behavior can be tweaked in a
#' variety of ways that will both impact function runtime and adjust the criteria
#' for what terms should be evaluated and what constitutes a match among terms.
#' Returns a list of the evaluated terms and their associated matches.
#'
#' Note: The function is structured so that no backwards evaluation occurs. If a
#' term has been compared against the term population (i.e., served as the source
#' term), it is removed from further comparisons. This reduces the number of
#' comparisons that are required as the code progresses and minimizes redundant
#' work.
#'
#' @param term_vector Character vector of terms to be evaluated.
#' @param max_dist Numeric from 0 to 1. Sets threshold for no match. See agrepl.
#' @param min_test_length Integer. Sets minimum length for term to be evaluated
#'   at all. Note: Excluded terms can still be matched against, they just won't
#'   be used as source terms.
#' @param skip_pure_digit Boolean. If TRUE, a term that consists only of digits
#'   will not be evaluated at all. Note: Same behavior as for min_test_length.
#' @param assume_unique Boolean. If TRUE, the function assumes that 100% matches
#'   have already been filtered. In this case, the function attempts to minimize
#'   matches by ignoring any terms that have more than match_max matches. This is
#'   based on the logic that terms with high match rates in a unique set are more
#'   likely to be "promiscuous" terms (i.e., have highly very common
#'   characters/patterns, such as "the") than duplicate terms. Such terms can
#'   still be matched against but will not be treated as source terms. If
#'   remove_matches is set to TRUE, this also prevent excess removal of terms
#'   from the population due to high-match rates from early terms.
#' @param match_max Integer. If assume_unique == TRUE, then this sets the
#'   threshold for excluding "high" match terms from evaluation.
#' @param remove_matches Boolean. If TRUE, terms will be removed from the
#'   population being evaluated against if they are ever flagged as a match. This
#'   shrinks the term population any time assocations are discovered, reducing
#'   the number of comparisons for following source terms. This also helps us
#'   avoid a variety of tricky issues that arise when terms can matched multiple
#'   times or when terms can act as both source and match.
#' @param dist_method The method used to measure similarity between two strings.
#'   See "?stringdist" for details and links to method descriptions.
#' @param jw_penalty The default similarity metric is the Jaro distance. A
#'   penalty can be applied to convert to using Jaro-Winkler distance.
#'
#' @return
#'  Returns
#'
#' @export
fuzzy_match_all <- function(term_vector, max_dist = 0.1,
                            min_test_length = NA, skip_pure_digit = FALSE,
                            assume_unique = FALSE,
                            match_max = 10, remove_matches = FALSE,
                            dist_method = "jw", jw_penalty = 0) {
    # Create our target bucket - this will catch the results of each source term
    # tested along with any terms in the remaining term population that are
    # within max_dist of similarity.
    test_results <- list()

    # Assess the total number of terms. This is used for tracking overall
    # progress.
    total_terms <- length(term_vector)

    # Make a test counter to send updates to the user.
    test_counter <- -1

    # As long as terms remain to process.. keep testing.
    while(length(term_vector) > 0) {
        # Increment our counter.
        test_counter <- test_counter + 1

        # Every 50 tests, send a progress report to the terminal.
        if(test_counter %% 50 == 0) {
            message(sprintf("Percent remaining: %s",
                            100 * (length(term_vector) / total_terms))
            )
        }

        # Pop the lead term off of the term_vector to use as our target for
        # comparison.
        target_term <- term_vector[1]
        term_vector <- term_vector[-1]

        # As long as the term population still has items in it...
        if(length(term_vector) > 0) {
            # Test source term similarity against the remaining term population.
            test_result <- fuzzy_match(target_term, term_vector,
                                       max_dist = max_dist,
                                       min_test_length = min_test_length,
                                       skip_pure_digit = skip_pure_digit,
                                       dist_method = dist_method,
                                       jw_penalty = jw_penalty)
        } else {
            # Otherwise append the current test result as NA and move on to the
            # next loop (should only really occur when the current target_term
            # was the last term in the population - the next loop will end the
            # testing).
            test_results[[target_term]] <- NA
            next()
        }

        # If unique names have been passed to the function, it likely doesn't
        # make sense to accept terms with excessively large numbers of matches.
        # In such cases, we update our result to identify the terms as
        # problematic and to insure that we don't remove matches from the
        # term population.
        if(assume_unique & (length(test_result[["matches"]]) > match_max)) {
            test_result[["matches"]] <- "---max exceeded---"
            test_result[["match_logic"]] <- rep(FALSE, length(term_vector))
        }

        # Update our test_results bucket.
        test_results[[target_term]] <- test_result[["matches"]]

        # One way to make this work more efficient is to remove terms from the
        # population once they have been matched to something - shrinking the
        # term population any time assocations are discovered. This also
        # helps us avoid a variety of tricky issues that arise when terms
        # can matched multiple times or when terms can act as both source
        # and match.
        if(remove_matches) {
            term_vector <- term_vector[!test_result[["match_logic"]]]
        }
    }

    # Pass back our work.
    message("All done!")
    return(test_results)
}
