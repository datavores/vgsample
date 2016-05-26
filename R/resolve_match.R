#' Resolve match (source and one match).
#'
#' Inspects source-match pair to see if can auto-determine if valid match or
#' not.
#'
#' The heart of the auto-resolver, this function contains the logic for
#' evaluating a single source--match pair of strings.
#'
#' @param source_term The term selected for testing against similar terms.
#' @param match_term A term identified as similar.
#'
#' @return Returns a list specifying which term was selected as the source
#' ("source"), which term (if either) was selected as the match ("match"), and
#' whether the pair was considered auto-resolved or needs manual attention
#' ("auto_accept").
#'
#' @export
resolve_match <- function(source_term, match_term) {
    # Define the possible outcomes for the comparison.
    outcome_set <- list(
        "manual_review" = list(
            "source" = source_term,
            "match" = match_term,
            "auto_accept" = FALSE
        ),
        "no_match" = list(
            "source" = source_term,
            "match" = NA,
            "auto_accept" = FALSE
        ),
        "match" = list(
            "source" = source_term,
            "match" = match_term,
            "auto_accept" = TRUE
        ),
        "reverse_match" = list(
            "source" = match_term,
            "match" = source_term,
            "auto_accept" = TRUE
        )
    )

    ## NA TESTING ##
    # Test for NA. Will flag for manual review but NA term handling is automated
    # at the fuzzy match processing function.
    if(is.na(match_term) | match_term %in% "---max exceeded---") {
        return(outcome_set$manual_review)
    }

    ## MATCH TESTING ##
    # Test conditions that signal there is a true match. Take the richer term
    # as source.

    # Are the source/match identical if punctuation and spacing are removed? If
    # punctuation is complex (e.g., intentional "##"), the leading punctuation
    # will be preserved to indicate this complexity.
    bare_source <- gsub("[[:punct:]](?![[:punct:]])|[[:space:]]", "",
                        source_term, perl = TRUE)
    bare_match <- gsub("[[:punct:]](?![[:punct:]])|[[:space:]]", "",
                       match_term, perl = TRUE)

    # If identical, adopt whichever original term is longer (signaling that it
    # has more punctuation) as the source.
    if(bare_source %in% bare_match) {
        if(nchar(source_term) > nchar(match_term)) {
            return(outcome_set$match)
        } else if(nchar(match_term) > nchar(source_term)) {
            return(outcome_set$reverse_match)
        } else {
            # If the original term length was identical, favor whichever term
            # explicitly has more punctuation characters.
            punct_source <- nchar(gsub("[^[:punct:]]", "", source_term))
            punct_match <- nchar(gsub("[^[:punct:]]", "", match_term))

            if(punct_source > punct_match) {
                return(outcome_set$match)
            } else if(punct_match > punct_source) {
                return(outcome_set$reverse_match)
            } else {
                # If can't disambiguate, flag for manual review.
                return(outcome_set$manual_review)
            }
        }
    }

    ## MISMATCH TESTING ##
    # Test conditions that signal a match is very unlikely. This will replace
    # the match as NA so that it can be handled appropriately.

    # After space/punctuation has been accounted for, check if the remaining
    # differences can be attributed to differences in number. This is almost
    # certainly a non-match as it is unlikely for numbers to vary across titles
    # unless intentionally.
    numless_source <- gsub("[[:digit:]]", "", bare_source)
    numless_match <- gsub("[[:digit:]]", "", bare_match)

    if(numless_source %in% numless_match) {
        return(outcome_set$no_match)
    }

    # Series games may have very similar titles followed by subtitles. Where
    # there is an indicator of a serial game, we pull out the indicator and
    # contrast the volume numbers directly. Mismatch suggests that we can
    # ignore.
    source_has_serial <- grepl("(volume)|(episode)|(number) [[:digit:]]",
                               source_term)
    match_has_serial <- grepl("(volume)|(episode)|(number) [[:digit:]]",
                              match_term)

    if(source_has_serial & match_has_serial) {
        extract_regex <- "(?:(volume)|(episode)|(number)) [[:digit:]]*"

        serial_source <- str_extract(extract_regex, source_term)
        serial_match <- str_extract(extract_regex, match_term)

        serial_source <- gsub("[[:digit:]]", "", serial_source)
        serial_match <- gsub("[[:digit:]]", "", serial_match)

        if(serial_source %in% serial_match) {
            return(outcome_set$no_match)
        }
    }

    ## MANUAL REVIEW CLEAN-UP ##
    # If none of the test conditions are met, return the source/match pair as-is
    # and flag for manual review.
    return(outcome_set$manual_review)
}
