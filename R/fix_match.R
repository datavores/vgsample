#' Fix title match (one source-match pair).
#'
#' Uses a single source-match pair to update values throughout dataframe.
#'
#' This takes a single source-match pair and updates all the "flat" and "clean"
#' fields containing the match term with the source term. This is designed to
#' support interaction with complex fields where the elements are identified by
#' provided regex. In these cases, the matching element of the field will be
#' replaced and the rest of the field left untouched.
#'
#' NOTE: "flat" refers to the version of the term used during the matching
#' process. "clean" refers to the reader-friendly version of the term.
#'
#' @param source_term A string containing the flat term that will be retained
#'   and used to replace the match term in the target columns.
#' @param match_term A string containing the flat term to be replaced.
#' @param term_df The dataframe to be updated.
#' @param clean_col String with the name of the column "clean" term values.
#' @param flat_col String with the name of the column with "flat" term values.
#' @param start_reg, end_reg Strings providing the regular expressions that can
#'   be used to identify the start and and of a term.
#' @param split_reg String with regex identifying the pattern to split on when
#'   fields are complex.
#'
#' @return Returns an updated version of the passed-in dataframe.
#'
#' @export
fix_match <- function(source_term, match_term, term_df, clean_col, flat_col,
                      start_reg = "(?<=^|----)", end_reg = "(?=----|$)",
                      split_reg = "----") {
    # Get a record whose flat string has our source term in it.
    source_reg <- paste0(start_reg,
                         # This obnoxious looking regex simply replaces any
                         # regex metacharacters in the actual term with escaped
                         # versions so they don't break the regex.
                         gsub("([.|()\\^{}+$*?]|\\[|\\])",
                              "\\\\\\1",
                              source_term),
                         end_reg)
    source_string_index <- grepl(source_reg, as.data.frame(term_df)[, flat_col],
                                 perl = TRUE)
    source_string <- unlist(
        str_split(term_df[, flat_col][source_string_index][1],
                  split_reg)
    )

    # Identify the position of the source term in the string.
    source_term_index <- source_string %in% source_term

    # Get the clean string from the record.
    clean_string <- unlist(
        str_split(term_df[, clean_col][source_string_index][1],
                  split_reg)
    )

    # Get the clean term at the position identified for the flat string.
    clean_term <- clean_string[source_term_index]

    # Identify which records correspond to the match term.
    match_reg <- paste0(start_reg,
                        gsub("([.|()\\^{}+$*?]|\\[|\\])",
                             "\\\\\\1",
                             match_term),
                        end_reg)
    match_index <- grepl(match_reg, term_df[, flat_col], perl = TRUE)

    # Replace the flat and clean match terms.
    for(index in which(match_index)) {
        # Extract the flat string and identify where the match occurs.
        old_flat_string <- unlist(str_split(term_df[, flat_col][index],
                                            split_reg))
        old_term_index <- old_flat_string %in% match_term

        # Make a new flat string.
        old_flat_string[old_term_index] <- source_term
        new_flat_string <- paste(old_flat_string, collapse = split_reg)

        # Extract the clean string.
        old_clean_string <- unlist(str_split(term_df[, clean_col][index],
                                             split_reg))

        # Make a new clean string.
        old_clean_string[old_term_index] <- clean_term
        new_clean_string <- paste(old_clean_string, collapse = split_reg)

        # Update the dataframe.
        term_df[, flat_col][index] <- new_flat_string
        term_df[, clean_col][index] <- new_clean_string
    }

    # Return the updated data frame.
    return(term_df)
}
