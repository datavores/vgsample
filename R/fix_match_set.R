#' Fix match (source and all matches).
#'
#' Uses a list of a source and its matches to update values throughout
#' dataframe.
#'
#' This takes a single source term and a list of its matches and updates all the
#' "flat" and "clean" fields containing the match terms with the source term.
#' This is designed to support interaction with complex fields where the
#' elements are identified by provided regex. In these cases, the matching
#' element of the field will be replaced and the rest of the field left
#' untouched.
#'
#' NOTE: "flat" refers to the version of the term used during the matching
#' process. "clean" refers to the reader-friendly version of the term.
#'
#' @param match_set A list whose name is flat term that will be retained and
#'   used to replace the element match terms in the target columns.
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
fix_match_set <- function(match_set, term_df, clean_col, flat_col,
                          start_reg = "(?<=^|----)", end_reg = "(?=----|$)",
                          split_reg = "----") {

    # Extract the source term from the set.
    source_term <- names(match_set)

    # Cycle through the source-match pairs, updating the dataframe for each.
    for(match_term in match_set[[1]]) {
        term_df <- fix_match(source_term, match_term, term_df,
                             clean_col, flat_col, start_reg, end_reg,
                             split_reg)
    }

    # Return the updated dataframe.
    return(term_df)
}
