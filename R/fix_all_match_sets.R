#' Fix match (list of source-match sets).
#'
#' Uses a list of source-match collections to update values throughout
#' dataframe.
#'
#' This takes collection of source-match lists and updates all the "flat" and
#' "clean" fields containing the match terms with the respective source terms.
#' This is designed to support interaction with complex fields where the
#' elements are identified by provided regex. In these cases, the matching
#' element of the field will be replaced and the rest of the field left
#' untouched.
#'
#' NOTE: "flat" refers to the version of the term used during the matching
#' process. "clean" refers to the reader-friendly version of the term.
#'
#' @param match_set_list A list whose element names are flat term that will be
#'   retained whose sub-elements are the match terms to be replaced in the
#'   target columns.
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
fix_all_match_sets <- function(match_set_list, term_df, clean_col, flat_col,
                               start_reg, end_reg, split_reg) {
    # Loop over all the source-match lists...
    for(set_index in 1:length(match_set_list)) {
        # Provide progress updates to the user.
        if(set_index %% 50 == 0 |
           set_index == 1 |
           set_index == length(match_set_list)) {
            message(sprintf("Set %s of %s, Percent complete: %s",
                            set_index, length(match_set_list),
                            100 * (set_index / length(match_set_list))
            )
            )
        }

        # Perform the dataframe updates for the current source-match(es)
        # list.
        term_df <- fix_match_set(match_set_list[set_index],
                                 term_df, clean_col, flat_col)
    }

    # Return the updated dataframe.
    return(term_df)
}
