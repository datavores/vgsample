#' Sort complex elements.
#'
#' Sorts (and optionally deduplicates) the elements of complex field.
#'
#' @param tar_vector The vector of strings with elements to sort.
#' @param split_reg String providing the regex logic for breaking/collapsing
#'   target strings.
#' @param decreasing Boolean controlling sort direction.
#'
#' @return Returns an updated vector.
#'
#' @export
sort_elements <- function(tar_vector, split_reg = "----",
                          decreasing = FALSE, dedup = TRUE,
                          collapse_first = TRUE, cf_split = split_reg) {
    # Standardize blanks and NAs.
    tar_vector <- gsub(" {2,}", " ", tar_vector)
    tar_vector[which(tar_vector == "")] <- NA
    tar_vector <- str_replace_na(tar_vector)

    # If requested, the vector is collapsed into a single term which will then
    # be processed as below.
    if(collapse_first) {
        # First perform the sorting on the individual vector elements (in
        # case they are themselves complex).
        tar_vector <- sort_elements(tar_vector, split_reg = split_reg,
                                    decreasing = decreasing, dedup = dedup,
                                    collapse_first = FALSE)

        # Then collapse the elements together.
        tar_vector <- paste(tar_vector, collapse = cf_split)

        # Adjust the split_reg so that it operates on the collapsed elements.
        split_reg <- cf_split
    }

    # The element(s) in the target vector are processed.
    new_vector <- sapply(tar_vector, function(x) {
        # Break the current string up into its elements.
        old_string_parts <- unlist(str_split(x, split_reg))

        # If requested, deduplicate the elements.
        if(dedup) {
            old_string_parts <- unique(old_string_parts)
        }

        # Sort the elements.
        new_string_parts <- sort(old_string_parts, decreasing = decreasing)

        # Collapse the sorted string.
        new_string <- paste(new_string_parts, collapse = split_reg)

        # Clean out any junk features (likely to get added in instances where
        # there are empty character vectors - which should be sorted first).
        new_string <- gsub("(?<=^|----)NA(?=$|----)", "", new_string,
                           perl = TRUE)
        new_string <- gsub("(?<=^)----", "", new_string, perl = TRUE)
        new_string <- gsub("----(?=$)", "", new_string, perl = TRUE)
        new_string <- gsub("-{5,}", "----", new_string, perl = TRUE)

        # Assess if any content remains after cleaning out junk. If not, simply
        # return NA.
        if(new_string == "") {
            return("NA")
        } else {
            return(new_string)
        }
    }, USE.NAMES = FALSE)

    # If returning the final version of the sorted string, turn string NAs to
    # real NAs.
    if(collapse_first) {
        new_vector[which(new_vector == "NA")] <- NA
    }

    return(new_vector)
}
