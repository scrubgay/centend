#' Nix care-ofs and attentions
#' @description In datasets where address is split between two fields,
#' nullify the field that contains a C/O or ATTN
#' @details This function is based off a Shimberg-specific application where
#' address is split in between two fields but one field maximum is required
#' by the analysis. C/Os and ATTNs make it harder to pattern match with
#' a registered agents field so this function takes care of it.
#'
#' This is an optional function in the centend version of this script.
#'
#' @param address An address vector to be cleaned
#' @returns a vector where each entry containing C/O or ATTN has been
#' transformed into an NA

remove_attn <- \(x) {
  ifelse(
    word(x, 1) %>%
      str_remove("(:|;)") %>%
      str_detect(regex_or(
        "ATT", "ATTN", "ATTENTION", "C./O", "C.O",
        "C/", "C/0", "C/I", "C/JO", "C/O", "C\\O", "CO/")
      ),
    NA,
    x
  )
}
