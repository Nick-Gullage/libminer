#' A function title
#'
#' A function description
#'
#' @param sizes LOGICAL; if TRUE (default), adds a column of the total bit size of libraries
#'
#' @return a data.frame of packages called
#' @export
#'
#' @examples
#' lib_summary()
lib_summary <- function(sizes = FALSE) {
  if (!is.logical(sizes)) {
    stop("'sizes' must be logical (TRUE or FALSE)")
  }

  pkg_df <- lib()
  pkg_tbl <- table(pkg_df[, "LibPath"])
  pkg_df <- as.data.frame(pkg_tbl, stringsAsFactors = FALSE)

  names(pkg_df) <- c("Library", "n_packages")

  if (isTRUE(sizes)) {
    pkg_df$lib_size <- map_dbl(
      pkg_df$Library,
      \(x) sum(fs::file_size(fs::dir_ls(x, recurse = TRUE)))
    )
  }

  pkg_df
}

#' Get libraries
#'
#' Another description
#'
#' @return a data.frame of packages
#'
#' @examples
#' lib()
lib <- function() {
  pkgs <- utils::installed.packages()
  as.data.frame(pkgs, stringsAsFactors = FALSE)
}
