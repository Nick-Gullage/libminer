#' A function title
#'
#' A function description
#'
#' @return a data.frame of packages called
#' @export
#'
#' @examples
#' a use-case example
#' lib_summary()
lib_summary <-  function(){
  pkgs <- utils::installed.packages()
  pkg_tbl <- table(pkgs[, "LibPath"])
  pkg_df <-  as.data.frame(pkg_tbl, stringsAsFactors = F)
  names(pkg_df) <- c("library", "n_packages")
  pkg_df
}
