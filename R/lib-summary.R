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
lib_summary <-  function(sizes = F){
  if(!is.logical(sizes)){
    stop("Sizes must be logical.")
  }

  pkgs <- utils::installed.packages()
  pkg_tbl <- table(pkgs[, "LibPath"])
  pkg_df <-  as.data.frame(pkg_tbl, stringsAsFactors = F)
  names(pkg_df) <- c("Library", "n_packages")

  if(sizes){
    pkg_df$lib_size <- vapply(
      pkg_df$Library,
      \(x) {
        sum(fs::file_size(fs::dir_ls(x, recurse = T)))
      },
      FUN.VALUE = numeric(1)
    )
  }

  pkg_df
}
