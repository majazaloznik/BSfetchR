#' Dataset: full
#'
#' Full hierarchy of tables and category levels from the BSI website.
#' Data is created using the get_all_bsi_tables() funciton
#' and is saved in the package to avoid repeated scraping.
#'
#' @format A data frame with [193] rows and [16] variables:
#' \describe{
#'   \item{type}{node type}
#'   \item{name}{name of node}
#'   \item{url}{url of node}
#'   \item{parent}{parent of node}
#'   \item{full_url}{full url of node}
#'   \item{level}{level of node}
#'   \item{px_url}{px url of node}
#'   \item{modified_date}{date of last modification}
#'   \item{top_parent}{top parent of node}
#'   \item{category_code}{category code of node}
#'   \item{subcategory_code}{subcategory code of node}
#'   \item{px_code}{px code of node}
#'   \item{category_id}{id of category}
#'   \item{subcategory_id}{id of subcategory}
#'   \item{id}{id of node}
#'   \item{parent_id}{id of parent node}
#'
#'   ...
#' }
"full"
