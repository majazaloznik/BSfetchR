#' Dataset: full
#'
#' Full hierarchy of tables and category levels from the BSI website.
#' Data is created using the get_all_bsi_tables() funciton
#' and is saved in the package to avoid repeated scraping.
#'
#' @format A data frame with variables:
#' \describe{
#'   \item{type}{category, sub category or data table}
#'   \item{name}{name of the table or category}
#'   \item{px_url}{full URL to the PX file}
#'   \item{modified_date}{date of last modification}
#'   \item{category_code}{category code extracted from URL}
#'   \item{category_name}{name of the top-level category}
#'   \item{subcategory_code}{subcategory code extracted from URL, NA for top-level tables}
#'   \item{subcategory_name}{subcategory name}
#'   \item{px_code}{PX file code (e.g. "F2_A1S"), NA for category rows}
#'   \item{category_id}{integer ID of the category}
#'   \item{subcategory_id}{integer ID of the subcategory, NA if no subcategory}
#'   \item{id}{integer ID of the node, equal to subcategory_id if present, else category_id}
#'   \item{parent_id}{integer ID of the parent node, 0 for top-level categories}
#' }
"full"
#' Dataset: full_old
#'
#' Full from before. just in case.
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
"full_old"
