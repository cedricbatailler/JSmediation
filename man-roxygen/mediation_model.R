#' @return Returns an object of class "\code{mediation_model}" object.
#'
#'   An object of class "\code{mediation_model}" is a least containing at list
#'   the components:
#'
#'   \item{type}{A character string containing the type of model that has been
#'     conducted (e.g., \code{"simple mediation"}).}
#'   \item{method}{A character string containing the approach that has been
#'     used to conduct the mediation analysis (usually
#'     \code{"joint significance"}).}
#'   \item{params}{A named list of character strings describing variables used 
#'     in the model.}
#'   \item{paths}{A named list containing information on each relevent path of 
#'     the mediation model.}
#'   \item{indirect_index}{A boolean indicating wether indirect effect index has
#'     been computed. Defaults to \code{FALSE}. See \code{\link{add_index}} to
#'     compute mediation index.}
#'   \item{indirect_index_infos}{(Optional) An object of class
#'     \code{"indirect_index"}. Appears when one apply \code{\link{add_index}}
#'     to an object of class \code{"mediation_model"}.}
#'   \item{js_models}{A list of objects of class \code{"lm"}. Contains every
#'     model used in joint-significance test.}
#'   \item{data}{The original data frame that has been been passed through
#'     \code{data} argument.}
