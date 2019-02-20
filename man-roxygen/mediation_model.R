#' @return Returns an object of class "\code{mediation_model}".
#'
#'   An object of class "\code{mediation_model}" is a list containing at least
#'   the components:
#'
#'   \item{type}{A character string containing the type of model that has been
#'     conducted (e.g., \code{"simple mediation"}).}
#'   \item{method}{A character string containing the approach that has been
#'     used to conduct the mediation analysis (usually
#'   \code{"joint significance"}).} \item{params}{A named list of character
#'   strings describing the variables used in the model.}
#'   \item{paths}{A named list containing information on each relevent path of 
#'     the mediation model.}
#'   \item{indirect_index}{A boolean indicating whether an indirect effect index
#'   has been computed or not. Defaults to \code{FALSE}. See
#'   \code{\link{add_index}} to compute mediation index.}
#'   \item{indirect_index_infos}{(Optional) An object of class
#'     \code{"indirect_index"}. Appears when one apply \code{\link{add_index}}
#'     to an object of class \code{"mediation_model"}.}
#'   \item{js_models}{A list of objects of class \code{"lm"}. Contains every
#'     model relevant to joint-significance testing.}
#'   \item{data}{The original data frame that has been been passed through
#'     \code{data} argument.}
