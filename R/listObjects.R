#' List objects in a binary R data file
#'
#' This function loads a binary R data file and returns a data frame
#' with information about the contained objects.
#'
#' @param fileName Character string specifying the name of the binary R data file.
#'
#' @return A data frame with columns: "ObjectName", "ObjectType", "ObjectDimensions".
#'
#' @examples
#' X<- matrix(runif(100), nrow=20)
#' Y<- data.frame(Y1=rnorm(10), Y2=rpois(10, lambda=1), sex=sample(c(0,1), 10, replace=TRUE))
#' Z=c(X, Y)
#' save(X,Y,Z, file="binData.Rda")
#' listObjects(fileName = "binData.Rda")
#'
#' @importFrom methods as
#' @importFrom utils head
#'
#' @export
listObjects <- function(fileName) {
  # Create an empty environment
  obj_env <- new.env()
  # Load objects into the environment
  loaded_objects <- load(fileName, envir = obj_env)
  # List objects in the environment
  object_names <- ls(envir = obj_env)
  
  object_info <- data.frame(
    ObjectName = character(),
    ObjectType = character(),
    ObjectDimensions = character(),
    stringsAsFactors = FALSE
  )
  names4Cols <- colnames(object_info)
  for (obj_name in names(loaded_objects)) {
    obj <- loaded_objects[[obj_name]]
    obj_type <- class(obj)[1]
    obj_dims <- paste(dim(obj), collapse = "x")
    object_info <- rbind(object_info, c(obj_name, obj_type, obj_dims))
  }
  colnames(object_info) <- names4Cols

  return(object_info)
}

