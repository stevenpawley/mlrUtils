#' Training function for custom preprocessor wrapper to center and scale data
#'
#' @param data data.frame. Training data including response variable
#' @param target character. Name of target (response) variable
#' @param args list. Arguments to center and/or scale the data
#' @return
#' @export
trainStandardRescaler = function(data, target, args = list(center=T, scale=T)) {

  # Identify numerical features
  cns = colnames(data)
  nums = setdiff(cns[sapply(data, is.numeric)], target)

  # Extract numerical features from the data set and call scale
  x = as.matrix(data[, nums, drop = FALSE])
  x = scale(x, center = args$center, scale=args$scale)

  # Store the scaling parameters in control
  # These are needed to preprocess the data before prediction
  control = args
  if (is.logical(control$center) && control$center)
    control$center = attr(x, "scaled:center")
  if (is.logical(control$scale) && control$scale)
    control$scale = attr(x, "scaled:scale")

  # Recombine the data
  data = data[, setdiff(cns, nums), drop = FALSE]
  data = cbind(data, as.data.frame(x))

  return(list(data = data, control = control))
}

#' Prediction function for custom preprocessor wrapper to center and scale data
#'
#' Prediction function that is required for custom preprocessor wrapper in order
#' to apply the wrapper to new data.
#'
#' @param data data.frame. Newdata to predict
#' @param target character. Name of target (response) variable
#' @param args list. Arguments to center and/or scale the data
#' @param control scaling parameters from trainfun to be used in the prediction
#' @return
#' @export
predictStandardRescaler = function(data, target, args, control) {

  # Identify numerical features
  cns = colnames(data)
  nums = cns[sapply(data, is.numeric)]

  # Extract numerical features from the data set and call scale
  x = as.matrix(data[, nums, drop = FALSE])
  x = scale(x, center = control$center, scale = control$scale)

  # Recombine the data
  data = data[, setdiff(cns, nums), drop = FALSE]
  data = cbind(data, as.data.frame(x))

  return(data)
}
