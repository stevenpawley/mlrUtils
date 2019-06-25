#' Preprocessing scaling wrapper
#'
#' @param learner mlr learner
#' @param center_data logical, default = TRUE
#' @param scale_data logical, default = TRUE
#'
#' @return PreprocWrapper
#' @export
makePreprocWrapperScale = function(learner, center_data = TRUE, scale_data = TRUE) {
  trainfun = function(data, target, args = list(center_data, scale_data)) {
    cns = colnames(data)
    nums = setdiff(cns[sapply(data, is.numeric)], target)

    x = as.matrix(data[, nums, drop = FALSE])
    x = scale(x, center = args$center_data, scale = args$scale_data)
    control = args

    if (is.logical(control$center_data) && control$center_data)
      control$center_data = attr(x, "scaled:center")

    if (is.logical(control$scale_data) && control$scale_data)
      control$scale_data = attr(x, "scaled:scale")

    data = data[, setdiff(cns, nums), drop = FALSE]
    data = cbind(data, as.data.frame(x))

    return(list(data = data, control = control))
  }

  predictfun = function(data, target, args, control) {
    cns = colnames(data)
    nums = cns[sapply(data, is.numeric)]
    x = as.matrix(data[, nums, drop = FALSE])
    x = scale(x, center = control$center_data, scale = control$scale_data)
    data = data[, setdiff(cns, nums), drop = FALSE]
    data = cbind(data, as.data.frame(x))
    return(data)
  }

  makePreprocWrapper(
    learner,
    train = trainfun,
    predict = predictfun,
    par.set = makeParamSet(
      makeLogicalLearnerParam("center_data"),
      makeLogicalLearnerParam("scale_data")),
    par.vals = list(center_data = center_data, scale_data = scale_data))
}
