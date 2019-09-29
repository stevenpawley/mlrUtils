#' Prediction function for mlr regression model applied to raster data
#'
#' @param model mlr fitted model
#' @param data data passed from raster
#' @param lev named list of RasterLayer factor attribute tables. Each table
#' is a data.frame; the first column has to be named `ID` and contains a
#' list of unique pixel values in the raster; the second column contains the
#' factors that correspond to each pixel value.
#'
#' @return RasterLayer object
#' @export
predFun = function(model, data, lev = NULL) {
  data <- as.data.frame(data)

  if (!is.null(lev))
    for (fct_var in names(lev))
      data[[fct_var]] <- lev[[fct_var]][data[[fct_var]], 2]

    v = predict(model, newdata = data)
    v$data[, 1]
}
