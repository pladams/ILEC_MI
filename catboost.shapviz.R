shapviz.catboost.Model <- function(object, X_pred, X = X_pred, collapse = NULL, ...) {
  if (!requireNamespace("catboost", quietly = TRUE)) {
    stop("Package 'catboost' not installed")
  }
  stopifnot(
    "X must be a matrix or data.frame. It can't be an object of class catboost.Pool" =
      is.matrix(X) || is.data.frame(X),
    "X_pred must be a matrix, a data.frame, or a catboost.Pool" =
      is.matrix(X_pred) || is.data.frame(X_pred) || inherits(X_pred, "catboost.Pool"),
    "X_pred must have column names" = !is.null(colnames(X_pred))
  )
  
  if (!inherits(X_pred, "catboost.Pool")) {
    X_pred <- catboost.load_pool(X_pred)
  }
  
  S <- catboost.get_feature_importance(object, X_pred, type = "ShapValues", ...)
  
  # Call matrix method
  pp <- ncol(X_pred) + 1
  baseline <- S[1, pp]
  S <- S[, -pp, drop = FALSE]
  colnames(S) <- colnames(X_pred)
  shapviz(S, X = X, baseline = baseline, collapse = collapse)
}
