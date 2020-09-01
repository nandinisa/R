#' Handles polynomials for repeated irregular measures.
#' Used in longitudinal data modeling ex: with lmer, lcmm, predict effects packages instead of poly function
#' @param x Numeric vector or matrix with a single column
#' @param degree Polynomial order
#' @param coefs Coeffecients of previous fit
#' @param raw T for raw data, F for orthogonal polynomials
#' @param simple T for matrix result, F for poly object
#' @examples
#' # General example
#' ripoly(c(1:5), 2)
#'
#' # Linear mixed model example
#' model1 <- lmer(Reaction ~ ripoly(Days, 2) + (ripoly(Days, 2) | Subject), sleepstudy))
#'
#' # Predictor effects package
#' ef <- predictorEffects(model1, Days)
#' @export
ripoly <- function (x, ..., degree = 1, coefs = NULL, raw = FALSE, simple = FALSE)
{
  if(!is.null(dim(x)) | (is.matrix(x) & identical(dim(x)[2] , as.integer(1)))){
    stop("x must be a numeric vector or matrix with 1 column")
  }
  # replicate same as poly function to use degree variable
  args <- list(...)
  if (nd <- length(args)) {
    dots_deg <- nd == 1L && length(args[[1L]]) == 1L
    if(dots_deg)
      degree <- args[[1L]]
  }

  # sort unique x values
  x_new <- sort(unique(x))
  poly_result <- poly(x_new, ..., degree = degree, coefs = coefs, raw = raw, simple = simple)
  x_new <- data.frame(cbind(x_new, poly_result))
  x <- data.frame(x)
  colnames(x_new) <- c(colnames(x), 1L:degree)

  # to merge with the poly result
  x$id  <- 1:nrow(x)
  x <- merge(x, x_new, by=intersect(colnames(x), colnames(x_new)))
  x <- x[order(x$id), ] # maintain orginal order

  if (simple) return(as.matrix(x[, as.character(1L:degree)]))
  else {
    Z <- as.matrix(x[, as.character(1L:degree)])
    attr(Z, "coefs") <- attr(poly_result, "coefs")
    return(structure(Z, degree = 1L:degree, class = c("ripoly", "matrix")))
  }
}


#' Use this to append the custom ripoly function in makepredictcall.default (for model.frame.default).
#' @param var Variable
#' @param call The term used in the formula
#' @examples
#' # General example
#' ripoly(c(1:5), 2)
#'
#' # Linear mixed model example
#' model1 <- lmer(Reaction ~ ripoly(Days, 2) + (ripoly(Days, 2) | Subject), sleepstudy))
#'
#' # Predictor effects package
#' ef <- predictorEffects(model1, Days)
#' @export
makepredictcall.ripoly <- function (var, call)
{
  if (as.character(call)[1L] == "ripoly" || (is.call(call) &&
                                           identical(eval(call[[1L]]), ripoly)))
    call$coefs <- attr(var, "coefs")
  return(call)
}

