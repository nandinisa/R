# Handles orthogonoal polynomials for repeated irregular measures 
# Used in longitudinal data modeling with lmer, lcmm, predict effects packages instead of poly function
ripoly <- function (x, ..., degree = 1, coefs = NULL, raw = FALSE, simple = FALSE)
{
  if(!is.null(dim(x)) | dim(x)[2] > 1){
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
  colnames(x_new) <- c(colnames(x), 1L:degree)
  
  # to merge with the poly result
  x <- data.frame(x)
  x$id  <- 1:nrow(x)
  x <- merge(x, x_new, by=intersect(colnames(x), colnames(x_new)))
  x <- x[order(x$id), ] # maintain orginal order
  
  if (simple) return(as.matrix(x[, as.character(1L:degree)]))
  else {
    Z <- as.matrix(x[, as.character(1L:degree)])
    attr(Z, "coefs") <- list(alpha = attr(poly_result,"coefs")$alpha, 
                             norm2 = attr(poly_result,"coefs")$norm2)
    return(structure(Z, degree = 1L:degree, class = c("ripoly", "matrix")))
  }
}

# Use this to override the default functions (for model.frame.default)
makepredictcall.ripoly <- function (var, call) 
{
  if (as.character(call)[1L] == "ripoly" || (is.call(call) && 
                                           identical(eval(call[[1L]]), ripoly))) 
    call$coefs <- attr(var, "coefs")
  return(call)
}

predict.ripoly <- function (object, newdata, ...) 
{
  if (missing(newdata)) 
    object
  else if (is.null(attr(object, "coefs"))) 
    ripoly(newdata, degree = max(attr(object, "degree")), 
         raw = TRUE, simple = TRUE)
  else ripoly(newdata, degree = max(attr(object, "degree")), 
            coefs = attr(object, "coefs"), simple = TRUE)
}


