% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/repeated_irregular_poly.R
\name{makepredictcall.ripoly}
\alias{makepredictcall.ripoly}
\title{Use this to append the custom ripoly function in makepredictcall.default (for model.frame.default).}
\usage{
\method{makepredictcall}{ripoly}(var, call)
}
\arguments{
\item{var}{Variable}

\item{call}{The term used in the formula}
}
\description{
Use this to append the custom ripoly function in makepredictcall.default (for model.frame.default).
}
\examples{
# General example
ripoly(c(1:5), 2)

# Linear mixed model example
model1 <- lmer(Reaction ~ ripoly(Days, 2) + (ripoly(Days, 2) | Subject), sleepstudy))

# Predictor effects package
ef <- predictorEffects(model1, Days)
}
