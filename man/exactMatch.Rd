\name{exactMatch-methods}
\docType{methods}
\alias{exactMatch}
\alias{exactMatch-methods}
\alias{exactMatch,formula-method}
\alias{exactMatch,vector-method}
\title{Generate an exact matching set of subproblems}
\description{
  An exact match is one based on a factor. Within a level, all observations are allowed to be matched.
  An exact match can be combined with another distance matrix to create a set of matching subproblems.
}
\usage{
\S4method{exactMatch}{vector}(x, treatment)

\S4method{exactMatch}{formula}(x, data = NULL, subset = NULL, na.action = NULL, ...)
}
\arguments{
  \item{x}{A factor vector or a formula, used to select method.}
  \item{treatment}{A vector the same length as \code{x} that can be coerced to a two level factor (e.g. a vector of 1s and 0s or a logical vector).}
  \item{data}{A \code{data.frame} or \code{matrix} that contains the variables used in the formula \code{x}.}
  \item{subset}{an optional vector specifying a subset of observations to be used}
  \item{na.action}{a function which indicates what should happen when the data contain `NA's}
  \item{...}{Additional arguments for methods.}
}
\details{
  \code{exactMatch} creates a block diagonal matrix of 0s and \code{Inf}s. The pairs
  with 0 entries are within the same level of the factor and legitimate matches.
  \code{Inf} indicates units in different levels. \code{exactMatch} replaces the
  \code{structure.fmla} argument to several functions in previous versions of Optmatch.

  For the \code{factor} method, the two vectors \code{x} and \code{treatment} must be the same
  length. The vector \code{x} is interpreted as indicating the grouping factors for the data,
  and the vector \code{treatment} indicates whether a unit is in the treatment or control groups.

  For the \code{formula} method, the \code{data} argument may be omitted, in which case the
  method attempts to find the variables in the environment from which the function was called.
  This behavior, and the arguments \code{subset} and \code{na.action}, mimics the behavior of
  \code{\link{lm}}.
}
\value{
  Object of class \code{DistanceSpecification}, which is suitable to be given
  as \code{distance} argument to \code{\link{fullmatch}} or
  \code{\link{pairmatch}}. The exact match will be only zeros and \code{Inf} values,
  indicating a possible match or no possible match, respectively. It can be added to
  a another distance matrix to create a subclassed matching problem.
}
\author{Mark M. Fredrickson}


\seealso{\code{\link{caliper}}, \code{\link{mdist}}, \code{\link{fullmatch}}, \code{\link{pairmatch}}}

\examples{
data(nuclearplants)

### First generate a standard propensity score
ppty <- glm(pr~.-(pr+cost), family = binomial(), data = nuclearplants)
ppty.distances <- mdist(ppty)

### Only allow matches within the partial turn key plants
pt.em <- exactMatch(pr ~ pt, data = nuclearplants)
as.matrix(pt.em)

### Blunt matches:
match.pt.em <- fullmatch(pt.em)
print(match.pt.em, grouped = TRUE)

### Combine the propensity scores with the subclasses:
match.ppty.em <- fullmatch(ppty.distances + pt.em)
print(match.ppty.em, grouped = TRUE)


}
\keyword{nonparametric }% at least one, from doc/KEYWORDS