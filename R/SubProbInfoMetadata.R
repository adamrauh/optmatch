#' Tracking information related to subproblems
#' For a given MCFSolution, add relevant metadata to the SubProbInfo table associated with a particular subproblem.
#'
#' @param mcfsoln an object of class MCFSolution
#' @param dm EdgeList
#' @param maxerr double
#' @param feasibility logical
#' @param ... named list specifying which constraints and/or other parameters related to a particular subproblem that should be tracked and added to the SubProbInfo table. An additional column will be added to the table for each item passed.
#'
#' @return
#' @keywords internal
trackSubProblemInfoConstraints <- function(mcfsoln,
                                      dm,
                                      maxerr,
                                      feasibility,
                                      ...)
{
  mcfsoln@subproblems[1L, "exceedance"]  <- maxerr
  mcfsoln@subproblems[1L, "feasible"]  <- feasibility

  ## Presently we can treat this subproblem as non-flipped even if it was,
  ## since `dm` will have been transposed in the event of flipping.  Doing
  ## so will prevent the evaluate_* routines below from getting confused.
  ## Of course we have to remember to set it based on actual information as
  ## the MCFsolutions object passes up through the point in the call stack
  ## where that transposition was made.
  mcfsoln@subproblems[1L, "flipped"]  <- FALSE
  ## ... and now we can proceed with:
  evaluate_lagrangian(dm, mcfsoln) ->
    mcfsoln@subproblems[1L, "lagrangian_value"]
  evaluate_dual(dm, mcfsoln) ->
    mcfsoln@subproblems[1L,   "dual_value"]
  evaluate_primal(dm, mcfsoln) ->
    mcfsoln@subproblems[1L,   "primal_value"]

  otherBookkeepingParams <- list(...)
  # replace NULL arguments with an NA to preserve them in data frame
  # idea is to enable future flexibility but ensure standard dimensions for a set of arguments
  otherBookkeepingParams <- lapply(otherBookkeepingParams,
                                   function(x) if (is.null(x)) NA else x)
  lengths <- sapply(otherBookkeepingParams, length)
  if (isFALSE(all(lengths ==1)))
  {
    stop("Constraint parameters not well specified")
  }

  otherParams <- as.data.frame(otherBookkeepingParams)
  mcfsoln@subproblems <-
    new("SubProbInfo", cbind(mcfsoln@subproblems, otherParams))

  return(mcfsoln)
}
