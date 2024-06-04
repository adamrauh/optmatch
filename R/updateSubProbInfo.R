# updating the subprobinfo table at the individual subproblem level
# assume that all arguments passed via ... are named
updateSubProbInfoMetadata <- function(temp,
                                      dm,
                                      ...)
{
  temp[["MCFSolution"]]@subproblems[1L, "exceedance"]  <- temp$maxerr
  temp[["MCFSolution"]]@subproblems[1L, "feasible"]  <- any(temp$solutions==1L)

  ## Presently we can treat this subproblem as non-flipped even if it was,
  ## since `dm` will have been transposed in the event of flipping.  Doing
  ## so will prevent the evaluate_* routines below from getting confused.
  ## Of course we have to remember to set it based on actual information as
  ## the MCFsolutions object passes up through the point in the call stack
  ## where that transposition was made.
  temp[["MCFSolution"]]@subproblems[1L, "flipped"]  <- FALSE
  ## ... and now we can proceed with:
  evaluate_lagrangian(dm, temp[["MCFSolution"]]) ->
    temp[["MCFSolution"]]@subproblems[1L, "lagrangian_value"]
  evaluate_dual(dm, temp[["MCFSolution"]]) ->
    temp[["MCFSolution"]]@subproblems[1L,   "dual_value"]
  evaluate_primal(dm, temp[["MCFSolution"]]) ->
    temp[["MCFSolution"]]@subproblems[1L,   "primal_value"]

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
  temp[["MCFSolution"]]@subproblems <-
    new("SubProbInfo", cbind(temp[["MCFSolution"]]@subproblems, otherParams))
  return(temp)
}
