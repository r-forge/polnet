# The functions contained in this file serve to compute regulatory independence 
# as described in:
# 
# Ingold, Karin, Frédéric Varone and Frans Stokman (2013): A Social Network 
# Based Approach to Assess De Facto Independence of Regulatory Agencies.
# Journal of European Public Policy.
# 
# Code written by: Philip Leifeld

regindep <- function(influence, reputation) {
  
  # check whether 'influence' is valid input (first step)
  if (is.data.frame(influence)) {
    influence <- as.matrix(influence)
  }
  if (!is.matrix(influence) && class(influence) != "network") {
    stop("'influence' must be a matrix object or network object.")
  } else if (class(influence) == "network") {
    require(network)
    influence <- as.sociomatrix(influence)
  }
  if (nrow(influence) != ncol(influence)) {
    stop(paste("'influence' must be a square matrix (i.e., same number of", 
        "rows and columns)."))
  }
  if (any(is.na(influence)) || any(is.nan(influence)) || 
      any(is.infinite(influence))) {
    stop("NA, NaN, and Inf values are not allowed.")
  }
  if (any(rownames(influence) != colnames(influence))) {
    colnames(influence) <- rownames(influence)
    warning(paste("Row and column names of the influence matrix are not", 
        "identical. Replacing column labels by row labels."))
  }
  if (any(diag(influence) < 0) || any(diag(influence) == 0)) {
    diag(influence) <- rep(1, nrow(influence))
    warning(paste("There are zeros or negative elements in the diagonal of the",
        "influence matrix. Replacing diagonal with ones."))
  }
  
  # check whether 'reputation' is valid input
  if (is.data.frame(reputation)) {
    reputation <- as.matrix(reputation)
  }
  if (!is.matrix(reputation) && class(reputation) != "network" && 
      !is.numeric(reputation)) {
    stop(paste("'reputation' must be a matrix object, network object, or a", 
        "numeric vector."))
  } else if (class(reputation) == "network") {
    require(network)
    reputation <- as.sociomatrix(reputation)
  } else if (is.matrix(reputation)) {
    if (nrow(reputation) != ncol(reputation)) {
      stop(paste("'reputation' must be a square matrix (i.e., same number of", 
          "rows and columns), or a numeric vector."))
    }
    if (any(rownames(reputation) != colnames(reputation))) {
      colnames(reputation) <- rownames(reputation)
      warning(paste("Row and column names of the reputation matrix are not", 
          "identical. Replacing column labels by row labels."))
    }
    if (any(diag(reputation) < 0) || any(diag(reputation) == 0)) {
      warning(paste("There are zeros or negative elements in the diagonal of", 
          "the reputation matrix. Proceeding anyhow."))
    }
    reputation <- colSums(reputation) / nrow(reputation)  # second step
  } else {
    if (length(reputation) != nrow(influence)) {
      stop("'influence' and 'reputation' must have the same number of actors.")
    }
  }
  if (any(is.na(reputation)) || any(is.nan(reputation)) || 
      any(is.infinite(reputation))) {
    stop("NA, NaN, and Inf values are not allowed.")
  }
  
  infmat <- influence * rep(reputation, nrow(influence))  # third step
  
  cs <- rep(colSums(infmat), nrow(infmat))
  cs <- matrix(cs, nrow = nrow(infmat), byrow = TRUE)
  weightmat <- infmat / cs  # fourth step
  
  consmat <- weightmat
  diag(consmat) <- rep(0, nrow(consmat))
  relresources <- diag(weightmat)
  constraints <- colSums(consmat)
  impacts <- rowSums(consmat)
  
  output <- list(weight.matrix = weightmat, resources = relresources, 
      constraint = constraints, impact = impacts)
  class(output) <- "regindep"
  
  return(output)
}

print.regindep <- function(x, ...) {
  res <- format(x$resources, digits = 1, nsmall = 4)
  con <- format(x$constraint, digits = 1, nsmall = 4)
  imp <- format(x$impact, digits = 1, nsmall = 4)
  nam <- names(res)
  width <- max(nchar(nam)) + 2
  spaces <- paste(rep(" ", width), collapse="")
  nam <- format(nam, width = width)
  cat(paste0(spaces, "Resources  Impact  Constraint\n"))
  for (i in 1:length(res)) {
    cat(paste0(nam[i], format(res[i], width = 9), "  ", imp[i], "  ", con[i], 
        "\n"))
  }
}

summary.regindep <- function(object, ...) {
  res <- format(object$resources, digits = 1, nsmall = 4)
  con <- format(object$constraint, digits = 1, nsmall = 4)
  imp <- format(object$impact, digits = 1, nsmall = 4)
  nam <- names(res)
  width <- max(nchar(nam)) + 2
  spaces <- paste(rep(" ", width), collapse="")
  nam <- format(nam, width = width)
  cat(paste0(spaces, "Resources  Impact  Constraint\n"))
  for (i in 1:length(res)) {
    cat(paste0(nam[i], format(res[i], width = 9), "  ", imp[i], "  ", con[i], 
        "\n"))
  }
}

