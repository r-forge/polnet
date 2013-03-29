\name{regindep}
\alias{regindep}
\title{Compute the regulatory independence of political actors}
\description{Compute the regulatory independence of political actors.}
\usage{
regindep(influence, reputation)
}
\details{
The \code{regindep} function computes the regulatory independence of 
actors in a policy domain, as defined in the JEPP article of Ingold, 
Varone and Stokman (2013).
}
\arguments{
\item{influence}{ Influence network. A tie indicates that an actor exerts an influence on another actor. The data can be provided either as a \code{network} object as defined in the \pkg{network} package, or it can be provided as a (square and numeric) matrix where row actors influence column actors. Note that the diagonal should consist of 1s and that row and column labels should be equal. }
\item{reputation}{ Reputation network or, alternatively, a vector of reputation scores. In case a network is provided, a tie indicates that an actor deems another actor influential. The network can be provided either as a \code{network} object as defined in the \pkg{network} package, or it can be provided as a (square and numeric) matrix where row actors indicate which column actors they find influential. Note that row and column labels should be equal, and that they should correspond to the labels of the influence matrix. In case a numeric vector is provided, the values represent the aggregated reputation scores of the actors (as in figure 1 in the original article). }
}
\references{
Ingold, Karin, Frederic Varone and Frans Stokman (2013).
 A Social Network Based Approach to Assess De Facto Independence of 
 Regulatory Agencies. \emph{Journal of European Public Policy}. Forthcoming.
}
\seealso{
\code{\link{polnet-package}}
}
\author{
Philip Leifeld (\url{http://www.philipleifeld.de})
}
\examples{
infl <- matrix(c(
    1, 1, 0, 1, 0, 
    1, 1, 1, 1, 0, 
    1, 0, 1, 1, 1, 
    0, 1, 0, 1, 1, 
    1, 1, 1, 0, 1
    ), nrow = 5, byrow = TRUE
)
rownames(infl) <- LETTERS[1:5]
colnames(infl) <- LETTERS[1:5]

repu <- c(A = 0.8, B = 0.6, C = 0.5, D = 0.4, E = 0.2)

output <- regindep(infl, repu)

output$weight.matrix  # show the weight matrix
output$impact         # show the impact scores
output                # show a summary of the output
}
