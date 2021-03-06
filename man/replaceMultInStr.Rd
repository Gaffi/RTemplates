% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/replaceMultInStr.r
\name{replaceMultInStr}
\alias{replaceMultInStr}
\title{Replace Multiple Variables in a Large String}
\usage{
replaceMultInStr(text, replacements)
}
\arguments{
\item{text}{A character string of the text to find replacements in.}

\item{replacements}{A named vector/list of each variable/placeholder and it's respective replacement.}
}
\value{
A character string containing the original \code{text} with all the specified replacements made.
}
\description{
Replaces a series of variable placeholders with specified string replacements.
}
\details{

}
\examples{
#Simple replacement
replaceMultInStr('This is a &Var1 of some &Var2.',c('&Var1' = 'replacement', '&Var2' = 'text'))
# Outputs: "This is a replacement of some text."

#Placeholders need to be distinct. This doesn't work:
replaceMultInStr('This is a &Var1 of some &Var10.',c('&Var1' = 'replacement', '&Var10' = 'text'))
# Outputs: "This is a replacement of some replacement0."

}
\seealso{
\code{\link{gsub}}
}
\keyword{methods}
