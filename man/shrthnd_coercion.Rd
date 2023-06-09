% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coercion.R
\name{shrthnd_coercion}
\alias{shrthnd_coercion}
\title{Coercion of shrthnd_num vectors}
\description{
As an extension of the \code{{vectrs}} package, a \code{shrthnd_num()} is generally
coerced to behave as if it was a regular a \code{numeric()} vector. Where
\code{{vectrs}} doesn't automatically support coercion custom methods are
provided to enable a \code{shrthnd_num()} to be considered as a numeric vector.
}
\section{General principles}{
The principles underpinning the coercion of a \code{shrthnd_num()} vector are
that to maximise compatability with base R and other packages, the vector
should generally behave as a numeric vector. This means that
\code{as.numeric()} will produce a bare numeric vector containing just the
numeric component of a \code{shrthnd_num()}. Similarly \code{as.character()} will
produce a character vector of the numeric component of a \code{shrthnd_num()}.
To work with tags use \code{shrthnd_tags()} and the related
\link[=tag_match]{tag location} functions. To produce a traditional
character vector combining the numeric component and tag component use
\code{as_shrthnd()} on a \code{shrthnd_num()} vector.

In keeping with base R practice around complex numeric objects such as
\code{Date()}, \code{difftime()} and \code{POSIXct()}, using \code{is.numeric()} on a
\code{shrthnd_num()} vector will return \code{FALSE}. Use \code{is_shrthnd_num()} to test
if a vector is a \code{shrthnd_num()} vector.

See \code{\link{shrthnd_maths}} for details on how \code{shrthnd_num()} works with
arithmetic, mathematical and (some) statistical operations.
}

\section{Missing values}{
Of particular note is that using \code{is.na()} on a \code{shrthnd_num()} vector is
designed to work on the numeric component, i.e. if numeric component is
missing but a tag marker is present then \code{is.na()} will return \code{TRUE}. Use
\code{is_na_tag()} to identify where there is no tag marker, or \code{is_na_both()} to
identify where both the numeric and tag components are missing.
}

\examples{
x <- c("12", "34.567", "[c]", "NA", "56.78[e]", "78.9", "90.123[e]")
sh_x <- shrthnd_num(x, c("[c]", "[e]"))

as.numeric(sh_x)

as.character(sh_x)

is.na(sh_x)


}
\seealso{

\code{\link{as_shrthnd}()},
\code{\link{is_shrthnd_num}()},
\code{\link{make_shrthnd_num}()},
\code{\link{shrthnd_maths}},
\code{\link{shrthnd_num}()},
\code{\link{shrthnd_tags}()},
\code{\link{tag_match}()}
}
\concept{num}
