% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hdr.R, R/hdr_lines.R
\docType{data}
\name{geom_hdr}
\alias{geom_hdr}
\alias{stat_hdr}
\alias{StatHdr}
\alias{GeomHdr}
\alias{stat_hdr_lines}
\alias{StatHdrLines}
\alias{geom_hdr_lines}
\alias{GeomHdrLines}
\title{Highest density regions of a 2D density estimate}
\usage{
stat_hdr(
  mapping = NULL,
  data = NULL,
  geom = "hdr",
  position = "identity",
  ...,
  method = "kde",
  probs = c(0.99, 0.95, 0.8, 0.5),
  n = 100,
  xlim = NULL,
  ylim = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_hdr(
  mapping = NULL,
  data = NULL,
  stat = "hdr",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}}. If specified and
\code{inherit.aes = TRUE} (the default), it is combined with the default mapping
at the top level of the plot. You must supply \code{mapping} if there is no plot
mapping.}

\item{data}{The data to be displayed in this layer. There are three
options:

If \code{NULL}, the default, the data is inherited from the plot
data as specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.

A \code{data.frame}, or other object, will override the plot
data. All objects will be fortified to produce a data frame. See
\code{\link[ggplot2:fortify]{fortify()}} for which variables will be created.

A \code{function} will be called with a single argument,
the plot data. The return value must be a \code{data.frame}, and
will be used as the layer data. A \code{function} can be created
from a \code{formula} (e.g. \code{~ head(.x, 10)}).}

\item{geom}{The geometric object to use to display the data, either as a
\code{ggproto} \code{Geom} subclass or as a string naming the geom stripped of the
\code{geom_} prefix (e.g. \code{"point"} rather than \code{"geom_point"})}

\item{position}{Position adjustment, either as a string naming the adjustment
(e.g. \code{"jitter"} to use \code{position_jitter}), or the result of a call to a
position adjustment function. Use the latter if you need to change the
settings of the adjustment.}

\item{...}{Other arguments passed on to \code{\link[ggplot2:layer]{layer()}}. These are
often aesthetics, used to set an aesthetic to a fixed value, like
\code{colour = "red"} or \code{size = 3}. They may also be parameters
to the paired geom/stat.}

\item{method}{Density estimator to use, accepts character vector:
\code{"kde"},\code{"histogram"}, \code{"freqpoly"}, or \code{"mvnorm"}.
Alternatively accepts functions  which return closures corresponding to density estimates,
see \code{?get_hdr} or \code{vignette("method", "ggdensity")}.}

\item{probs}{Probabilities to compute highest density regions for.}

\item{n}{Resolution of grid defined by \code{xlim} and \code{ylim}.
Ignored if \code{method = "histogram"} or \code{method = "freqpoly"}.}

\item{xlim, ylim}{Range to compute and draw regions. If \code{NULL}, defaults to
range of data.}

\item{na.rm}{If \code{FALSE}, the default, missing values are removed with
a warning. If \code{TRUE}, missing values are silently removed.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}

\item{stat}{The statistical transformation to use on the data for this
layer, either as a \code{ggproto} \code{Geom} subclass or as a string naming the
stat stripped of the \code{stat_} prefix (e.g. \code{"count"} rather than
\code{"stat_count"})}
}
\description{
Perform 2D density estimation, compute and plot the resulting highest density regions.
\code{geom_hdr()} draws filled regions and \code{geom_hdr_lines()} draws lines outlining the regions.
Note, the plotted objects have probabilities mapped to the \code{alpha} aesthetic by default.
}
\section{Aesthetics}{
 \code{geom_hdr()} and \code{geom_hdr_lines()} understand the following aesthetics (required
aesthetics are in bold):
\itemize{
\item \strong{x}
\item \strong{y}
\item alpha
\item color
\item fill (only \code{geom_hdr})
\item group
\item linetype
\item linewidth
\item subgroup
}
}

\section{Computed variables}{


\describe{ \item{probs}{The probability associated with the highest density region, specified
by \code{probs} argument.} }
}

\examples{
# Basic simulated data with bivariate normal data and various methods
df <- data.frame(x = rnorm(1000), y = rnorm(1000))
p <- ggplot(df, aes(x, y)) + coord_equal()

p + geom_hdr()
p + geom_hdr(method = "mvnorm")
p + geom_hdr(method = "freqpoly")
# p + geom_hdr(method = "histogram")

# Adding point layers on top to visually assess region estimates
pts <- geom_point(size = .2, color = "red")

p + geom_hdr() + pts
p + geom_hdr(method = "mvnorm") + pts
p + geom_hdr(method = "freqpoly") + pts
# p + geom_hdr(method = "histogram") + pts

# Highest density region boundary lines
p + geom_hdr_lines()
p + geom_hdr_lines(method = "mvnorm")
p + geom_hdr_lines(method = "freqpoly")
# p + geom_hdr_lines(method = "histogram")

\dontrun{

# 2+ groups - mapping other aesthetics in the geom
rdata <- function(n, n_groups = 3, radius = 3) {
  list_of_dfs <- lapply(0:(n_groups-1), function(k) {
    mu <- c(cos(2*k*pi/n_groups), sin(2*k*pi/n_groups))
    m <- MASS::mvrnorm(n, radius*mu, diag(2))
    structure(data.frame(m, as.character(k)), names = c("x", "y", "c"))
  })
  do.call("rbind", list_of_dfs)
}

dfc <- rdata(1000, n_groups = 5)
pf <- ggplot(dfc, aes(x, y, fill = c)) + coord_equal()

pf + geom_hdr()
pf + geom_hdr(method = "mvnorm")
pf + geom_hdr(method = "mvnorm", probs = .90, alpha = .5)
pf + geom_hdr(method = "histogram")
pf + geom_hdr(method = "freqpoly")

pc <- ggplot(dfc, aes(x, y, color = c)) +
 coord_equal() +
 theme_minimal() +
 theme(panel.grid.minor = element_blank())

pc + geom_hdr_lines()
pc + geom_hdr_lines(method = "mvnorm")


# Data with boundaries
ggplot(df, aes(x^2)) + geom_histogram(bins = 30)
ggplot(df, aes(x^2)) + geom_histogram(bins = 30, boundary = 0)
ggplot(df, aes(x^2, y^2)) + geom_hdr(method = "histogram")

}

}
\references{
Scott, David W. Multivariate Density Estimation (2e), Wiley.
}
\keyword{datasets}
