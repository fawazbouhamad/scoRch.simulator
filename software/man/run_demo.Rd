% Generated from R/18_demo.R; documentation kept with the exported entry point.
\name{run_demo}
\alias{run_demo}
\title{Run the US demonstration}
\usage{run_demo(config_file = "prepared", input = NULL, output = NULL)}
\arguments{
\item{config_file}{"prepared" (default), "hourly", or a custom YAML path.}
\item{input}{Optional prepared NetCDF file or hourly file/folder. Required for hourly input.}
\item{output}{Optional destination; defaults to output/ or output/hourly/.}
}
\description{Runs the installed prepared example without cloning the repository or editing YAML. Both profiles retain the study settings and representative events. The full hourly archive is distributed separately when available.}
\value{The same invisible result as \code{run_scorch()}.}
\seealso{\code{\link{run_scorch}}, \code{\link{write_scorch_config}}}
\examples{
\dontrun{
run_demo()
run_demo("hourly", input = "path/to/hourly_folder")
}
}
