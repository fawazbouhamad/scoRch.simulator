# Assemble the readable repository folders into a standard R source package.
# Rscript software/install.R .package
# Or source this file and call install_scorch() to install from GitHub.

prepare_scorch_package <- function(root = ".", destination = tempfile("scoRch-package-")) {
  source <- normalizePath(file.path(root, "software"), winslash = "/", mustWork = TRUE)
  folders <- c(scripts = "R", helpers = "man", tests = "tests")
  files <- c(DESCRIPTION = "DESCRIPTION", NAMESPACE = "NAMESPACE",
             CITATION = "inst/CITATION")
  configuration <- file.path(root, "config.yaml")
  example <- file.path(root, "demo", "input", "north_america_processed_tmax.nc")
  provenance <- file.path(root, "demo", "PROVENANCE.txt")
  required <- c(file.path(source, c(names(folders), names(files))), configuration, example, provenance)
  if (!all(file.exists(required))) stop("The repository is missing required software files.", call. = FALSE)
  parent <- normalizePath(dirname(destination), winslash = "/", mustWork = TRUE)
  destination <- file.path(parent, basename(destination))
  if (startsWith(paste0(destination, "/"), paste0(source, "/"))) {
    stop("The package destination must be outside software/.", call. = FALSE)
  }
  if (file.exists(destination)) stop("Choose a new, empty package destination.", call. = FALSE)
  if (!dir.create(destination)) stop("Could not create the package destination.", call. = FALSE)
  # Copy only package contents. Maintenance scripts and Git settings stay
  # in the repository; installation metadata takes its standard R location.
  for (name in names(folders)) {
    target <- file.path(destination, folders[[name]])
    dir.create(target, recursive = TRUE)
    children <- list.files(file.path(source, name), all.files = TRUE, no.. = TRUE, full.names = TRUE)
    if (length(children) && !all(file.copy(children, target, recursive = TRUE, copy.mode = TRUE))) {
      stop("Could not copy ", name, " into the package.", call. = FALSE)
    }
  }
  if (!dir.create(file.path(destination, "inst", "extdata"), recursive = TRUE, showWarnings = FALSE)) {
    stop("Could not create the package data folder.", call. = FALSE)
  }
  for (name in names(files)) {
    if (!file.copy(file.path(source, name), file.path(destination, files[[name]]), copy.mode = TRUE)) {
      stop("Could not copy ", name, " into the package.", call. = FALSE)
    }
  }
  if (!file.copy(configuration, file.path(destination, "inst", "config.yaml"), copy.mode = TRUE)) {
    stop("Could not include config.yaml in the package.", call. = FALSE)
  }
  # Keep the installed filename stable for existing R examples and scripts.
  if (!file.copy(example, file.path(destination, "inst", "extdata", "north_america_tmax.nc"), copy.mode = TRUE)) {
    stop("Could not include the prepared demo input.", call. = FALSE)
  }
  if (!file.copy(provenance, file.path(destination, "inst", "extdata", "PROVENANCE.txt"), copy.mode = TRUE)) {
    stop("Could not include the demo data attribution.", call. = FALSE)
  }
  destination
}

install_scorch <- function(path = NULL, ref = "main", lib = NULL, upgrade = "never", ...) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    stop('First run install.packages("remotes").', call. = FALSE)
  }
  temporary <- tempfile("scoRch-install-")
  dir.create(temporary)
  on.exit(unlink(temporary, recursive = TRUE), add = TRUE)
  if (is.null(path)) {
    archive <- file.path(temporary, "repository.zip")
    url <- paste0("https://github.com/fawazbouhamad/scoRch.simulator/archive/",
                  utils::URLencode(ref, reserved = TRUE), ".zip")
    utils::download.file(url, archive, mode = "wb")
    entries <- utils::unzip(archive, list = TRUE)$Name
    if (any(grepl("(^/|^[A-Za-z]:|(^|/)\\.\\.(/|$)|\\\\)", entries))) {
      stop("The source archive contains an invalid path.", call. = FALSE)
    }
    roots <- unique(sub("/.*$", "", entries))
    if (length(roots) != 1L) stop("The source archive must contain one repository.", call. = FALSE)
    utils::unzip(archive, exdir = temporary)
    path <- file.path(temporary, roots)
  }
  package <- prepare_scorch_package(path, file.path(temporary, "scoRch.simulator"))
  remotes::install_local(package, lib = lib, upgrade = upgrade, build = FALSE, ...)
  invisible(NULL)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 1L) stop("Use: Rscript software/install.R <new_package_folder>", call. = FALSE)
  cat(prepare_scorch_package(destination = args[[1L]]), "\n", sep = "")
}
