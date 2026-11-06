# Source this file and call install_scorch(), or assemble with:
# Rscript software/setup/install.R .package

prepare_scorch_package <- function(root = ".", destination = tempfile("scoRch-package-")) {
  software <- normalizePath(file.path(root, "software"), winslash = "/", mustWork = TRUE)
  inputs <- c(
    R = file.path(software, "scripts"), man = file.path(software, "help"),
    "man/figures" = file.path(software, "figures"),
    tests = file.path(software, "tests"), DESCRIPTION = file.path(software, "setup", "DESCRIPTION"),
    NAMESPACE = file.path(software, "setup", "NAMESPACE"),
    "inst/config.yaml" = file.path(root, "config.yaml"),
    "inst/extdata/scorch.cff" = file.path(root, "CITATION.cff"),
    "inst/extdata/north_america_tmax.nc" = file.path(root, "demo", "input", "north_america_processed_tmax.nc"),
    "inst/extdata/PROVENANCE.txt" = file.path(root, "demo", "PROVENANCE.txt")
  )
  if (!all(file.exists(inputs))) stop("The repository is missing required software files.", call. = FALSE)
  parent <- normalizePath(dirname(destination), winslash = "/", mustWork = TRUE)
  destination <- file.path(parent, basename(destination))
  if (startsWith(paste0(destination, "/"), paste0(software, "/"))) {
    stop("The package destination must be outside software/.", call. = FALSE)
  }
  if (file.exists(destination)) stop("Choose a new, empty package destination.", call. = FALSE)
  if (!dir.create(destination)) stop("Could not create the package destination.", call. = FALSE)
  for (name in names(inputs)) {
    from <- inputs[[name]]
    to <- file.path(destination, name)
    folder <- dir.exists(from)
    dir.create(if (folder) to else dirname(to), recursive = TRUE, showWarnings = FALSE)
    if (folder) from <- list.files(from, all.files = TRUE, no.. = TRUE, full.names = TRUE)
    if (length(from) && !all(file.copy(from, to, recursive = folder, copy.mode = TRUE))) {
      stop("Could not copy package input to ", to, ".", call. = FALSE)
    }
  }
  writeLines("scoRch.simulator:::scorch_citation(meta)", file.path(destination, "inst", "CITATION"))
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
  if (length(args) != 1L) stop("Use: Rscript software/setup/install.R <new_package_folder>", call. = FALSE)
  cat(prepare_scorch_package(destination = args[[1L]]), "\n", sep = "")
}
