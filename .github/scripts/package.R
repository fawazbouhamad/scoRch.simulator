# Assemble the readable repository folders into a standard R source package.
# Rscript .github/scripts/package.R .package
# Or source this file and call install_scorch() to install from GitHub.

prepare_scorch_package <- function(root = ".", destination = tempfile("scoRch-package-")) {
  source <- normalizePath(file.path(root, "software"), winslash = "/", mustWork = TRUE)
  folders <- c(scripts = "R", helpers = "man", supporting_files = "inst", tests = "tests")
  required <- c(file.path(source, names(folders)), file.path(source, c("DESCRIPTION", "NAMESPACE")))
  if (!all(file.exists(required))) stop("The repository is missing required software files.", call. = FALSE)
  parent <- normalizePath(dirname(destination), winslash = "/", mustWork = TRUE)
  destination <- file.path(parent, basename(destination))
  if (startsWith(paste0(destination, "/"), paste0(source, "/"))) {
    stop("The package destination must be outside software/.", call. = FALSE)
  }
  if (file.exists(destination)) stop("Choose a new, empty package destination.", call. = FALSE)
  if (!dir.create(destination)) stop("Could not create the package destination.", call. = FALSE)
  items <- list.files(source, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  for (item in items) {
    name <- basename(item)
    if (dir.exists(item) && !name %in% names(folders)) {
      stop("Unrecognized software folder: ", name, call. = FALSE)
    }
    target <- if (name %in% names(folders)) unname(folders[[name]]) else name
    if (dir.exists(item)) {
      # Copy into a temporary standard name; the repository stays unchanged.
      target <- file.path(destination, target)
      dir.create(target)
      children <- list.files(item, all.files = TRUE, no.. = TRUE, full.names = TRUE)
      if (length(children) && !all(file.copy(children, target, recursive = TRUE, copy.mode = TRUE))) {
        stop("Could not copy ", name, " into the package.", call. = FALSE)
      }
    } else if (!file.copy(item, file.path(destination, target), copy.mode = TRUE)) {
      stop("Could not copy ", name, " into the package.", call. = FALSE)
    }
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
  if (length(args) != 1L) stop("Use: Rscript .github/scripts/package.R <new_package_folder>", call. = FALSE)
  cat(prepare_scorch_package(destination = args[[1L]]), "\n", sep = "")
}
