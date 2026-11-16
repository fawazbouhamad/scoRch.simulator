# Source this file and call install_scorch(), or assemble with:
# Rscript software/setup/install.R .package

prepare_scorch_package <- function(root = ".", destination = tempfile("scoRch-package-")) {
  software <- normalizePath(file.path(root, "software"), winslash = "/", mustWork = TRUE)
  inputs <- c(
    R = file.path(software, "scripts"), man = file.path(software, "help"),
    "man/figures" = file.path(software, "figures"),
    "inst/launcher" = file.path(software, "launcher"),
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

# Install once, then open the SCORCH window. The call asks no questions, so it
# can be the last line of a pasted block. Running it again with this release
# already installed downloads nothing and only opens the window.
#   path   a downloaded repository instead of a GitHub download
#   ref    release tag or branch to download
#   force  reinstall a release that is already installed
#   open   open the window afterwards (interactive sessions only)
install_scorch <- function(path = NULL, ref = "v1.3.0", lib = NULL, upgrade = "never", force = FALSE,
                           open = interactive(), ...) {
  pkg <- "scoRch.simulator"
  reopen <- "scoRch.simulator::run_scorch()"
  restart <- "Restart R (in RStudio: Session > Restart R)"
  # Nothing below may stop to ask: a question would take the next pasted line as its answer.
  repos <- getOption("repos")
  if (is.null(repos) || !"CRAN" %in% names(repos) || is.na(repos[["CRAN"]]) || identical(repos[["CRAN"]], "@CRAN@")) {
    repos <- c(CRAN = "https://cloud.r-project.org", repos[setdiff(names(repos), "CRAN")])
    options(repos = repos)
  }
  writable <- function(folder) {
    probe <- tempfile("scorch-write-", tmpdir = folder)
    ok <- dir.exists(folder) && suppressWarnings(dir.create(probe))
    unlink(probe, recursive = TRUE)
    ok
  }
  if (is.null(lib) && !writable(.libPaths()[1L])) {
    # A new R installation has no personal library yet; R would ask before creating it.
    personal <- path.expand(strsplit(Sys.getenv("R_LIBS_USER"), .Platform$path.sep, fixed = TRUE)[[1L]][1L])
    if (!is.na(personal) && nzchar(personal)) {
      dir.create(personal, recursive = TRUE, showWarnings = FALSE)
      if (writable(personal)) .libPaths(c(personal, .libPaths()))
    }
  }
  target <- normalizePath(if (is.null(lib)) .libPaths()[1L] else lib, winslash = "/", mustWork = TRUE)
  if (!writable(target)) stop("R cannot write to its package library: ", target, call. = FALSE)
  asked <- options(install.packages.compile.from.source = "never")
  on.exit(options(asked), add = TRUE)
  on_disk <- function() {
    found <- suppressWarnings(utils::packageDescription(pkg, lib.loc = target, fields = c("Version", "Built")))
    if (is.list(found)) c(Version = found$Version, Built = found$Built) else c(Version = NA_character_, Built = NA_character_)
  }
  loaded <- function() if (pkg %in% loadedNamespaces()) unname(getNamespaceVersion(pkg)) else NA_character_
  finish <- function(done) {
    message(done, "\nYou do not need to run the installation block again. From now on, open SCORCH with:\n  ", reopen)
    if (isTRUE(open) && interactive()) {
      return(invisible(getExportedValue(loadNamespace(pkg, lib.loc = target), "run_scorch")()))
    }
    invisible(NULL)
  }

  before <- on_disk()
  if (is.null(path) && !isTRUE(force) && identical(ref, paste0("v", before[["Version"]])) &&
      loaded() %in% c(NA_character_, before[["Version"]])) {
    return(finish(sprintf("%s %s is already installed. Nothing was downloaded or changed.", pkg, before[["Version"]])))
  }

  # 1. Fetch and assemble first: a failed download leaves everything as it was.
  temporary <- tempfile("scoRch-install-")
  dir.create(temporary)
  on.exit(unlink(temporary, recursive = TRUE), add = TRUE)
  if (is.null(path)) {
    archive <- file.path(temporary, "repository.zip")
    url <- paste0("https://github.com/fawazbouhamad/scoRch.simulator/archive/",
                  utils::URLencode(ref, reserved = TRUE), ".zip")
    message("Downloading ", pkg, " ", ref, " with its example data (about 100 MB). This can take a few minutes.")
    # The default limit of 60 seconds is too short for this file on many connections.
    limit <- options(timeout = max(3600, getOption("timeout")))
    on.exit(options(limit), add = TRUE)
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
  package <- prepare_scorch_package(path, file.path(temporary, pkg))
  wanted <- unname(read.dcf(file.path(package, "DESCRIPTION"), "Version")[1L, 1L])

  # 2. Packages this one needs, before anything of an earlier installation is
  # touched. Installed ones are left alone unless `upgrade` says otherwise.
  if (!requireNamespace("remotes", quietly = TRUE)) utils::install.packages("remotes", lib = target)
  if (!requireNamespace("remotes", quietly = TRUE)) {
    stop("The helper package 'remotes' could not be installed. Check the internet connection, then run the ",
         "installation block again.", call. = FALSE)
  }
  message("Installing ", pkg, " ", wanted, " and any packages it still needs.")
  remotes::install_deps(package, dependencies = NA, upgrade = upgrade, lib = lib, ...)
  imports <- trimws(strsplit(read.dcf(file.path(package, "DESCRIPTION"), "Imports")[1L, 1L], ",")[[1L]])
  needs <- data.frame(name = sub("[[:space:]]*[(].*$", "", imports),
                      least = ifelse(grepl(">=", imports, fixed = TRUE),
                                     sub("^.*>=[[:space:]]*([^)[:space:]]+).*$", "\\1", imports), NA_character_),
                      stringsAsFactors = FALSE)
  installed <- function() vapply(needs$name, function(name) {
    version <- suppressWarnings(utils::packageDescription(name, lib.loc = unique(c(target, .libPaths())), fields = "Version"))
    if (is.character(version) && !is.na(version)) version else NA_character_
  }, "")
  older <- function(have) !is.na(have) & !is.na(needs$least) &
    mapply(function(a, b) !is.na(a) && !is.na(b) && utils::compareVersion(a, b) < 0, have, needs$least)
  have <- installed()
  if (any(older(have))) {
    utils::install.packages(needs$name[older(have)], lib = target)
    have <- installed()
  }
  if (anyNA(have)) {
    stop(pkg, " was not changed. These packages it needs could not be installed: ",
         paste(needs$name[is.na(have)], collapse = ", "),
         ".\nCheck the internet connection, then run the installation block again.", call. = FALSE)
  }
  if (any(older(have))) {
    stop(pkg, " was not changed. It needs newer versions of: ",
         paste(sprintf("%s (at least %s, installed %s)", needs$name, needs$least, have)[older(have)], collapse = ", "),
         ".\n", restart, ", update them with install.packages(), then run the installation block again.", call. = FALSE)
  }

  # 3. Let go of a copy this session has loaded; R cannot replace code in use.
  previous <- loaded()
  attached <- paste0("package:", pkg) %in% search()
  if (!is.na(previous)) {
    users <- getNamespaceUsers(pkg)
    if (length(users)) {
      stop(pkg, " ", previous, " is in use by ", paste(users, collapse = ", "), " in this R session and was not changed.\n",
           restart, ", then run the installation block again.", call. = FALSE)
    }
    # Reading every function once keeps the later reload free of warnings.
    try(suppressWarnings(eapply(asNamespace(pkg), force, all.names = TRUE)), silent = TRUE)
    released <- tryCatch({ unloadNamespace(pkg); TRUE }, error = function(e) conditionMessage(e))
    if (!isTRUE(released)) {
      stop(pkg, " ", previous, " is in use in this R session and was not changed: ", released, "\n",
           restart, ", then run the installation block again.", call. = FALSE)
    }
  }
  done <- FALSE
  on.exit(if (!done && attached && !is.na(on_disk()[["Version"]])) {
    try(suppressPackageStartupMessages(library(pkg, character.only = TRUE, lib.loc = target)), silent = TRUE)
  }, add = TRUE)

  # 4. Install. R reports a failed installation only as a warning, so look at the result.
  notes <- character()
  withCallingHandlers(
    remotes::install_local(package, lib = target, dependencies = FALSE, upgrade = "never", build = FALSE, force = TRUE),
    warning = function(w) {
      notes <<- c(notes, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  after <- on_disk()
  if (!identical(after[["Version"]], wanted) || identical(after[["Built"]], before[["Built"]])) {
    stop(pkg, " ", wanted, " was NOT installed. ",
         if (is.na(after[["Version"]])) "No version is installed now."
         else paste0("Version ", after[["Version"]], " is still in place."),
         if (length(notes)) paste0("\nR reported: ", paste(notes, collapse = "; ")),
         "\nRead the messages above, then run the installation block again.", call. = FALSE)
  }

  # 5. Use the new copy in this session, so no restart is needed.
  quiet <- function(w) if (grepl("R_decompress|restarting interrupted promise", conditionMessage(w))) invokeRestart("muffleWarning")
  namespace <- tryCatch(withCallingHandlers(loadNamespace(pkg, lib.loc = target), warning = quiet), error = identity)
  current <- !inherits(namespace, "error") && identical(unname(getNamespaceVersion(namespace)), wanted) &&
    identical(tolower(normalizePath(getNamespaceInfo(namespace, "path"), winslash = "/")), tolower(file.path(target, pkg)))
  if (!current) {
    done <- TRUE
    message(pkg, " ", wanted, " is installed, but this R session cannot use it yet.\n",
            "One step remains, once: ", restart, ". Then run:\n  ", reopen,
            "\nDo not run the installation block again. The new version is already installed.")
    return(invisible(NULL))
  }
  if (attached) suppressPackageStartupMessages(library(pkg, character.only = TRUE, lib.loc = target))
  done <- TRUE
  finish(paste0(
    if (is.na(before[["Version"]]) && is.na(previous)) sprintf("Installed %s %s.", pkg, wanted)
    else if (identical(before[["Version"]], wanted)) sprintf("Reinstalled %s %s.", pkg, wanted)
    else sprintf("Updated %s from %s to %s.", pkg, if (is.na(before[["Version"]])) previous else before[["Version"]], wanted),
    if (!is.na(previous)) " The copy this session had loaded was replaced; no restart is needed."))
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 1L) stop("Use: Rscript software/setup/install.R <new_package_folder>", call. = FALSE)
  cat(prepare_scorch_package(destination = args[[1L]]), "\n", sep = "")
}
