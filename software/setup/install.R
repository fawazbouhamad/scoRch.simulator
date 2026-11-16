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
#   ref    release tag, commit or branch to download
#   lib    library folder to install into instead of R's first one
#   force  reinstall a release that is already installed
#   open   open the window afterwards (interactive sessions only)
install_scorch <- function(path = NULL, ref = "v1.3.0", lib = NULL, upgrade = "never", force = FALSE,
                           open = interactive(), ...) {
  pkg <- "scoRch.simulator"
  reopen <- "scoRch.simulator::run_scorch()"
  restart <- "Restart R (in RStudio: Session > Restart R)"
  again <- "then run the installation block again."
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
  # A library that R does not search by itself comes first while this call
  # runs, so that what is installed there is found. The last message says how
  # later sessions reach it.
  fold <- if (.Platform$OS.type == "windows") tolower else identity
  searched <- .libPaths()
  aside <- ""
  if (!fold(target) %in% fold(searched)) {
    .libPaths(c(target, searched))
    on.exit(.libPaths(searched), add = TRUE)
    aside <- paste0("\nR does not search this library by itself. In this and in every new R session, run this line first:\n  ",
                    ".libPaths(c(\"", target, "\", .libPaths()))")
  }
  asked <- options(install.packages.compile.from.source = "never")
  on.exit(options(asked), add = TRUE)
  # Where an installed copy came from: a release tag or commit that was
  # downloaded, or "local" for a folder. It is written into its DESCRIPTION.
  origin <- "Config/scoRch/source"
  on_disk <- function() {
    found <- suppressWarnings(utils::packageDescription(pkg, lib.loc = target, fields = c("Version", "Built", origin)))
    text <- function(value) if (is.character(value) && length(value) == 1L) value else NA_character_
    if (is.list(found)) c(Version = text(found$Version), Built = text(found$Built), Source = text(found[[origin]]))
    else c(Version = NA_character_, Built = NA_character_, Source = NA_character_)
  }
  loaded <- function() if (pkg %in% loadedNamespaces()) unname(getNamespaceVersion(pkg)) else NA_character_
  finish <- function(done) {
    opens <- isTRUE(open) && interactive()
    # The package reaches the others with `::`, so one that is damaged, or too
    # old for another, shows only when the window uses it. Loading each here
    # finds it before the window opens.
    for (name in if (opens) c(asks(utils::packageDescription(pkg, lib.loc = target, fields = "Imports"))$name, pkg)) {
      failed <- tryCatch({ loadNamespace(name, lib.loc = if (identical(name, pkg)) target); NULL }, error = identity)
      if (is.null(failed)) next
      # R names the package at fault in quotes. Anything else it quotes (a file,
      # an object) is not an installed package; the one being loaded is named then.
      reason <- conditionMessage(failed)
      named <- sub("^[^'\u2018]*['\u2018]([A-Za-z][A-Za-z0-9.]*)['\u2019].*$", "\\1", reason)
      # A library file or code file R could not load lies in the folder of the package it belongs to.
      inside <- regmatches(reason, regexpr(
        "[A-Za-z][A-Za-z0-9.]*(?=/(libs(/[^/]+)?|R)/[^/]+[.](dll|so|dylib|rdb|rdx)(?![A-Za-z0-9]))", reason, perl = TRUE))
      if (length(inside)) named <- inside
      if (!inherits(failed, "packageNotFoundError") && !nzchar(system.file(package = named))) named <- name
      stop(pkg, " is installed, but the SCORCH window cannot open: ",
           if (identical(named, pkg)) "it" else paste0("the package ", named, ", which it needs,"), " cannot be loaded.",
           "\nR reported: ", reason, "\n", restart, ", ",
           if (identical(named, pkg)) "then run the installation block with install_scorch(force = TRUE) as its last line."
           else paste0("run install.packages(\"", named, "\"), ", again), call. = FALSE)
    }
    message(done, "\nYou do not need to run the installation block again. From now on, open SCORCH with:\n  ", reopen, aside)
    if (opens) return(invisible(getExportedValue(loadNamespace(pkg, lib.loc = target), "run_scorch")()))
    invisible(NULL)
  }

  # What a DESCRIPTION field asks for, with the lowest version it accepts.
  asks <- function(field) {
    field <- as.character(field)
    entries <- trimws(unlist(strsplit(field[!is.na(field)], ",", fixed = TRUE)))
    entries <- entries[nzchar(entries) & !grepl("^R([[:space:]]*[(]|$)", entries)]
    data.frame(name = sub("[[:space:]]*[(].*$", "", entries),
               least = ifelse(grepl(">=", entries, fixed = TRUE),
                              sub("^.*>=[[:space:]]*([^)[:space:]]+).*$", "\\1", entries), NA_character_),
               stringsAsFactors = FALSE)
  }
  # The needed packages that are missing or older than asked for, with those
  # they need in turn. The DESCRIPTION files on disk are read and nothing is
  # loaded, so a package can still be replaced afterwards.
  short <- function(needs) {
    described <- function(name, fields) {
      suppressWarnings(utils::packageDescription(name, lib.loc = unique(c(target, .libPaths())), fields = fields))
    }
    queue <- needs$name
    while (length(queue)) {
      found <- described(queue[1L], c("Depends", "Imports"))
      further <- if (is.list(found)) setdiff(asks(c(found$Depends, found$Imports))$name, needs$name) else character()
      if (length(further)) needs <- rbind(needs, data.frame(name = further, least = NA_character_, stringsAsFactors = FALSE))
      queue <- c(queue[-1L], further)
    }
    needs$have <- vapply(needs$name, function(name) {
      version <- described(name, "Version")
      if (is.character(version) && !is.na(version)) version else NA_character_
    }, "", USE.NAMES = FALSE)
    needs$old <- vapply(seq_len(nrow(needs)), function(i) {
      !is.na(needs$have[i]) && !is.na(needs$least[i]) && utils::compareVersion(needs$have[i], needs$least[i]) < 0
    }, NA)
    needs[is.na(needs$have) | needs$old, , drop = FALSE]
  }
  outdated <- function() if (getRversion() < "4.4.0") {
    paste0("\nThis is R ", getRversion(), ", older than the versions SCORCH is checked on (R 4.4 and newer), and CRAN may ",
           "no longer provide these packages for it. Update R, ", again)
  }
  # What a reader can do when R could not install a package.
  why <- function() paste0(
    "R's messages above, and any warnings below, give the reason.", outdated(),
    if (identical(unname(Sys.info()["sysname"]), "Linux")) paste0(
      "\nOn Linux the usual cause is a missing system library. ncdf4 needs netCDF and fs needs libuv: install ",
      "libnetcdf-dev and libuv1-dev (Debian, Ubuntu) or netcdf-devel and libuv-devel (Fedora), ", again),
    "\nIf they report a failed download, check the internet connection, ", again)
  # Install what is missing or too old, and stop with the reason when something
  # still lacks. `tried`: R has just attempted the missing ones in its usual way.
  provide <- function(needs, tried = FALSE, reported = NULL) {
    added <- character()
    warned <- character()
    fetch <- function(names, ...) {
      added <<- union(added, names)
      # R's warnings are shown as always; they are only read here as well.
      tryCatch(withCallingHandlers(utils::install.packages(names, lib = target, ...),
                                   warning = function(w) warned <<- c(warned, conditionMessage(w))),
               error = function(e) reported <<- c(reported, conditionMessage(e)))
    }
    lacking <- short(needs)
    usual <- if (tried) lacking$name[lacking$old] else lacking$name
    if (length(usual)) {
      if (!tried) message(pkg, " needs packages that are missing or too old: ", paste(usual, collapse = ", "), ". Installing them.")
      fetch(usual)
      lacking <- short(needs)
    }
    if (nrow(lacking) && !identical(.Platform$pkgType, "source")) {
      # CRAN builds the packages for one version of R together. Where its
      # newest sources no longer suit an older R, those builds still fit each other.
      message("Trying the packages that CRAN built for this version of R: ", paste(lacking$name, collapse = ", "))
      fetch(lacking$name, type = "binary")
      lacking <- short(needs)
    }
    said <- if (length(reported)) paste0("\nR reported: ", paste(unique(reported), collapse = "; "))
    if (anyNA(lacking$have)) {
      absent <- lacking$name[is.na(lacking$have)]
      # R removes its own locks when an installation ends. One that is still
      # there was left by an installation that was cut off (or belongs to
      # another R session that is installing now).
      left <- file.path(target, c("00LOCK", paste0("00LOCK-", absent)))
      left <- left[dir.exists(left)]
      # R cannot replace a package whose files an R session has open: one this
      # session has loaded and that is still partly on disk, or one R said it
      # could not remove. Trying again in this session cannot help then.
      in_use <- any(absent %in% loadedNamespaces() & dir.exists(file.path(target, absent))) ||
        any(grepl("cannot remove prior installation", c(reported, warned), fixed = TRUE))
      stop(pkg, " was not changed. These packages it needs could not be installed: ", paste(absent, collapse = ", "), ".",
           said, "\n",
           if (length(left)) paste0("An earlier installation into this library was cut off and left a lock behind; ",
                                    "R installs nothing there while it exists:\n  ", paste(left, collapse = "\n  "),
                                    "\nClose every other R session, delete ",
                                    if (length(left) > 1L) "these folders" else "this folder", ", ", again,
                                    # Unless R itself named the lock, something else may have failed as well.
                                    if (!any(grepl("failed to lock directory", c(reported, warned), fixed = TRUE)))
                                      paste0("\n", why()))
           else if (in_use) paste0(restart, ", ", again, " R cannot replace a package while an R session is using it; ",
                                   "close any other R session too.")
           else why(), call. = FALSE)
    }
    if (nrow(lacking)) {
      stop(pkg, " was not changed. It needs newer versions of: ",
           paste(sprintf("%s (at least %s, installed %s)", lacking$name, lacking$least, lacking$have), collapse = ", "),
           ".", said, outdated(), "\n", restart, ", ", again, call. = FALSE)
    }
    added
  }

  # An installation that was cut off (R was closed, the computer went down, the
  # step was stopped) leaves R's lock for this package behind, and R then
  # refuses every later installation. The copy R had set aside goes back when
  # no version is in place. Only this package's own lock is looked at.
  lock <- file.path(target, paste0("00LOCK-", pkg))
  if (file.exists(lock)) {
    kept <- file.path(lock, pkg)
    restore <- is.na(on_disk()[["Version"]]) && file.exists(file.path(kept, "DESCRIPTION"))
    if (restore) unlink(file.path(target, pkg), recursive = TRUE)
    # The lock goes only once the copy it holds is back in place.
    if (!restore || suppressWarnings(file.rename(kept, file.path(target, pkg))) ||
        all(file.copy(kept, target, recursive = TRUE, copy.date = TRUE))) unlink(lock, recursive = TRUE)
    if (file.exists(lock)) {
      stop("An earlier installation of ", pkg, " was interrupted, and what it left behind could not be removed:\n  ", lock,
           "\n", restart, ", close every other R session, ", again, " If this message returns, delete that folder first.",
           call. = FALSE)
    }
    message("An earlier installation of ", pkg, " was interrupted; ",
            if (restore) paste0("version ", on_disk()[["Version"]], ", which it had set aside, was put back and "),
            "its leftovers were removed.")
  }

  before <- on_disk()
  # A release tag and a commit always name the same code, so a copy made from
  # one is not fetched again. A branch moves on, and a copy installed from a
  # folder is not the published one.
  same_code <- function() identical(before[["Source"]], ref) &&
    (identical(ref, paste0("v", before[["Version"]])) || grepl("^[0-9a-f]{40}$", ref))
  if (is.null(path) && !isTRUE(force) && same_code() && loaded() %in% c(NA_character_, before[["Version"]])) {
    # A package it needs may have been removed or damaged since.
    added <- provide(asks(utils::packageDescription(pkg, lib.loc = target, fields = "Imports")))
    return(finish(sprintf("%s %s is already installed. %s", pkg, before[["Version"]],
                          if (length(added)) paste0("It was not downloaded again; packages it needs were installed: ",
                                                    paste(added, collapse = ", "), ".")
                          else "Nothing was downloaded or changed.")))
  }

  # 1. Fetch and assemble first: a failed download leaves everything as it was.
  temporary <- tempfile("scoRch-install-")
  dir.create(temporary)
  on.exit(unlink(temporary, recursive = TRUE), add = TRUE)
  came_from <- if (is.null(path)) ref else "local"
  if (is.null(path)) {
    archive <- file.path(temporary, "repository.zip")
    url <- paste0("https://github.com/fawazbouhamad/scoRch.simulator/archive/",
                  utils::URLencode(ref, reserved = TRUE), ".zip")
    message("Downloading ", pkg, " ", ref, " with its example data (about 100 MB). This can take a few minutes.")
    # The default limit of 60 seconds is too short for this file on many connections.
    limit <- options(timeout = max(3600, getOption("timeout")))
    on.exit(options(limit), add = TRUE)
    fetched <- tryCatch(utils::download.file(url, archive, mode = "wb"), error = function(e) conditionMessage(e))
    if (is.character(fetched) || !isTRUE(fetched == 0)) {
      stop(pkg, " ", ref, " could not be downloaded, so nothing was changed",
           if (is.character(fetched)) paste0(": ", fetched), ".\nCheck the internet connection and that ", ref,
           " exists at https://github.com/fawazbouhamad/scoRch.simulator, ", again, call. = FALSE)
    }
    # A download can report success and still be cut short, or be a web page
    # that a network put in its place.
    entries <- tryCatch(utils::unzip(archive, list = TRUE)$Name, error = function(e) NULL)
    if (is.null(entries)) {
      stop(pkg, " ", ref, " did not arrive complete, so nothing was changed.\nCheck the internet connection, ", again,
           call. = FALSE)
    }
    if (any(grepl("(^/|^[A-Za-z]:|(^|/)\\.\\.(/|$)|\\\\)", entries))) {
      stop("The source archive contains an invalid path.", call. = FALSE)
    }
    roots <- unique(sub("/.*$", "", entries))
    if (length(roots) != 1L) stop("The source archive must contain one repository.", call. = FALSE)
    utils::unzip(archive, exdir = temporary)
    path <- file.path(temporary, roots)
  }
  package <- prepare_scorch_package(path, file.path(temporary, pkg))
  # Only a copy made from the same release may later count as already installed.
  description <- file.path(package, "DESCRIPTION")
  fields <- readLines(description, warn = FALSE)
  stamped <- file(description, "wb")
  writeLines(c(fields[nzchar(trimws(fields))], paste0(origin, ": ", came_from)), stamped)
  close(stamped)
  wanted <- unname(read.dcf(description, "Version")[1L, 1L])

  # 2. Packages this one needs, before anything of an earlier installation is
  # touched. Installed ones are left alone unless `upgrade` says otherwise.
  if (!requireNamespace("remotes", quietly = TRUE)) try(utils::install.packages("remotes", lib = target))
  if (!requireNamespace("remotes", quietly = TRUE)) {
    stop("The helper package 'remotes' could not be installed, so ", pkg, " was not changed.\nR's messages above, and ",
         "any warnings below, give the reason. If they report a failed download, check the internet connection, ",
         again, call. = FALSE)
  }
  message("Installing ", pkg, " ", wanted, " and any packages it still needs.")
  # R can stop here for one package that failed. What is still missing is
  # tried again below and named, so its report is kept instead.
  refused <- tryCatch({
    remotes::install_deps(package, dependencies = NA, upgrade = upgrade, lib = target, ...)
    NULL
  }, error = function(e) conditionMessage(e))
  provide(asks(read.dcf(description, "Imports")[1L, 1L]), tried = TRUE, reported = refused)
  if (!is.null(refused)) {
    message("R reported a problem while it installed packages (", refused, "). Every package ", pkg,
            " needs is installed, so the installation continues.")
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
            "\nDo not run the installation block again. The new version is already installed.", aside)
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
