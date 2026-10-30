# Deploys the app from this working tree to shinyapps.io for preview.
#
#   Rscript scripts/deploy.R            build the deploy library, then deploy
#   Rscript scripts/deploy.R --dry-run  build the deploy library only
#
# Run from the package root, with the kelpbio and kelpbiodata checkouts beside
# it (~/Code/HakaiInstitute/).
#
#   * shinyapps.io installs every package from a repository it can reach, so
#     kelpbio and kelpbiodata are installed from GitHub at the commits checked
#     out locally, which must be committed and pushed. They go into a library
#     of their own (kept between runs, so kelpbio's Stan models are recompiled
#     only when its commit changes), and the deploy runs in an R process on that
#     library alone: the locally built packages in the user library are never
#     touched or deployed.
#   * kelpbiodata is private: shinyapps.io installs it only if the account has a
#     GitHub login with private repository access (Account > Profile).
#   * The app is deployed from the working tree as it is, uncommitted changes
#     included, and runs it from source (app.R).

account <- "poissonconsulting"
app_name <- "kelpbioshiny-dev"
instance_size <- "xlarge"
app_files <- c("app.R", "DESCRIPTION", "NAMESPACE", "LICENSE", "R", "inst")

usage <- paste(
  "Usage: Rscript scripts/deploy.R [--dry-run]",
  "  --dry-run  build the deploy library but do not deploy",
  sep = "\n"
)
args <- commandArgs(trailingOnly = TRUE)
if (any(c("--help", "-h") %in% args)) {
  cat(usage, "\n", sep = "")
  quit(status = 0)
}
unknown <- setdiff(args, "--dry-run")
if (length(unknown)) {
  cat(
    "Unknown option: ",
    paste(unknown, collapse = " "),
    "\n\n",
    usage,
    "\n",
    sep = ""
  )
  quit(status = 1)
}
dry_run <- "--dry-run" %in% args

if (
  !file.exists("DESCRIPTION") ||
    read.dcf("DESCRIPTION", "Package")[[1]] != "kelpbioshiny"
) {
  stop("Run from the kelpbioshiny package root.", call. = FALSE)
}

git <- function(path, ...) {
  system2("git", c("-C", shQuote(path), ...), stdout = TRUE)
}

# The GitHub ref of a sibling checkout's HEAD, which must be clean and pushed.
pushed_ref <- function(pkg) {
  path <- normalizePath(file.path("..", pkg), mustWork = TRUE)
  if (length(git(path, "status", "--porcelain", "--untracked-files=no"))) {
    stop(
      pkg,
      " has uncommitted changes: commit and push them first.",
      call. = FALSE
    )
  }
  sha <- git(path, "rev-parse", "HEAD")
  if (!length(git(path, "branch", "-r", "--contains", sha))) {
    stop(
      pkg,
      " HEAD (",
      substr(sha, 1, 7),
      ") is not pushed: push it first.",
      call. = FALSE
    )
  }
  message(
    pkg,
    ": ",
    git(path, "branch", "--show-current"),
    " @ ",
    substr(sha, 1, 7)
  )
  paste0("HakaiInstitute/", pkg, "@", sha)
}

refs <- c(
  "deps::.",
  pushed_ref("kelpbio"),
  pushed_ref("kelpbiodata"),
  "pkgload",
  "rsconnect"
)

dirty <- git(".", "status", "--porcelain", "--", app_files)
if (length(dirty)) {
  message(
    "Deploying ",
    length(dirty),
    " uncommitted change(s) in the app files:"
  )
  message(paste0("  ", dirty, collapse = "\n"))
}

lib <- file.path(
  tools::R_user_dir("kelpbioshiny", "cache"),
  "deploy-library",
  paste(R.version$major, R.version$minor, sep = ".")
)
dir.create(lib, recursive = TRUE, showWarnings = FALSE)
message("Deploy library: ", lib)

callr::r(
  function(refs, lib) {
    pak::pkg_install(refs, lib = lib, upgrade = FALSE, ask = FALSE)
  },
  args = list(refs = refs, lib = lib),
  show = TRUE
)

if (dry_run) {
  message("Dry run: not deployed.")
  quit(status = 0)
}

callr::r(
  function(app_files, account, app_name, instance_size) {
    rsconnect::deployApp(
      appDir = ".",
      appFiles = app_files,
      appName = app_name,
      account = account,
      server = "shinyapps.io",
      forceUpdate = TRUE,
      launch.browser = FALSE
    )
    rsconnect::configureApp(
      appName = app_name,
      account = account,
      server = "shinyapps.io",
      size = instance_size
    )
  },
  args = list(
    app_files = app_files,
    account = account,
    app_name = app_name,
    instance_size = instance_size
  ),
  libpath = lib,
  show = TRUE
)
