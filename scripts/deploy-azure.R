# Builds the app's Docker image and deploys it to Azure Container Apps.
#
#   Rscript scripts/deploy-azure.R          build for Azure, push, deploy
#   Rscript scripts/deploy-azure.R --local  build and run at http://localhost:3838
#
# Run from the package root, with the kelpbio and kelpbiodata checkouts beside
# it (~/Code/HakaiInstitute/) and Docker running; to deploy, the Azure CLI must
# be signed in (az login).
#
#   * kelpbio and kelpbiodata are installed from GitHub at the commits checked
#     out locally, which must be committed and pushed. The GitHub token
#     (GITHUB_PAT, else `gh auth token`) is passed as a build secret, so the
#     image does not keep it.
#   * The app is built from the working tree as it is. Each deploy's image is
#     tagged with the kelpbioshiny commit, "dirty" when the app files have
#     uncommitted changes, and the time, so each deploy makes a new revision to
#     roll back to, even when only kelpbio or kelpbiodata changed.
#   * --local builds for this machine and runs the image at the app's size on
#     Azure (4 CPUs, 8 GB). A deploy builds for linux/amd64, which an Apple
#     silicon Mac emulates: compiling kelpbio's Stan models then takes much
#     longer than in a local build.
#   * The first deploy creates the container app: 4 CPUs and 8 GB, at most one
#     replica (every session and fit lives in it, in memory only), none while
#     idle, and the image pulled with the app's managed identity, not a
#     password. Its ingress is internal, so the app is not reachable from the
#     internet until it is opened (below). A deploy replaces the running
#     replica, which ends every open session.
#
# Before the first deploy, create the registry and container apps environment
# once, in the resource group (created by the subscription's administrator):
#
#   az acr create --resource-group rg-kelpbioshiny --name kelpbioshiny --sku Basic
#   az containerapp env create --resource-group rg-kelpbioshiny \
#     --name kelpbioshiny-env --location canadacentral
#
# After the first deploy, in the Azure portal:
#
#   * Cost Management: a budget with alerts on the resource group. The app has
#     no sign-in, so anyone with its address can start fits, and an open
#     browser tab keeps the replica running.
#   * Policy: "Allowed locations" on the resource group, Canada only.
#   * Optional, to limit who can use the app: Authentication (Microsoft as the
#     identity provider, sign-in required, the enterprise application set to
#     "Assignment required" with each user assigned), or Ingress > IP security
#     restrictions.
#
# then open the app to the internet (HTTPS only):
#
#   az containerapp ingress enable --resource-group rg-kelpbioshiny \
#     --name kelpbioshiny --type external --target-port 3838 --transport auto

resource_group <- "rg-kelpbioshiny"
# Registry names are global across Azure: change it if taken.
registry <- "kelpbioshiny"
environment <- "kelpbioshiny-env"
app_name <- "kelpbioshiny"
app_files <- c(
  "DESCRIPTION",
  "NAMESPACE",
  "LICENSE",
  "R",
  "inst",
  "man",
  "Dockerfile",
  ".dockerignore"
)

usage <- paste(
  "Usage: Rscript scripts/deploy-azure.R [--local]",
  "  --local  build for this machine and run at http://localhost:3838",
  sep = "\n"
)
args <- commandArgs(trailingOnly = TRUE)
if (any(c("--help", "-h") %in% args)) {
  cat(usage, "\n", sep = "")
  quit(status = 0)
}
unknown <- setdiff(args, "--local")
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
local <- "--local" %in% args

if (
  !file.exists("DESCRIPTION") ||
    read.dcf("DESCRIPTION", "Package")[[1]] != "kelpbioshiny"
) {
  stop("Run from the kelpbioshiny package root.", call. = FALSE)
}

git <- function(path, ...) {
  system2("git", c("-C", shQuote(path), ...), stdout = TRUE)
}

run <- function(command, args) {
  message("$ ", command, " ", paste(args, collapse = " "))
  status <- system2(command, args)
  if (status != 0) {
    stop(command, " failed (exit status ", status, ").", call. = FALSE)
  }
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

github_pat <- Sys.getenv("GITHUB_PAT")
if (!nzchar(github_pat)) {
  github_pat <- suppressWarnings(system2(
    "gh",
    c("auth", "token"),
    stdout = TRUE,
    stderr = FALSE
  ))
}
if (!length(github_pat) || !nzchar(github_pat[[1]])) {
  stop(
    "No GitHub token: set GITHUB_PAT or sign in with `gh auth login`.",
    call. = FALSE
  )
}
Sys.setenv(GITHUB_PAT = github_pat[[1]])

build_args <- c(
  "--secret",
  "id=github_pat,env=GITHUB_PAT",
  "--build-arg",
  paste0("R_VERSION=", R.version$major, ".", R.version$minor),
  "--build-arg",
  paste0("KELPBIO_REF=", pushed_ref("kelpbio")),
  "--build-arg",
  paste0("KELPBIODATA_REF=", pushed_ref("kelpbiodata"))
)

if (local) {
  run(
    "docker",
    c(
      "buildx",
      "build",
      build_args,
      "--tag",
      "kelpbioshiny:local",
      "--load",
      "."
    )
  )
  message("Running at http://localhost:3838 (Ctrl+C stops it).")
  run(
    "docker",
    c(
      "run",
      "--rm",
      "--name",
      "kelpbioshiny-local",
      "--publish",
      "3838:3838",
      "--cpus",
      "4",
      "--memory",
      "8g",
      "kelpbioshiny:local"
    )
  )
  quit(status = 0)
}

tag <- git(".", "rev-parse", "--short", "HEAD")
dirty <- git(".", "status", "--porcelain", "--", app_files)
if (length(dirty)) {
  message(
    "Deploying ",
    length(dirty),
    " uncommitted change(s) in the app files:"
  )
  message(paste0("  ", dirty, collapse = "\n"))
  tag <- paste0(tag, "-dirty")
}
tag <- paste0(tag, "-", format(Sys.time(), "%Y%m%d%H%M%S"))
registry_server <- paste0(registry, ".azurecr.io")
image <- paste0(registry_server, "/kelpbioshiny:", tag)

run("az", c("acr", "login", "--name", registry))
run(
  "docker",
  c(
    "buildx",
    "build",
    build_args,
    "--platform",
    "linux/amd64",
    "--tag",
    image,
    "--push",
    "."
  )
)

app_exists <- system2(
  "az",
  c(
    "containerapp",
    "show",
    "--resource-group",
    resource_group,
    "--name",
    app_name
  ),
  stdout = FALSE,
  stderr = FALSE
) ==
  0

if (app_exists) {
  run(
    "az",
    c(
      "containerapp",
      "update",
      "--resource-group",
      resource_group,
      "--name",
      app_name,
      "--image",
      image
    )
  )
} else {
  run(
    "az",
    c(
      "containerapp",
      "create",
      "--resource-group",
      resource_group,
      "--name",
      app_name,
      "--environment",
      environment,
      "--image",
      image,
      "--registry-server",
      registry_server,
      "--registry-identity",
      "system",
      "--ingress",
      "internal",
      "--target-port",
      "3838",
      "--cpu",
      "4",
      "--memory",
      "8Gi",
      "--min-replicas",
      "0",
      "--max-replicas",
      "1"
    )
  )
  run(
    "az",
    c(
      "containerapp",
      "ingress",
      "sticky-sessions",
      "set",
      "--resource-group",
      resource_group,
      "--name",
      app_name,
      "--affinity",
      "sticky"
    )
  )
}

fqdn <- system2(
  "az",
  c(
    "containerapp",
    "show",
    "--resource-group",
    resource_group,
    "--name",
    app_name,
    "--query",
    "properties.configuration.ingress.fqdn",
    "--output",
    "tsv"
  ),
  stdout = TRUE
)
message("Deployed ", tag, ": https://", fqdn)
if (!app_exists) {
  message(
    "The app is internal: set a budget, then open its ingress ",
    "(see the steps at the top of this script)."
  )
}
