# syntax=docker/dockerfile:1
# The app's image, built by scripts/deploy-azure.R. The GitHub token, which
# lifts GitHub's rate limit on installs from GitHub, is a build secret, so no
# layer keeps it.
ARG R_VERSION=4.6.2
FROM rocker/r-ver:${R_VERSION}

ARG KELPBIO_REF
ARG KELPBIODATA_REF

ENV PKG_SYSREQS=true

RUN Rscript -e 'install.packages("pak")'

# The dependencies go in a layer of their own, so a change to the app's code
# does not recompile kelpbio's Stan models.
COPY DESCRIPTION /src/DESCRIPTION
RUN --mount=type=secret,id=github_pat \
  test -n "$KELPBIO_REF" && test -n "$KELPBIODATA_REF" && \
  GITHUB_PAT="$(cat /run/secrets/github_pat)" Rscript -e ' \
    pak::pkg_install( \
      c( \
        "deps::/src", \
        Sys.getenv("KELPBIO_REF"), \
        Sys.getenv("KELPBIODATA_REF"), \
        "knitr", \
        "rmarkdown" \
      ), \
      upgrade = FALSE, \
      ask = FALSE \
    ); \
    pak::cache_clean()'

# The Help pages show kelpbio's articles from its installed doc/ folder, which
# an install from GitHub leaves out: they are rendered here from kelpbio's
# source at the same commit, with the kelpbio just installed.
RUN --mount=type=secret,id=github_pat \
  apt-get update && apt-get install -y --no-install-recommends pandoc && \
  GITHUB_PAT="$(cat /run/secrets/github_pat)" Rscript - <<'EOF' && \
  apt-get purge -y pandoc && apt-get autoremove -y && rm -rf /var/lib/apt/lists/*
ref <- Sys.getenv("KELPBIO_REF")
tarball <- tempfile(fileext = ".tar.gz")
download.file(
  paste0("https://api.github.com/repos/", sub("@.*", "", ref), "/tarball/", sub(".*@", "", ref)),
  tarball,
  headers = c(Authorization = paste("Bearer", Sys.getenv("GITHUB_PAT"))),
  quiet = TRUE
)
src <- tempfile()
untar(tarball, exdir = src)
vignettes <- file.path(list.files(src, full.names = TRUE), "vignettes")
doc <- file.path(system.file(package = "kelpbio"), "doc")
dir.create(doc, showWarnings = FALSE)
for (rmd in list.files(vignettes, "[.]Rmd$", full.names = TRUE)) {
  rmarkdown::render(rmd, output_dir = doc, quiet = TRUE)
}
unlink(c(tarball, src), recursive = TRUE)
EOF

COPY . /src
RUN R CMD INSTALL --no-docs --no-multiarch /src && rm -rf /src

RUN useradd --create-home app
USER app
WORKDIR /home/app

# Two fit daemons, so a second user's fit starts at once rather than waiting
# for the first; a session idle for 30 minutes ends, so a forgotten browser tab
# does not keep the server running; errors show users a generic message, not R's.
EXPOSE 3838
CMD ["Rscript", "-e", "options(kelpbioshiny.daemons = 2, kelpbioshiny.idle_minutes = 30, shiny.sanitize.errors = TRUE);shiny::runApp(kelpbioshiny::run_app(), host = '0.0.0.0', port = 3838, launch.browser = FALSE)"]
