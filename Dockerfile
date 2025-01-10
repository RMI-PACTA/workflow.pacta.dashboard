FROM docker.io/rocker/r-ver:4.3.1 AS base

# install system dependencies
RUN apt-get update \
    && DEBIAN_FRONTEND="noninteractive" \
    apt-get install -y --no-install-recommends \
      git=1:2.34.* \
      libcurl4-openssl-dev=7.81.* \
      libgit2-dev=1.1.* \
      libicu-dev=70.* \
      libnode-dev=12.22.* \
      libssl-dev=3.0.* \
    && chmod -R a+rwX /root \
    && rm -rf /var/lib/apt/lists/*

# set frozen CRAN repo and RProfile.site
# This block makes use of the builtin ARG $TARGETPLATFORM (See:
# https://www.docker.com/blog/faster-multi-platform-builds-dockerfile-cross-compilation-guide/
# ) to pick the correct CRAN-like repo, which will let us target binaries fo
# supported platforms
ARG TARGETPLATFORM
RUN PACKAGE_PIN_DATE="2024-03-05" && \
  echo "TARGETPLATFORM: $TARGETPLATFORM" && \
  if [ "$TARGETPLATFORM" = "linux/amd64" ] && grep -q -e "Jammy Jellyfish" "/etc/os-release" ; then \
    CRAN_LIKE_URL="https://packagemanager.posit.co/cran/__linux__/jammy/$PACKAGE_PIN_DATE"; \
  else \
    CRAN_LIKE_URL="https://packagemanager.posit.co/cran/$PACKAGE_PIN_DATE"; \
  fi && \
  echo "CRAN_LIKE_URL: $CRAN_LIKE_URL" && \
  printf "options(\n \
    repos = c(CRAN = '%s'),\n \
    pak.no_extra_messages = TRUE,\n \
    pkg.sysreqs = FALSE,\n \
    pkg.sysreqs_db_update = FALSE,\n \
    pkg.sysreqs_update = FALSE\n \
  )\n" \
  "$CRAN_LIKE_URL" \
  > "${R_HOME}/etc/Rprofile.site"

# install pak
RUN Rscript -e "install.packages( \
      'pak', \
      repos = sprintf( \
        'https://r-lib.github.io/p/pak/stable/%s/%s/%s', \
        .Platform[['pkgType']], \
        R.Version()[['os']], \
        R.Version()[['arch']] \
      ) \
    )"

# copy in DESCRIPTION from this repo
COPY DESCRIPTION /workflow.pacta.dashboard/DESCRIPTION

# install pak, find dependencises from DESCRIPTION, and install them.
RUN Rscript -e "pak::local_install_deps('/workflow.pacta.dashboard')"

FROM base AS install-pacta

ENV DASHBOARD_SKELETON_FILES_DIR="/mnt/dashboard_skeleton_files"
COPY --from=ghcr.io/rmi-pacta/pacta-dashboard-ui:main /app/build $DASHBOARD_SKELETON_FILES_DIR/

COPY . /workflow.pacta.dashboard/

RUN Rscript -e "pak::local_install(root = '/workflow.pacta.dashboard')"

# set default run behavior
ENTRYPOINT ["Rscript", "--vanilla", "/workflow.pacta.dashboard/inst/extdata/scripts/prepare_dashboard_data.R"]
