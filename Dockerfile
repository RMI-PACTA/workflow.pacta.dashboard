FROM docker.io/rocker/r-ver:4.3.1 AS base

RUN CRAN_LIKE_URL="https://packagemanager.posit.co/cran/__linux__/jammy/2024-04-05"; \
    echo "options(repos = c(CRAN = '$CRAN_LIKE_URL'))" \
    > "${R_HOME}/etc/Rprofile.site"

# install system dependencies
RUN apt-get update \
    && DEBIAN_FRONTEND="noninteractive" \
    apt-get install -y --no-install-recommends \
    git=1:2.34.*

# install pak
RUN Rscript -e "install.packages('pak', repos = 'https://r-lib.github.io/p/pak/stable/')"

# copy in DESCRIPTION from this repo
COPY DESCRIPTION /workflow.pacta.dashboard/DESCRIPTION

# install pak, find dependencises from DESCRIPTION, and install them.
RUN Rscript -e "pak::local_install_deps('/workflow.pacta.dashboard')"

FROM base AS install-pacta

COPY . /workflow.pacta.dashboard/

RUN Rscript -e "pak::local_install(root = '/workflow.pacta.dashboard')"

# set default run behavior
ENTRYPOINT ["Rscript", "--vanilla", "/workflow.pacta.dashboard/inst/extdata/scripts/prepare_dashboard_data.R"]
