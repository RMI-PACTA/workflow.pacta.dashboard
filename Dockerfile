FROM docker.io/rocker/r-ver:4.3.1

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

COPY main.R /workflow.pacta.dashboard/main.R

WORKDIR /workflow.pacta.dashboard

CMD ["Rscript", "--vanilla", "/workflow.pacta.dashboard/main.R"]
