FROM docker.io/rocker/r-ver:4.3.1

RUN CRAN_LIKE_URL="https://packagemanager.posit.co/cran/__linux__/jammy/2024-04-05"; \
    printf "options(\n \
    repos = c(CRAN = '%s'),\n \
    pak.no_extra_messages = TRUE,\n \
    pkg.sysreqs = FALSE,\n \
    pkg.sysreqs_db_update = FALSE,\n \
    pkg.sysreqs_update = FALSE\n \
    )\n" \
    "$CRAN_LIKE_URL" \
    > "${R_HOME}/etc/Rprofile.site" \
    && Rscript -e "install.packages('pak', repos = sprintf('https://r-lib.github.io/p/pak/stable/%s/%s/%s', .Platform[['pkgType']], R.Version()[['os']], R.Version()[['arch']]))"

# install system dependencies
RUN apt-get update \
    && DEBIAN_FRONTEND="noninteractive" \
    apt-get install -y --no-install-recommends \
    git=1:2.34.*

# copy in DESCRIPTION from this repo
COPY DESCRIPTION /workflow.pacta.dashboard/DESCRIPTION

# install pak, find dependencises from DESCRIPTION, and install them.
RUN Rscript -e "\
    install.packages('pak'); \
    deps <- pak::local_deps(root = '/workflow.pacta.dashboard'); \
    pkg_deps <- deps[!deps[['direct']], 'ref']; \
    print(pkg_deps); \
    pak::pak(pkg_deps); \
    "

COPY main.R /workflow.pacta.dashboard/main.R

WORKDIR /workflow.pacta.dashboard

CMD ["Rscript", "--vanilla", "/workflow.pacta.dashboard/main.R"]
