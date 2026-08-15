# The per-user application image.
#
# One container per logged-in user, started by ShinyProxy and thrown away when
# they leave. It deliberately has NO access to the Docker socket: the project
# directory is user-supplied and ends up in a `-v` argument, so a container that
# could reach Docker would let any user mount any host path into a root
# container. Reconstruction goes through the broker instead.

FROM rocker/geospatial:4.4.2

# ODM products are large and the app streams them; terra wants a scratch area.
ENV R_LIBS_USER=/usr/local/lib/R/site-library \
    DRONEBIOR_DATA_ROOT=/data \
    DRONEBIOR_JOB_DIR=/spool

RUN apt-get update && apt-get install -y --no-install-recommends \
      libglpk40 libxt6 libgdal-dev libproj-dev libgeos-dev libudunits2-dev \
    && rm -rf /var/lib/apt/lists/*

# lidR and rlas left CRAN in June 2026; the maintainer publishes them on
# r-universe, which is what DESCRIPTION's Additional_repositories points at.
RUN Rscript -e 'install.packages(c("shiny","bslib","htmltools","leaflet","leafem", \
      "jsonlite","promises","future","callr","processx","caret","ranger","FNN", \
      "RANN","interp","RCSF","withr","rmarkdown","knitr","httr"), \
      repos = "https://cloud.r-project.org")' \
 && Rscript -e 'install.packages("lidR", repos = c("https://r-lidar.r-universe.dev", \
      "https://cloud.r-project.org"))'

ARG DRONEBIOR_REF=v0.6.0
RUN Rscript -e 'remotes::install_github(paste0("HugoMachadoRodrigues/DroneBioR@", \
      Sys.getenv("DRONEBIOR_REF", "v0.6.0")), upgrade = "never")'
ENV DRONEBIOR_REF=${DRONEBIOR_REF}

# Never run the application as root, even without the socket.
RUN useradd -u 10001 -m -s /usr/sbin/nologin app
USER 10001

EXPOSE 3838
CMD ["R", "-e", "DroneBioR::run_drone_biomass_studio(project_dir = Sys.getenv('DRONEBIOR_DATA_ROOT', '/data'), port = 3838, launch.browser = FALSE, host = '0.0.0.0')"]
