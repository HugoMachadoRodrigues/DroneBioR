# The reconstruction broker.
#
# The one container that holds the Docker socket, and the one no user can
# reach: it has no network service and no port. Users leave a file; it reads
# the file. That asymmetry is the whole security model.

FROM rocker/geospatial:4.4.2

RUN apt-get update && apt-get install -y --no-install-recommends \
      docker.io ca-certificates \
    && rm -rf /var/lib/apt/lists/*

RUN Rscript -e 'install.packages(c("jsonlite","withr"), repos = "https://cloud.r-project.org")' \
 && Rscript -e 'install.packages("lidR", repos = c("https://r-lidar.r-universe.dev", \
      "https://cloud.r-project.org"))'

ARG DRONEBIOR_REF=v0.6.0
RUN Rscript -e 'remotes::install_github(paste0("HugoMachadoRodrigues/DroneBioR@", \
      Sys.getenv("DRONEBIOR_REF", "v0.6.0")), upgrade = "never")'

COPY broker.R /usr/local/bin/broker.R
CMD ["Rscript", "/usr/local/bin/broker.R"]
