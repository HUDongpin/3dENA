FROM rocker/r-ver:4.4.1

ARG ENA3D_BUILD_ID=development
ARG ENA3D_APP_VERSION=0.2.0-dev
ENV DEBIAN_FRONTEND=noninteractive \
    ENA3D_BUILD_ID=${ENA3D_BUILD_ID} \
    ENA3D_APP_VERSION=${ENA3D_APP_VERSION} \
    ENA3D_PROJECT_ROOT=/opt/ena3d \
    RENV_PATHS_LIBRARY=/opt/renv/library \
    RENV_PATHS_CACHE=/opt/renv/cache \
    R_LIBS_USER=/opt/renv/library \
    RENV_CONFIG_AUTO_SNAPSHOT=FALSE \
    RENV_CONFIG_SANDBOX_ENABLED=FALSE

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
      ca-certificates \
      curl \
      g++ \
      gcc \
      gfortran \
      libcurl4-openssl-dev \
      libfontconfig1-dev \
      libfreetype6-dev \
      libharfbuzz-dev \
      libfribidi-dev \
      libjpeg-dev \
      libpng-dev \
      libssl-dev \
      libtiff-dev \
      libxml2-dev \
      make \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/ena3d
COPY renv.lock ./renv.lock
COPY .Rprofile ./.Rprofile
COPY renv/bootstrap.R ./renv/bootstrap.R
COPY renv/activate.R ./renv/activate.R
RUN Rscript renv/bootstrap.R
RUN Rscript -e 'stopifnot(normalizePath("/opt/renv/library") %in% .libPaths()); stopifnot(requireNamespace("shiny", quietly=TRUE), requireNamespace("rENA", quietly=TRUE), requireNamespace("jsonlite", quietly=TRUE), requireNamespace("readxl", quietly=TRUE))'

COPY R ./R
COPY sample_data ./sample_data
COPY README.md TRAJECTORY_ANALYSIS.md LICENSE VERSION ./

RUN groupadd --system --gid 10001 ena3d \
    && useradd --system --uid 10001 --gid ena3d --home /home/ena3d ena3d \
    && mkdir -p /home/ena3d /tmp/ena3d \
    && chown -R ena3d:ena3d /home/ena3d /tmp/ena3d \
    && chmod -R a-w /opt/ena3d/R /opt/ena3d/sample_data

USER ena3d:ena3d
EXPOSE 3838

HEALTHCHECK --interval=30s --timeout=5s --start-period=30s --retries=3 \
  CMD curl --fail --silent --show-error http://127.0.0.1:3838/ena3d-health/healthz.json >/dev/null || exit 1

CMD ["R", "-e", "shiny::runApp('/opt/ena3d/R', host='0.0.0.0', port=3838, launch.browser=FALSE)"]
