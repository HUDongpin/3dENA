FROM docker.io/rocker/r-ver:4.4.1@sha256:f3ef082e63ca36547fcf0c05a0d74255ddda6ca7bd88f1dae5a44ce117fc3804

ENV DEBIAN_FRONTEND=noninteractive \
    ENA3D_PROJECT_ROOT=/opt/ena3d \
    ENA3D_RUNTIME_PROFILE=persistent \
    RENV_PATHS_LIBRARY=/opt/renv/library \
    RENV_PATHS_CACHE=/opt/renv/cache \
    R_LIBS_USER=/opt/renv/library \
    RENV_CONFIG_AUTO_SNAPSHOT=FALSE \
    RENV_CONFIG_CACHE_SYMLINKS=FALSE \
    RENV_CONFIG_SANDBOX_ENABLED=FALSE \
    SHINY_LOG_STDERR=1

ARG UBUNTU_ARCHIVE_MIRROR=https://archive.ubuntu.com
ARG UBUNTU_SECURITY_MIRROR=https://security.ubuntu.com
RUN case "${UBUNTU_ARCHIVE_MIRROR}" in https://*) ;; *) exit 1 ;; esac \
    && case "${UBUNTU_ARCHIVE_MIRROR}" in \
      *[!A-Za-z0-9:./_-]*) exit 1 ;; \
    esac \
    && case "${UBUNTU_SECURITY_MIRROR}" in https://*) ;; *) exit 1 ;; esac \
    && case "${UBUNTU_SECURITY_MIRROR}" in \
      *[!A-Za-z0-9:./_-]*) exit 1 ;; \
    esac \
    && sed -i \
      -e "s#http://archive.ubuntu.com#${UBUNTU_ARCHIVE_MIRROR}#g" \
      -e "s#http://security.ubuntu.com#${UBUNTU_SECURITY_MIRROR}#g" \
      /etc/apt/sources.list \
    && apt-get -o Acquire::Retries=5 update \
    && apt-get install -y --no-install-recommends \
      ca-certificates \
      curl \
      gdebi-core \
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
      lsb-release \
      make \
    && rm -rf /var/lib/apt/lists/*

ARG SHINY_SERVER_VERSION=1.5.23.1030
ARG SHINY_SERVER_SHA256=4a3d063a06ccd1b6c53eb1d7f4fb59965bced10d1c5c87e8c476b58dd6fd35ee
ARG TARGETARCH
# Do not export SHINY_SERVER_VERSION. The installed SockJS adapter injects
# the actual host version before loading app.R; exporting the build argument
# would let a direct application runner impersonate the persistent host.
RUN --mount=type=cache,target=/var/cache/ena3d-downloads,sharing=locked \
    shiny_package=/var/cache/ena3d-downloads/shiny-server.deb; \
    test "${TARGETARCH:-amd64}" = "amd64" \
    && curl --fail --location --silent --show-error \
      --retry 10 \
      --retry-all-errors \
      --retry-delay 2 \
      --connect-timeout 30 \
      --max-time 300 \
      --speed-time 30 \
      --speed-limit 1024 \
      --continue-at - \
      "https://download3.rstudio.org/ubuntu-20.04/x86_64/shiny-server-${SHINY_SERVER_VERSION}-amd64.deb" \
      --output "${shiny_package}" \
    && printf '%s  %s\n' \
      "${SHINY_SERVER_SHA256}" \
      "${shiny_package}" \
      | sha256sum --check --strict - \
    && gdebi --non-interactive "${shiny_package}" \
    && rm -f "${shiny_package}"

ARG RENV_REPOSITORY=https://cloud.r-project.org
ENV RENV_CONFIG_PPM_ENABLED=FALSE \
    RENV_CONFIG_REPOS_OVERRIDE=${RENV_REPOSITORY}

WORKDIR /opt/ena3d
COPY renv.lock ./renv.lock
COPY .Rprofile ./.Rprofile
COPY renv/bootstrap.R ./renv/bootstrap.R
COPY renv/activate.R ./renv/activate.R
ARG RENV_VERSION=1.1.8
ARG RENV_SHA256=141b3a77a9e405eb1b586db7bda43088825b955ceacc1a2de0322a4fcf78ae08
RUN --mount=type=cache,target=/opt/renv/cache,sharing=locked \
    curl --fail --location --silent --show-error \
      --retry 10 \
      --retry-all-errors \
      --retry-delay 2 \
      --connect-timeout 30 \
      --max-time 120 \
      --speed-time 30 \
      --speed-limit 1024 \
      "https://cloud.r-project.org/src/contrib/Archive/renv/renv_${RENV_VERSION}.tar.gz" \
      --output /tmp/renv.tar.gz \
    && printf '%s  %s\n' "${RENV_SHA256}" /tmp/renv.tar.gz \
      | sha256sum --check --strict - \
    || exit 1; \
    restore_status=1; \
    for attempt in 1 2 3 4; do \
      attempt_library="/opt/renv/restore-library-${attempt}"; \
      rm -rf "${attempt_library}"; \
      mkdir -p "${attempt_library}"; \
      if R CMD INSTALL \
          --library="${attempt_library}" \
          /tmp/renv.tar.gz \
        && R_LIBS_USER="${attempt_library}" \
          RENV_PATHS_LIBRARY="${attempt_library}" \
          Rscript renv/bootstrap.R; then \
        rm -rf /opt/renv/library; \
        mv "${attempt_library}" /opt/renv/library; \
        restore_status=0; \
        break; \
      fi; \
      if [ "${attempt}" -lt 4 ]; then \
        sleep_seconds=$((attempt * 5)); \
        printf \
          'renv restore attempt %s failed; retrying in %s seconds\n' \
          "${attempt}" \
          "${sleep_seconds}" \
          >&2; \
        sleep "${sleep_seconds}"; \
      fi; \
    done; \
    rm -rf \
      /opt/renv/restore-library-1 \
      /opt/renv/restore-library-2 \
      /opt/renv/restore-library-3 \
      /opt/renv/restore-library-4; \
    rm -f /tmp/renv.tar.gz; \
    exit "${restore_status}"

ARG ENA3D_BUILD_ID=development
ARG ENA3D_APP_VERSION=0.2.0-dev
ENV ENA3D_BUILD_ID=${ENA3D_BUILD_ID} \
    ENA3D_APP_VERSION=${ENA3D_APP_VERSION}
RUN test -n "${ENA3D_BUILD_ID}" \
    && test "${#ENA3D_BUILD_ID}" -le 128 \
    && test -n "${ENA3D_APP_VERSION}" \
    && test "${#ENA3D_APP_VERSION}" -le 128 \
    && case "${ENA3D_BUILD_ID}" in \
      *[!A-Za-z0-9._-]*) exit 1 ;; \
    esac \
    && case "${ENA3D_APP_VERSION}" in \
      *[!A-Za-z0-9._+-]*) exit 1 ;; \
    esac \
    && install -d -m 0555 /usr/local/share/ena3d/provenance \
    && printf '%s' "${ENA3D_BUILD_ID}" \
      > /usr/local/share/ena3d/provenance/build-id \
    && printf '%s' "${ENA3D_APP_VERSION}" \
      > /usr/local/share/ena3d/provenance/app-version \
    && chmod 0444 \
      /usr/local/share/ena3d/provenance/build-id \
      /usr/local/share/ena3d/provenance/app-version
COPY R ./R
COPY images ./images
COPY sample_data ./sample_data
COPY README.md TRAJECTORY_ANALYSIS.md LICENSE VERSION ./
COPY deploy/shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY deploy/ena3d-entrypoint.sh /usr/local/bin/ena3d-entrypoint
COPY deploy/write-runtime-env.R /usr/local/lib/ena3d/write-runtime-env.R

RUN groupadd --system --gid 10001 ena3d \
    && useradd --system --uid 10001 --gid ena3d --home /home/ena3d ena3d \
    && mkdir -p /home/ena3d /tmp/ena3d \
    && chown -R ena3d:ena3d /home/ena3d /tmp/ena3d \
    && chmod 0555 /usr/local/bin/ena3d-entrypoint \
    && chmod 0444 \
      /etc/shiny-server/shiny-server.conf \
      /usr/local/lib/ena3d/write-runtime-env.R \
    && chmod -R a-w /opt/ena3d/R /opt/ena3d/images /opt/ena3d/sample_data

USER ena3d:ena3d
RUN Rscript -e 'required <- c("shiny", "plotly", "data.table", "R6", "rENA", "bslib", "scales", "digest", "jsonlite", "zip", "readxl", "curl", "callr", "later", "promises", "bit", "bit64"); stopifnot(normalizePath("/opt/renv/library") %in% .libPaths()); stopifnot(all(vapply(required, requireNamespace, logical(1L), quietly=TRUE))); package_paths <- vapply(required, function(package) normalizePath(find.package(package), mustWork=TRUE), character(1L)); stopifnot(all(startsWith(package_paths, "/opt/renv/library/")))'

EXPOSE 3838

HEALTHCHECK --interval=30s --timeout=5s --start-period=60s --retries=3 \
  CMD curl --fail --silent --show-error --max-time 4 http://127.0.0.1:3838/ena3d-health/healthz.json >/dev/null || exit 1

ENTRYPOINT ["/usr/local/bin/ena3d-entrypoint"]
CMD ["/usr/bin/shiny-server", "/etc/shiny-server/shiny-server.conf"]
