#!/bin/sh
set -eu

umask 077

provenance_error() {
  printf '%s\n' \
    'ena3d: runtime provenance does not match image provenance; refusing to start' \
    >&2
  exit 78
}

build_id_file=/usr/local/share/ena3d/provenance/build-id
app_version_file=/usr/local/share/ena3d/provenance/app-version

[ -r "${build_id_file}" ] || provenance_error
[ -r "${app_version_file}" ] || provenance_error

image_build_id=$(cat "${build_id_file}")
image_app_version=$(cat "${app_version_file}")

[ "${ENA3D_BUILD_ID-}" = "${image_build_id}" ] || provenance_error
[ "${ENA3D_APP_VERSION-}" = "${image_app_version}" ] || provenance_error

ENA3D_BUILD_ID=${image_build_id}
ENA3D_APP_VERSION=${image_app_version}
export ENA3D_BUILD_ID ENA3D_APP_VERSION

home_dir=/home/ena3d
worker_profile="${home_dir}/.Rprofile"
mkdir -p "${home_dir}"

# Shiny Server intentionally gives R workers a minimal environment. Bridge
# only reviewed, non-secret settings into R's user environment. In particular,
# never copy DASHSCOPE_API_KEY; the application receives only the mounted
# secret-file path.
Rscript --vanilla /usr/local/lib/ena3d/write-runtime-env.R "${worker_profile}"

mkdir -p \
  /tmp/ena3d/sockets \
  /tmp/ena3d/bookmarks \
  /tmp/ena3d/logs
chmod 0700 \
  /tmp/ena3d \
  /tmp/ena3d/sockets \
  /tmp/ena3d/bookmarks \
  /tmp/ena3d/logs

exec "$@"
