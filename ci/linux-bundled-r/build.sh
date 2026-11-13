#!/bin/bash
# Build a relocatable R with precompiled MIRO packages that runs on any distribution with glibc >= 2.28.
# Run from the repository root in almalinux:8 (manylinux_2_28 baseline) after `r/library_src` was populated.
# Result: dist/miro-r-linux-$(uname -m).tar.gz containing `r/` (R_HOME incl. MIRO packages and bundled shared libs).
set -euo pipefail

REPO=$(pwd)
OUT=${OUT:-$REPO/dist}
config() { sed -n "s/.*\"$1\": *\"\([^\"]*\)\".*/\1/p" build-config.json; }
R_VERSION=$(config rVersion)
PREFIX=/opt/miro-r
DEPS=/opt/miro-deps
WORK=$(mktemp -d)
TOOLSET=gcc-toolset-13

dnf install -y dnf-plugins-core epel-release
dnf config-manager --set-enabled powertools
dnf install -y $TOOLSET $TOOLSET-gcc-gfortran make perl file tzdata patchelf which \
  pkgconf-pkg-config libpng-devel libxml2-devel pcre2-devel \
  xz-devel bzip2-devel zlib-devel libicu-devel cairo-devel pango-devel
# newer compiler that links the parts of libstdc++ missing from the system's GCC 8 statically
source /opt/rh/$TOOLSET/enable

# OpenSSL 3 and a libcurl linked against it instead of the system's OpenSSL 1.1.1
curl -fsSL "https://www.openssl.org/source/openssl-$(config opensslVersion).tar.gz" | tar -xz -C "$WORK"
pushd "$WORK/openssl-$(config opensslVersion)" >/dev/null
./Configure --prefix=$DEPS --libdir=lib shared no-module no-tests no-docs
make -j"$(nproc)"
make install_sw
popd >/dev/null
curl -fsSL "https://curl.se/download/curl-$(config curlVersion).tar.gz" | tar -xz -C "$WORK"
pushd "$WORK/curl-$(config curlVersion)" >/dev/null
./configure --prefix=$DEPS --with-openssl=$DEPS --disable-static --disable-ldap --without-libpsl \
  --without-libidn2 --without-nghttp2 --without-brotli --without-zstd
make -j"$(nproc)"
make install
popd >/dev/null
export PATH=$DEPS/bin:$PATH PKG_CONFIG_PATH=$DEPS/lib/pkgconfig${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH} \
  LD_LIBRARY_PATH=$DEPS/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}

curl -fsSL "https://cloud.r-project.org/src/base/R-${R_VERSION%%.*}/R-${R_VERSION}.tar.gz" | tar -xz -C "$WORK"
pushd "$WORK/R-${R_VERSION}" >/dev/null
./configure --prefix=$PREFIX LIBnn=lib CPPFLAGS=-I$DEPS/include LDFLAGS=-L$DEPS/lib --with-x=no --with-readline=no --disable-java
make -j"$(nproc)"
make install
popd >/dev/null

RHOME=$WORK/r
cp -a $PREFIX/lib/R "$RHOME"
rm -rf $PREFIX # anything still pointing to the prefix fails from here on

# bin/R hardcodes the install prefix; derive R_HOME from the script location instead
sed -i -e '/^if test "${R_HOME_DIR}" = /,/^fi$/d' \
  -e "s|^R_HOME_DIR=.*|R_HOME_DIR=\$(cd \"\$(dirname \"\$0\")/..\" \&\& pwd -P)|" \
  -e "s|$PREFIX/lib/R|\${R_HOME_DIR}|g" "$RHOME/bin/R"
# the bundled libcurl/OpenSSL look for CA certificates at build-time paths; use the distribution's bundle
cat >>"$RHOME/etc/ldpaths" <<'EOF'
for ca in /etc/ssl/certs/ca-certificates.crt /etc/pki/tls/certs/ca-bundle.crt /etc/ssl/ca-bundle.pem /etc/ssl/cert.pem; do
  if [ -f "$ca" ]; then
    CURL_CA_BUNDLE=${CURL_CA_BUNDLE:-$ca}
    SSL_CERT_FILE=${SSL_CERT_FILE:-$ca}
    export CURL_CA_BUNDLE SSL_CERT_FILE
    break
  fi
done
EOF
# Rscript has R_HOME compiled in but honours RHOME
mv "$RHOME/bin/Rscript" "$RHOME/bin/Rscript.bin"
cat >"$RHOME/bin/Rscript" <<'EOF'
#!/bin/sh
RHOME=$(cd "$(dirname "$0")/.." && pwd -P) exec "$(dirname "$0")/Rscript.bin" "$@"
EOF
chmod +x "$RHOME/bin/Rscript"

# precompile MIRO packages exactly like the launcher does on first start, but into R_HOME/library
mkdir -p "$WORK/repo"
cp -a --parents scripts build/scripts build-config.json r/library_src src/miro-pkg-lock.csv "$WORK/repo/"
pushd "$WORK/repo" >/dev/null
"$RHOME/bin/Rscript" -e 'eval(parse("build/scripts/install-packages.R")[[1]])' # regenerates globals.R from the lock file
LIB_PATH="$RHOME/library" SCRIPTS_PATH="$WORK/repo/scripts" "$RHOME/bin/Rscript" scripts/install_source.R
popd >/dev/null

rm -rf "$RHOME"/doc "$RHOME"/library/*/{help,doc,html,tests,include}
find "$RHOME" -type f \( -name '*.so' -o -path '*/bin/exec/*' \) -exec strip --strip-unneeded {} +

# bundle every shared library outside glibc/libgcc/libstdc++ and find it via RPATH rather than
# LD_LIBRARY_PATH, so processes spawned by R (GAMS, Python) keep using the system libraries
SYSTEM_LIBS='^(linux-vdso|ld-linux|libc|libm|libdl|libpthread|librt|libutil|libresolv|libgcc_s|libstdc\+\+)\.so'
BUNDLED=$RHOME/lib/bundled
mkdir -p "$BUNDLED"
mapfile -t ELF_FILES < <(find "$RHOME" -type f -exec file {} + | grep -E 'ELF .*(shared object|executable)' | cut -d: -f1)
printf '%s\0' "${ELF_FILES[@]}" | LD_LIBRARY_PATH=$RHOME/lib:$DEPS/lib xargs -0 ldd 2>/dev/null |
  awk '/=> \// {print $1, $3}' | sort -u |
  while read -r name path; do
    [[ $name =~ $SYSTEM_LIBS ]] || [[ $path == "$RHOME"/* ]] || cp -L "$path" "$BUNDLED/$name"
  done
if ls "$BUNDLED" | grep -E '^lib(ssl|crypto)\.so\.1'; then
  echo "OpenSSL 1.1 must not be bundled" >&2
  exit 1
fi
for elf in "${ELF_FILES[@]}" "$BUNDLED"/*; do
  patchelf --force-rpath --set-rpath "\$ORIGIN/$(realpath --relative-to="$(dirname "$elf")" "$BUNDLED")" "$elf"
done

mkdir -p "$OUT"
tar -C "$WORK" -czf "$OUT/miro-r-linux-$(uname -m).tar.gz" r
du -sh "$RHOME" "$OUT/miro-r-linux-$(uname -m).tar.gz"
