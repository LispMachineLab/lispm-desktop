#!/bin/sh
# build-emacs.sh --- build the GNU Emacs LispM runs on GNU/Linux, as a .deb
#
# Copyright (C) 2026  Ryan.K
# SPDX-License-Identifier: AGPL-3.0-or-later
#
#   linux/build-emacs.sh OUT
#
# Builds GNU Emacs on the Debian or Ubuntu it runs on, with what LispM
# uses and distributions' Emacs lacks: xwidgets, the WebKitGTK views
# LispM Browser draws pages in.  Its Lisp is compiled to machine code
# ahead of time.  It is installed under /opt/lispm/emacs, beside any
# Emacs of the system, and packed in OUT as
#
#   lispm-emacs-VERSION-REVISION-SYSTEM-ARCH.deb
#
# as lispm-emacs-31.1-1-debian13-amd64.deb, depending on the libraries
# its programs link to: apt installs them with it.  The workflow
# .github/workflows/emacs-linux.yml runs it in each system's container,
# and install.sh downloads what it built.  It runs as root, as in a
# container, or with sudo for apt.

set -eu

EMACS_VERSION=31.1
EMACS_SHA256=1da5790d9580c81932b5bf700633114468da7b3412d69faa767daebf974f4586
# Counts the builds of one Emacs version: raised when this script builds
# it differently.
REVISION=1
PREFIX=/opt/lispm/emacs

fail() {
  printf 'build-emacs: %s\n' "$*" >&2
  exit 1
}

[ $# = 1 ] || fail "usage: build-emacs.sh OUT"
mkdir -p "$1"
OUT=$(cd "$1" && pwd)

if [ "$(id -u)" = 0 ]; then
  SUDO=
else
  SUDO=sudo
fi

apt_install() {
  $SUDO env DEBIAN_FRONTEND=noninteractive apt-get install -y -q --no-install-recommends "$@"
}

# The system, as SYSTEM names it: debian13, ubuntu24.04.
# shellcheck source=/dev/null
. /etc/os-release
case $ID in
  debian | ubuntu) SYSTEM=$ID$VERSION_ID ;;
  *) fail "this builds on Debian and Ubuntu, not on $ID" ;;
esac

$SUDO apt-get update -q
apt_install ca-certificates curl xz-utils file dpkg-dev build-essential autoconf texinfo pkg-config
# Native compilation uses the libgccjit of the system's own GCC.
GCC=$(gcc -dumpversion | cut -d. -f1)
apt_install "libgccjit-$GCC-dev" \
  libgtk-3-dev libwebkit2gtk-4.1-dev libcairo2-dev librsvg2-dev libharfbuzz-dev \
  libfreetype-dev libfontconfig-dev libxi-dev libxrandr-dev libxinerama-dev libxfixes-dev \
  libxpm-dev libjpeg-dev libpng-dev libgif-dev libtiff-dev libwebp-dev liblcms2-dev \
  libgnutls28-dev libxml2-dev libsqlite3-dev libtree-sitter-dev libgmp-dev \
  libm17n-dev libotf-dev libdbus-1-dev libsystemd-dev libacl1-dev libselinux1-dev \
  libgpm-dev libncurses-dev libasound2-dev zlib1g-dev

WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
cd "$WORK"
# From the first of GNU's servers that answers: the SHA-256 pinned above
# makes any of them as good.
for site in https://ftpmirror.gnu.org/gnu https://ftp.gnu.org/gnu https://mirrors.kernel.org/gnu; do
  curl -fsSL --retry 2 --connect-timeout 30 -o "emacs-$EMACS_VERSION.tar.xz" \
    "$site/emacs/emacs-$EMACS_VERSION.tar.xz" && break
done
echo "$EMACS_SHA256  emacs-$EMACS_VERSION.tar.xz" | sha256sum -c - >/dev/null ||
  fail "emacs-$EMACS_VERSION.tar.xz is not the one expected"
tar xJf "emacs-$EMACS_VERSION.tar.xz"
cd "emacs-$EMACS_VERSION"

./configure --prefix="$PREFIX" --with-x-toolkit=gtk3 --with-xwidgets --with-cairo \
  --with-native-compilation=aot --with-tree-sitter --with-rsvg --with-modules \
  >"$OUT/configure-$SYSTEM.log" 2>&1 || {
  tail -n 30 "$OUT/configure-$SYSTEM.log" >&2
  fail "configure failed"
}
# What LispM needs of it, all found.
for feature in 'support Xwidgets' 'have native lisp compiler' 'use -ltree-sitter' 'use cairo'; do
  grep -q "Does Emacs $feature?  *yes" "$OUT/configure-$SYSTEM.log" ||
    fail "configure left out: $feature"
done
make -j"$(nproc)" >"$OUT/make-$SYSTEM.log" 2>&1 || {
  tail -n 30 "$OUT/make-$SYSTEM.log" >&2
  fail "make failed"
}
src/emacs --batch --eval '(unless (and (featurep (quote xwidget-internal)) (native-comp-available-p)) (kill-emacs 1))' ||
  fail "the Emacs built has no xwidgets or no native compilation"

ROOT=$WORK/root
make install-strip DESTDIR="$ROOT" >>"$OUT/make-$SYSTEM.log" 2>&1

# The package: what its programs link to, as dpkg-shlibdeps finds it in
# the packages installed here; what native compilation of the packages
# LispM installs runs: libgccjit, with GCC's and the C library's start
# files, and the assembler and linker; and the certificates HTTPS is
# checked against, which nothing else brings to a bare Ubuntu 22.04.
ARCH=$(dpkg --print-architecture)
mkdir -p "$WORK/debian"
printf 'Source: lispm-emacs\n\nPackage: lispm-emacs\nArchitecture: any\n' >"$WORK/debian/control"
depends=$(cd "$WORK" && find "$ROOT$PREFIX/bin" "$ROOT$PREFIX/libexec" -type f -exec sh -c 'file -b "$1" | grep -q ^ELF' _ {} \; -print0 |
  xargs -0 dpkg-shlibdeps -O 2>/dev/null | sed -n 's/^shlibs:Depends=//p')
[ -n "$depends" ] || fail "dpkg-shlibdeps found no dependencies"
mkdir -p "$ROOT/DEBIAN"
cat >"$ROOT/DEBIAN/control" <<EOF
Package: lispm-emacs
Version: $EMACS_VERSION-$REVISION~$SYSTEM
Architecture: $ARCH
Maintainer: LispMachineLab <https://github.com/LispMachineLab>
Installed-Size: $(du -sk "$ROOT$PREFIX" | cut -f1)
Depends: $depends, libgccjit-$GCC-dev, binutils, libc6-dev, ca-certificates
Section: editors
Priority: optional
Homepage: https://lispm.ai
Description: GNU Emacs $EMACS_VERSION for LispM
 GNU Emacs $EMACS_VERSION as LispM runs on it: with xwidgets, for LispM
 Browser, tree-sitter, and its Lisp compiled to machine code.  It is
 installed in $PREFIX, beside any Emacs of the system.
 .
 GNU Emacs is free software under the GNU General Public License, version
 3 or later; its source is at https://ftp.gnu.org/gnu/emacs/.
EOF
deb=$OUT/lispm-emacs-$EMACS_VERSION-$REVISION-$SYSTEM-$ARCH.deb
dpkg-deb --root-owner-group -Zxz --build "$ROOT" "$deb" >/dev/null
dpkg-deb --info "$deb" | sed -n 's/^ \(Version\|Depends\|Installed-Size\):/&/p'
echo "$deb"
