#!/bin/sh
# install.sh --- install LispM on macOS, or bring it up to date
#
# Copyright (C) 2026  Ryan.K
# SPDX-License-Identifier: GPL-3.0-or-later
#
#   curl -fsSL https://lispmachinelab.github.io/install.sh | sh
#
# LispM.app is GNU Emacs, under LispM's name and icon, running LispM
# Desktop: nothing needs to be known about Emacs to use it.  This script
#
#   1. installs Apple's command line tools, when they are missing (git,
#      and the compiler Emacs turns its packages into machine code with);
#   2. checks out lispm-desktop, lispm-agent and lispm-browser side by side
#      in ~/.lispm, or updates them;
#   3. downloads GNU Emacs, as built, signed and notarized by
#      https://github.com/jimeh/emacs-builds, checks it, and makes
#      LispM.app of it in /Applications (else ~/Applications);
#   4. installs the Emacs packages LispM Desktop uses, so that the first
#      start has nothing to fetch;
#   5. opens LispM.
#
# Run it again to update.  The checkouts keep what Emacs writes too, the
# packages and your settings, and ~/.lispm/skills holds the skills you
# wrote for LispM's agents, if any; uninstalling is
#
#   rm -rf /Applications/LispM.app ~/.lispm
#
# The environment changes where things go:
#
#   LISPM_HOME     the checkouts, ~/.lispm by default
#   LISPM_APP_DIR  the folder LispM.app goes in
#   LISPM_OPEN     0 to leave LispM closed at the end
#
# Everything is in functions, called on the last line, so that a download
# cut short runs nothing.

set -eu

# The Emacs LispM.app is made of, its build for each processor, and the
# Developer ID it is signed with.
EMACS_RELEASE=Emacs-31.1
EMACS_BUILD=Emacs.2026-08-24.a360712.emacs-31-1.macOS-11
EMACS_SHA256_ARM64=41d118033eb766f14f853c2cdbce7a27cfed99fe244c7d9c9552a8dc22508480
EMACS_SHA256_X86_64=83fb3864548eea3803dff2081d9736fbad57e14feb3df1ce5f158bd7834eb028
EMACS_TEAM=5HX66GF82Z

REPOSITORIES=https://github.com/LispMachineLab
APP_ID=org.lispmachinelab.lispm
LSREGISTER=/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister

if [ -t 1 ]; then
  BOLD=$(printf '\033[1m') DIM=$(printf '\033[2m') PLAIN=$(printf '\033[0m')
else
  BOLD='' DIM='' PLAIN=''
fi

say() {
  printf '%s==>%s %s%s%s\n' "$DIM" "$PLAIN" "$BOLD" "$*" "$PLAIN"
}

note() {
  printf '    %s\n' "$*"
}

fail() {
  printf 'LispM: %s\n' "$*" >&2
  exit 1
}

# Settle the processor and the system, or stop.
check_system() {
  [ "$(uname -s)" = Darwin ] ||
    fail "this installer is for macOS.  On GNU/Linux, see https://github.com/LispMachineLab/lispm-desktop#installation"
  ARCH=$(uname -m)
  # A shell run by Rosetta reports an Intel processor on Apple silicon.
  if [ "$ARCH" = x86_64 ] && [ "$(sysctl -n hw.optional.arm64 2>/dev/null || true)" = 1 ]; then
    ARCH=arm64
  fi
  case $ARCH in
    arm64) EMACS_SHA256=$EMACS_SHA256_ARM64 ;;
    x86_64) EMACS_SHA256=$EMACS_SHA256_X86_64 ;;
    *) fail "LispM runs on Apple silicon and Intel Macs, not on $ARCH" ;;
  esac
  major=$(sw_vers -productVersion | cut -d. -f1)
  [ "$major" -ge 11 ] || fail "LispM needs macOS 11 Big Sur or later"
}

# Install Apple's command line tools, when they are missing, and wait for
# them.
check_tools() {
  if ! xcode-select -p >/dev/null 2>&1; then
    say "Installing Apple's command line tools"
    note "Click Install in the window that opens; this goes on once they are in."
    xcode-select --install >/dev/null 2>&1 || true
    until xcode-select -p >/dev/null 2>&1; do
      sleep 5
    done
  fi
  git --version >/dev/null 2>&1 || fail "git does not run; reinstall the command line tools with xcode-select --install"
}

# Choose where things go.
settle_places() {
  LISPM_HOME=${LISPM_HOME:-$HOME/.lispm}
  mkdir -p "$LISPM_HOME"
  LISPM_HOME=$(cd "$LISPM_HOME" && pwd)
  DESKTOP=$LISPM_HOME/lispm-desktop
  if [ -n "${LISPM_APP_DIR:-}" ]; then
    APP_DIR=$LISPM_APP_DIR
  elif [ -w /Applications ]; then
    APP_DIR=/Applications
  else
    APP_DIR=$HOME/Applications
  fi
  APP=$APP_DIR/LispM.app
  # The launcher finds the checkout through the home directory of whoever
  # opens LispM, when it is in the installer's: it expands the "~/".
  # shellcheck disable=SC2088
  case $DESKTOP in
    "$HOME"/*) INIT_DIRECTORY="~/${DESKTOP#"$HOME"/}" ;;
    *) INIT_DIRECTORY=$DESKTOP ;;
  esac
  if pgrep -f "$APP/Contents/MacOS/Emacs" >/dev/null 2>&1; then
    fail "LispM is running: quit it (Command-Q), then run this again"
  fi
}

# Check out the repository NAME in LISPM_HOME, or update the checkout
# there, unless it is on no branch or has changes of its own.
fetch_project() {
  dir=$LISPM_HOME/$1
  if [ -d "$dir/.git" ]; then
    if git -C "$dir" rev-parse -q --verify '@{upstream}' >/dev/null 2>&1; then
      git -C "$dir" pull -q --ff-only ||
        note "$1 has changes of its own, so it was left as it is"
    fi
  elif [ -e "$dir" ]; then
    fail "$dir is in the way: move it, then run this again"
  else
    git clone -q --depth 1 "$REPOSITORIES/$1.git" "$dir"
  fi
}

fetch_projects() {
  say "Fetching LispM into $LISPM_HOME"
  for project in lispm-desktop lispm-agent lispm-browser; do
    fetch_project "$project"
  done
}

# Print the Emacs build LispM.app at APP was made of, or nothing.
installed_build() {
  /usr/libexec/PlistBuddy -c 'Print :LispMEmacsBuild' "$APP/Contents/Info.plist" 2>/dev/null || true
}

# Put Emacs.app, checked, at $WORK/LispM.app: the one in LispM.app when it
# is the build wanted, else downloaded.
fetch_emacs() {
  wanted=$EMACS_BUILD.$ARCH
  if [ "$(installed_build)" = "$wanted" ]; then
    ditto "$APP" "$WORK/LispM.app"
    return
  fi
  say "Downloading GNU Emacs ($EMACS_RELEASE, about 190 MB)"
  dmg=$WORK/emacs.dmg
  curl -fL --progress-bar -o "$dmg" \
    "https://github.com/jimeh/emacs-builds/releases/download/$EMACS_RELEASE/$wanted.dmg"
  [ "$(shasum -a 256 "$dmg" | cut -d' ' -f1)" = "$EMACS_SHA256" ] ||
    fail "the Emacs downloaded is not the one expected; nothing was installed"
  MOUNT=$WORK/emacs
  mkdir "$MOUNT"
  hdiutil attach -quiet -nobrowse -readonly -mountpoint "$MOUNT" "$dmg" 2>"$WORK/hdiutil.log" ||
    fail "the Emacs disk image does not open: $(cat "$WORK/hdiutil.log")"
  codesign --verify --strict "$MOUNT/Emacs.app" 2>/dev/null ||
    fail "Emacs's signature does not hold; nothing was installed"
  team=$(codesign -dv "$MOUNT/Emacs.app" 2>&1 | sed -n 's/^TeamIdentifier=//p')
  [ "$team" = "$EMACS_TEAM" ] || fail "Emacs is signed by $team, not by its builder; nothing was installed"
  ditto "$MOUNT/Emacs.app" "$WORK/LispM.app"
  hdiutil detach -quiet "$MOUNT" || true
  MOUNT=
}

# Set KEY of the property list PLIST to the string VALUE.
plist_set() {
  /usr/libexec/PlistBuddy -c "Delete :$2" "$1" >/dev/null 2>&1 || true
  /usr/libexec/PlistBuddy -c "Add :$2 string $3" "$1"
}

# Make Emacs.app at $WORK/LispM.app into LispM.app: LispM's name, icon and
# identity, the launcher as its executable, signed for this Mac.
make_app() {
  say "Making LispM.app"
  contents=$WORK/LispM.app/Contents
  plist=$contents/Info.plist
  plist_set "$plist" CFBundleName LispM
  plist_set "$plist" CFBundleDisplayName LispM
  plist_set "$plist" CFBundleIdentifier "$APP_ID"
  plist_set "$plist" CFBundleExecutable LispM
  plist_set "$plist" CFBundleIconFile LispM.icns
  plist_set "$plist" CFBundleGetInfoString "LispM, on GNU Emacs ${EMACS_RELEASE#Emacs-}"
  plist_set "$plist" LispMEmacsBuild "$EMACS_BUILD.$ARCH"
  # Emacs's icon from the asset catalog would win over LispM's.
  /usr/libexec/PlistBuddy -c 'Delete :CFBundleIconName' "$plist" >/dev/null 2>&1 || true
  for key in NSAppleEventsUsageDescription NSDesktopFolderUsageDescription \
    NSDocumentsFolderUsageDescription NSDownloadsFolderUsageDescription \
    NSRemovableVolumesUsageDescription NSSpeechRecognitionUsageDescription; do
    text=$(/usr/libexec/PlistBuddy -c "Print :$key" "$plist" 2>/dev/null) || continue
    plist_set "$plist" "$key" "$(printf '%s' "$text" | sed 's/^Emacs requires/LispM requires/')"
  done
  i=0
  while title=$(/usr/libexec/PlistBuddy -c "Print :NSServices:$i:NSMenuItem:default" "$plist" 2>/dev/null); do
    plist_set "$plist" "NSServices:$i:NSMenuItem:default" "$(printf '%s' "$title" | sed 's|^Emacs\.app/|LispM/|')"
    i=$((i + 1))
  done
  cp "$DESKTOP/macos/LispM.icns" "$contents/Resources/LispM.icns"
  cc -O2 -Wall -Wextra -DLISPM_INIT_DIRECTORY="\"$INIT_DIRECTORY\"" \
    -mmacosx-version-min=11.0 -o "$contents/MacOS/LispM" "$DESKTOP/macos/launcher.c"
  # Emacs's own signature covered the property list changed above, so the
  # application is signed again, for this Mac only.
  { codesign --force --sign - --identifier "$APP_ID.emacs" "$contents/MacOS/Emacs" &&
    codesign --force --sign - "$WORK/LispM.app" &&
    codesign --verify --strict "$WORK/LispM.app"; } 2>"$WORK/codesign.log" ||
    fail "LispM.app could not be signed: $(cat "$WORK/codesign.log")"
}

install_app() {
  mkdir -p "$APP_DIR"
  rm -rf "$APP"
  mv "$WORK/LispM.app" "$APP"
  "$LSREGISTER" -f "$APP" >/dev/null 2>&1 || true
}

# Install the packages LispM Desktop uses, by starting it once without a
# display, as Emacs starts (a batch Emacs leaves the packages installed
# before inactive); the first start then asks only how you like to work.
install_packages() {
  say "Installing LispM's Emacs packages (a few minutes the first time)"
  log=$LISPM_HOME/install.log
  if ! "$APP/Contents/MacOS/Emacs" --batch --init-directory="$DESKTOP" \
    -l "$DESKTOP/early-init.el" --eval '(when package-enable-at-startup (package-activate-all))' \
    -l "$DESKTOP/init.el" >"$log" 2>&1; then
    tail -n 20 "$log" >&2
    fail "the packages could not all be installed; the whole log is in $log"
  fi
}

# Build the native modules where their compilers are: the browser's,
# for Chrome extensions, with the command line tools' Swift; the
# terminals', with Rust's Cargo, which LispM can also do later.
build_modules() {
  webext=$LISPM_HOME/lispm-browser/native/lispm-browser-webext
  version=$(sw_vers -productVersion)
  if [ -d "$webext" ] && command -v swift >/dev/null 2>&1 &&
    [ "$(printf '15.4\n%s\n' "$version" | sort -t. -k1,1n -k2,2n | head -n 1)" = 15.4 ]; then
    say "Building the browser's support for Chrome extensions"
    (cd "$webext" && swift build -c release --product lispm_browser_webext >"$LISPM_HOME/webext.log" 2>&1) ||
      note "It did not build (see $LISPM_HOME/webext.log); LispM tries again when you add an extension."
  fi
  if command -v cargo >/dev/null 2>&1; then
    say "Building LispM's terminals"
    make -C "$DESKTOP" native >"$LISPM_HOME/terminal.log" 2>&1 ||
      note "They did not build (see $LISPM_HOME/terminal.log); LispM tries again at its next start."
  fi
}

cleanup() {
  if [ -n "${MOUNT:-}" ]; then
    hdiutil detach -quiet -force "$MOUNT" 2>/dev/null || true
  fi
  if [ -n "${WORK:-}" ]; then
    rm -rf "$WORK"
  fi
}

main() {
  check_system
  check_tools
  settle_places
  fetch_projects
  WORK=$(mktemp -d "${TMPDIR:-/tmp}/lispm.XXXXXX")
  MOUNT=
  trap cleanup EXIT
  trap 'exit 130' INT TERM
  fetch_emacs
  make_app
  install_app
  install_packages
  build_modules
  say "LispM is in $APP_DIR"
  if [ "${LISPM_OPEN:-1}" != 0 ]; then
    note "Opening it now; next time, open it from Launchpad, Spotlight or the Dock."
    open "$APP"
  fi
}

main "$@"
