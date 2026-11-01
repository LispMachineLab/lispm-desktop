#!/bin/sh
# install.sh --- install LispM on macOS or GNU/Linux, or bring it up to date
#
# Copyright (C) 2026  Ryan.K
# SPDX-License-Identifier: AGPL-3.0-or-later
#
#   curl -fsSL https://lispm.ai/install.sh | sh
#
# On GNU/Linux, Debian 12 and 13 and Ubuntu 22.04 and 24.04 (and the
# systems made from them), it installs GNU Emacs 31.1 with xwidgets, as
# linux/build-emacs.sh builds it, in /opt/lispm/emacs with apt, checks
# out LispM in ~/.lispm, installs its packages, and adds the command
# lispm and LispM to the applications menu.  Uninstalling is
#
#   sudo apt remove lispm-emacs
#   rm -rf ~/.lispm ~/.local/bin/lispm ~/.local/share/applications/lispm.desktop
#
# On macOS:
#
# LispM.app is GNU Emacs, under LispM's name and icon, running LispM
# Desktop: nothing needs to be known about Emacs to use it.  This script
#
#   1. installs Apple's command line tools, when they are missing (git,
#      and the compiler Emacs turns its packages into machine code with);
#   2. checks out the latest release of lispm-desktop, lispm-agent,
#      lispm-browser, lispm-kit and lispm-ui side by side in ~/.lispm, or
#      updates them to it;
#   3. downloads GNU Emacs, as built, signed and notarized by
#      https://github.com/jimeh/emacs-builds, checks it, and makes
#      LispM.app of it in /Applications (else ~/Applications);
#   4. installs the Emacs packages LispM Desktop uses, and the native
#      modules of its terminals and browser, so that the first start has
#      nothing to fetch or build;
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
#   LISPM_EMACS    the GNU Emacs LispM.app is made of: 31, the default, or
#                  32, experimental (see below); by default, the one
#                  LispM.app has, when it is there
#
# For example, the experimental LispM:
#
#   curl -fsSL https://lispm.ai/install.sh | LISPM_EMACS=32 sh
#
# LispM's Kernel panel switches LispM.app to another Emacs in two steps,
# with LISPM_STEP:
#
#   stage   makes LispM.app of LISPM_EMACS's Emacs in ~/.lispm/staging,
#           LispM running on: downloaded and checked, or the one kept in
#           ~/.lispm/backup when it is that Emacs
#   switch  once LispM has quit, keeps LispM.app in ~/.lispm/backup, puts
#           the staged one in its place, and opens it
#
# Everything is in functions, called on the last line, so that a download
# cut short runs nothing.

set -eu

# The Emacs LispM.app is made of, by default: its release, its build for
# each processor, and the version it is.
EMACS_31_RELEASE=Emacs-31.1
EMACS_31_BUILD=Emacs.2026-08-24.a360712.emacs-31-1.macOS-11
EMACS_31_SHA256_ARM64=41d118033eb766f14f853c2cdbce7a27cfed99fe244c7d9c9552a8dc22508480
EMACS_31_SHA256_X86_64=83fb3864548eea3803dff2081d9736fbad57e14feb3df1ce5f158bd7834eb028
EMACS_31_VERSION=31.1

# The experimental one: Emacs 32 is not released, so this is a build of
# its development branch, master, as of the day it was tried with LispM.
# What only Emacs 32 can do, as OpenGL drawn in a buffer, is LispM's on it
# alone.
EMACS_32_RELEASE=Emacs.2026-10-21.3354d07.master
EMACS_32_BUILD=Emacs.2026-10-21.3354d07.master.macOS-11
EMACS_32_SHA256_ARM64=28c58868313880b16d483da8534ec983d6ac60c5a8532e5a0f3ef3c16d222ba7
EMACS_32_SHA256_X86_64=85ee0efdc9e88a35b4fc1677d8c0282ae1c6ca958087d1cf8577921c40e96c1f
EMACS_32_VERSION=32.0.50

# The Developer ID both are signed with.
EMACS_TEAM=5HX66GF82Z

# The Emacs of GNU/Linux: GNU Emacs 31.1 with xwidgets, built by
# linux/build-emacs.sh and published by the workflow emacs-linux.yml as
# a release of lispm-desktop, a .deb for each system and processor,
# whose SHA-256 is pinned here.
LINUX_EMACS_RELEASE=emacs-31.1-1-linux
LINUX_EMACS_VERSION=31.1-1
LINUX_EMACS_SHA256S='
debian12-amd64 b3176796b4f30f148e9bd9bac83f82d437648830d6c4a07dfa90dfdc407eb83b
debian12-arm64 36a479d81e6eaf670f7c725592a46e4290db8c6ca010ecb468793725582622c4
debian13-amd64 3c92e308ac5d0605f21b79143cd2e10bc44a0a470635ddb6e47cc79a7b9d4a4d
debian13-arm64 793a9a6d4a0008c315e2d0f1c30c605473a4e5f1d1d5d8f98b57b11749714230
ubuntu22.04-amd64 483e207adf740602d8eb5c3254c9696e492ad9606108520bfb3c47a062d9674f
ubuntu22.04-arm64 40d1c767158740f081a97cfb8d9efc50b5c899c420acbebd3f10b65d5e2a0a75
ubuntu24.04-amd64 99ed618c7bcb7402f25d6dc0859684d66df11105d0c9dac348038950d81a49b0
ubuntu24.04-arm64 1d5919fe1ccecdc5755b8c89e7015b23aa5caad7e255a44367325e0ba6288b6e
'
LINUX_EMACS=/opt/lispm/emacs/bin/emacs

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
  case $(uname -s) in
    Darwin) OS=macos && check_mac ;;
    Linux) OS=linux && check_linux ;;
    *) fail "LispM installs on macOS and GNU/Linux, not on $(uname -s)" ;;
  esac
}

check_mac() {
  ARCH=$(uname -m)
  # A shell run by Rosetta reports an Intel processor on Apple silicon.
  if [ "$ARCH" = x86_64 ] && [ "$(sysctl -n hw.optional.arm64 2>/dev/null || true)" = 1 ]; then
    ARCH=arm64
  fi
  case $ARCH in
    arm64 | x86_64) ;;
    *) fail "LispM runs on Apple silicon and Intel Macs, not on $ARCH" ;;
  esac
  major=$(sw_vers -productVersion | cut -d. -f1)
  [ "$major" -ge 11 ] || fail "LispM needs macOS 11 Big Sur or later"
}

check_linux() {
  { command -v dpkg && command -v apt-get; } >/dev/null 2>&1 ||
    fail "on GNU/Linux this installer is for Debian and Ubuntu; elsewhere, see https://github.com/LispMachineLab/lispm-desktop#installation"
  ARCH=$(dpkg --print-architecture)
  case $ARCH in
    amd64 | arm64) ;;
    *) fail "LispM runs on x86_64 (amd64) and ARM (arm64) processors, not on $ARCH" ;;
  esac
  SYSTEM=$(linux_system)
  case $SYSTEM in
    debian12 | debian13 | ubuntu22.04 | ubuntu24.04) ;;
    # Elsewhere, an Emacs built here with linux/build-emacs.sh.
    *) [ -n "${LISPM_EMACS_DEB:-}" ] ||
      fail "LispM's Emacs is built for Debian 12 and 13 and Ubuntu 22.04 and 24.04, not for $SYSTEM; see https://github.com/LispMachineLab/lispm-desktop#on-gnulinux" ;;
  esac
  case ${LISPM_EMACS:-31} in
    31) ;;
    *) fail "on GNU/Linux LispM runs on Emacs 31, not on $LISPM_EMACS" ;;
  esac
  if [ "$(id -u)" = 0 ]; then
    SUDO=
  elif command -v sudo >/dev/null 2>&1; then
    SUDO=sudo
  else
    fail "installing GNU Emacs takes sudo, which is not installed"
  fi
}

# Print the system as LispM's Emacs builds name it, as debian13 or
# ubuntu24.04: one made from Ubuntu, as Linux Mint, by the Ubuntu it is
# made from, and one made from Debian by its Debian.
linux_system() {
  (
    # shellcheck source=/dev/null
    . /etc/os-release
    case ${UBUNTU_CODENAME:-} in
      jammy) echo ubuntu22.04 && exit ;;
      noble) echo ubuntu24.04 && exit ;;
    esac
    case $ID in
      debian | ubuntu) echo "$ID${VERSION_ID:-}" && exit ;;
    esac
    case ${DEBIAN_CODENAME:-${VERSION_CODENAME:-}} in
      bookworm) echo debian12 ;;
      trixie) echo debian13 ;;
      *) echo "$ID${VERSION_ID:-}" ;;
    esac
  )
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
  STAGED=$LISPM_HOME/staging/LispM.app
  BACKUP=$LISPM_HOME/backup/LispM.app
  if [ "$OS" = linux ]; then
    EMACS_PROGRAM=$LINUX_EMACS
  else
    EMACS_PROGRAM=$APP/Contents/MacOS/Emacs
  fi
  # The launcher finds the checkout through the home directory of whoever
  # opens LispM, when it is in the installer's: it expands the "~/".
  # shellcheck disable=SC2088
  case $DESKTOP in
    "$HOME"/*) INIT_DIRECTORY="~/${DESKTOP#"$HOME"/}" ;;
    *) INIT_DIRECTORY=$DESKTOP ;;
  esac
}

lispm_running() {
  if [ "$OS" = linux ]; then
    pgrep -f -- "--init-directory=$DESKTOP" >/dev/null 2>&1
  else
    pgrep -f "$APP/Contents/MacOS/Emacs" >/dev/null 2>&1
  fi
}

refuse_running() {
  if lispm_running; then
    fail "LispM is running: quit it, then run this again"
  fi
}

# Print the major version of the Emacs the LispM.app at APP, or at $1,
# was made of, or nothing: a build of master is 32's, the version master
# is now.
installed_emacs() {
  case $(installed_build "${1:-}") in
    *.master.*) echo 32 ;;
    *.emacs-[0-9]*) installed_build "${1:-}" | sed 's/.*\.emacs-\([0-9]*\)-.*/\1/' ;;
  esac
}

# Settle the Emacs LispM.app is made of: LISPM_EMACS's, else the one it
# has, else 31.
choose_emacs() {
  EMACS_BEFORE=$(installed_emacs)
  EMACS=${LISPM_EMACS:-${EMACS_BEFORE:-31}}
  case $EMACS in
    31)
      EMACS_RELEASE=$EMACS_31_RELEASE EMACS_BUILD=$EMACS_31_BUILD EMACS_VERSION=$EMACS_31_VERSION
      EMACS_NAME="GNU Emacs $EMACS_VERSION"
      ;;
    32)
      EMACS_RELEASE=$EMACS_32_RELEASE EMACS_BUILD=$EMACS_32_BUILD EMACS_VERSION=$EMACS_32_VERSION
      EMACS_NAME="GNU Emacs 32, in development"
      ;;
    *) fail "LISPM_EMACS is 31 or 32, not $EMACS" ;;
  esac
  case $EMACS.$ARCH in
    31.arm64) EMACS_SHA256=$EMACS_31_SHA256_ARM64 ;;
    31.x86_64) EMACS_SHA256=$EMACS_31_SHA256_X86_64 ;;
    32.arm64) EMACS_SHA256=$EMACS_32_SHA256_ARM64 ;;
    32.x86_64) EMACS_SHA256=$EMACS_32_SHA256_X86_64 ;;
  esac
  if [ "$EMACS" = 32 ]; then
    say "This is the experimental LispM, on $EMACS_NAME"
    note "Back to the one on Emacs 31: curl -fsSL https://lispm.ai/install.sh | LISPM_EMACS=31 sh"
  fi
}

# Print the tags of the releases of the repository NAME, the latest
# first.  A release is a tag vX.Y.Z of its main branch; Software Update
# follows them too.
releases() {
  git ls-remote --tags --refs --sort=-v:refname "$REPOSITORIES/$1.git" 'v*' |
    sed -n 's|.*refs/tags/\(v[0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*\)$|\1|p'
}

# Print the tag of the latest release of the repository NAME, as v0.2.0,
# or nothing before its first.
latest_release() {
  releases "$1" | head -n 1
}

# Fetch the branch the checkout DIR of the repository NAME follows, with
# the tags of its commits, not every tag and the history of each.  A
# checkout cloned shallow stays so: fetching into one, git counts the
# commits before its own as missing, so that a branch merged since that
# forked before them would bring in all the history behind the fork.
# Leaving out what the release before the checkout's own holds cuts such
# a branch at its own commits, as Software Update does.
fetch_branch() {
  before=
  if [ "$(git -C "$2" rev-parse --is-shallow-repository)" = true ] &&
    own=$(git -C "$2" describe --tags --abbrev=0 --match 'v[0-9]*' --exclude '*-*' 2>/dev/null); then
    before=$(releases "$1" | awk -v own="$own" 'found { print; exit } $0 == own { found = 1 }')
  fi
  git -C "$2" fetch -q ${before:+"--shallow-exclude=$before"}
}

# Check out the latest release of the repository NAME in LISPM_HOME, on
# main, or move the checkout there on to it, unless it holds the release
# or newer commits already, is on no branch, or has changes of its own.
# Before the first release, main is checked out, and followed.
fetch_project() {
  dir=$LISPM_HOME/$1
  release=$(latest_release "$1")
  if [ -d "$dir/.git" ]; then
    if ! git -C "$dir" rev-parse -q --verify '@{upstream}' >/dev/null 2>&1; then
      :
    elif [ -z "$release" ]; then
      git -C "$dir" pull -q --ff-only ||
        note "$1 has changes of its own, so it was left as it is"
    else
      fetch_branch "$1" "$dir" &&
        { git -C "$dir" merge-base --is-ancestor "$release" HEAD ||
            git -C "$dir" merge -q --ff-only "$release" 2>/dev/null; } ||
        note "$1 has changes of its own, so it was left as it is"
    fi
  elif [ -e "$dir" ]; then
    fail "$dir is in the way: move it, then run this again"
  elif [ -z "$release" ]; then
    git clone -q --depth 1 "$REPOSITORIES/$1.git" "$dir"
  else
    # The release, as main following origin's main, as Software Update
    # needs it; the commits since are fetched down to the release before
    # it only.
    git clone -q --depth 1 --branch "$release" "$REPOSITORIES/$1.git" "$dir" 2>/dev/null
    git -C "$dir" remote set-branches origin main
    fetch_branch "$1" "$dir"
    git -C "$dir" checkout -q -B main
    git -C "$dir" branch -q --set-upstream-to=origin/main main
  fi
}

fetch_projects() {
  say "Fetching LispM into $LISPM_HOME"
  for project in lispm-desktop lispm-agent lispm-browser lispm-kit lispm-ui; do
    fetch_project "$project"
  done
}

# Print the Emacs build the LispM.app at APP, or at $1, was made of, or
# nothing.
installed_build() {
  /usr/libexec/PlistBuddy -c 'Print :LispMEmacsBuild' "${1:-$APP}/Contents/Info.plist" 2>/dev/null || true
}

# Print the size of FILE in bytes, 0 when there is none.
file_size() {
  if [ -f "$1" ]; then
    wc -c <"$1" | tr -d ' '
  else
    echo 0
  fi
}

# Print the SHA-256 of FILE.
sha256() {
  if command -v sha256sum >/dev/null 2>&1; then
    sha256sum "$1" | cut -d' ' -f1
  else
    shasum -a 256 "$1" | cut -d' ' -f1
  fi
}

# Download URL to FILE, or the rest of it when FILE holds its start.  A
# connection that drops, or crawls under 1 KB/s for a minute, is taken up
# again where it stopped: curl's own --retry throws away what each try
# got, and its --retry-all-errors is newer than macOS 11's curl.  Fail
# after 5 tries in a row that got no further, leaving FILE to go on with.
download() {
  best=$(file_size "$2") stuck=0 delay=2
  until curl -fL --progress-bar --connect-timeout 30 --speed-limit 1024 --speed-time 60 \
    -C - -o "$2" "$1"; do
    # A server that cannot resume sends the whole file again.
    if [ $? = 33 ]; then
      rm -f "$2"
    fi
    size=$(file_size "$2")
    if [ "$size" -gt "$best" ]; then
      best=$size stuck=0 delay=2
    else
      stuck=$((stuck + 1)) delay=$((delay * 2))
      [ "$stuck" -lt 5 ] || return 1
    fi
    note "The download stopped at $((size / 1000000)) MB; it goes on in $delay seconds."
    sleep "$delay"
  done
}

# Put Emacs.app, checked, at $WORK/LispM.app: the one in LispM.app, or in
# its backup, when it is the build wanted, else downloaded.
fetch_emacs() {
  wanted=$EMACS_BUILD.$ARCH
  for kept in "$APP" "$BACKUP"; do
    if [ "$(installed_build "$kept")" = "$wanted" ]; then
      ditto "$kept" "$WORK/LispM.app"
      return
    fi
  done
  say "Downloading $EMACS_NAME (about 190 MB)"
  # The download is kept in LISPM_HOME until it is whole and checked, so
  # that a run cut short loses none of it: the next one goes on with it.
  cache=$LISPM_HOME/.cache
  part=$cache/$wanted.dmg.part
  mkdir -p "$cache"
  for old in "$cache"/*.dmg.part; do
    [ "$old" = "$part" ] || rm -f "$old"
  done
  if [ -s "$part" ]; then
    note "Going on with the download begun before, at $(($(file_size "$part") / 1000000)) MB."
  fi
  download "https://github.com/jimeh/emacs-builds/releases/download/$EMACS_RELEASE/$wanted.dmg" "$part" ||
    fail "Emacs could not be downloaded; run this again to go on from where it stopped"
  if [ "$(sha256 "$part")" != "$EMACS_SHA256" ]; then
    rm -f "$part"
    fail "the Emacs downloaded is not the one expected; nothing was installed"
  fi
  dmg=$WORK/emacs.dmg
  mv "$part" "$dmg"
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
  plist_set "$plist" CFBundleGetInfoString "LispM, on GNU Emacs $EMACS_VERSION"
  plist_set "$plist" LispMEmacsBuild "$EMACS_BUILD.$ARCH"
  # Emacs's icon from the asset catalog would win over LispM's.
  /usr/libexec/PlistBuddy -c 'Delete :CFBundleIconName' "$plist" >/dev/null 2>&1 || true
  for key in NSAppleEventsUsageDescription NSDesktopFolderUsageDescription \
    NSDocumentsFolderUsageDescription NSDownloadsFolderUsageDescription \
    NSRemovableVolumesUsageDescription NSSpeechRecognitionUsageDescription; do
    text=$(/usr/libexec/PlistBuddy -c "Print :$key" "$plist" 2>/dev/null) || continue
    plist_set "$plist" "$key" "$(printf '%s' "$text" | sed 's/^Emacs requires/LispM requires/')"
  done
  # The Music library's songs lie in ~/Music, which macOS lets read only
  # an application it asked, and it asks only one saying why: EMMS plays
  # them through LispM.
  plist_set "$plist" NSAppleMusicUsageDescription \
    "LispM plays the songs of your Music library in EMMS, and only reads them."
  # macOS keeps an application off the local network until it is allowed
  # there, and asks without a word on why unless told one; refused, LispM
  # reaches no other machine of the network, as the server an app works
  # with ("No route to host").
  plist_set "$plist" NSLocalNetworkUsageDescription \
    "LispM connects to the computers of your local network that you or its apps ask it to reach, as the server an app works with."
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

# Show the packages a batch Emacs says it installs, on lines beginning with
# LISPM-PACKAGE, and keep the rest of what it says in the log $1; succeed
# as the Emacs did, which the line LISPM-STATUS ends with.
show_packages() {
  status=1
  while IFS= read -r line; do
    case $line in
      "LISPM-PACKAGE "*) note "${line#LISPM-PACKAGE }" ;;
      "LISPM-STATUS "*) status=${line#LISPM-STATUS } ;;
      *) printf '%s\n' "$line" >>"$1" ;;
    esac
  done
  return "$status"
}

# Install the packages LispM Desktop uses, by starting it once without a
# display, as Emacs starts (a batch Emacs leaves the packages installed
# before inactive); the first start then asks only how you like to work.
# Each package is shown as it installs, as "magit (12/33)".
install_packages() {
  say "Installing LispM's Emacs packages (a few minutes the first time)"
  log=$LISPM_HOME/install.log
  recompile_packages || {
    tail -n 20 "$LISPM_HOME/compile.log" >&2
    fail "the packages could not be compiled again; the whole log is in $LISPM_HOME/compile.log"
  }
  : >"$log"
  if ! {
    status=0
    "$EMACS_PROGRAM" --batch --init-directory="$DESKTOP" \
      -l "$DESKTOP/early-init.el" --eval '(when package-enable-at-startup (package-activate-all))' \
      -L "$DESKTOP/lisp" -l lispm-desktop-deps -f lispm-desktop-deps-batch-progress \
      --eval '(setq user-init-file (expand-file-name "init.el" user-emacs-directory))' \
      -l "$DESKTOP/init.el" -f lispm-desktop-store-install-defaults-batch 2>>"$log" || status=$?
    printf '\nLISPM-STATUS %s\n' "$status"
  } | show_packages "$log"; then
    tail -n 20 "$log" >&2
    fail "the packages could not all be installed; the whole log is in $log"
  fi
}

# Compile the packages again when EMACS_BEFORE, the Emacs they were
# compiled by, is not EMACS, whose LispM.app is at APP: this one might not
# load that one's compiled code.  It runs before LispM loads them; the
# output is in compile.log.
recompile_packages() {
  if [ -n "$EMACS_BEFORE" ] && [ "$EMACS_BEFORE" != "$EMACS" ]; then
    note "Compiling LispM's packages again for Emacs $EMACS"
    "$EMACS_PROGRAM" --batch --init-directory="$DESKTOP" \
      -l "$DESKTOP/early-init.el" --eval '(when package-enable-at-startup (package-activate-all))' \
      --eval '(package-recompile-all)' >"$LISPM_HOME/compile.log" 2>&1
  fi
}

# The native modules: the browser's, for Chrome extensions, built where
# the command line tools' Swift is; the terminals', downloaded as built for
# this Mac by lispm-desktop's workflow and kept only when its SHA-256 is
# the one the release pins, as LispM does itself.  Nobody has to build it.
build_modules() {
  webext=$LISPM_HOME/lispm-browser/native/lispm-browser-webext
  version=$(sw_vers -productVersion)
  if [ -d "$webext" ] && command -v swift >/dev/null 2>&1 &&
    [ "$(printf '15.4\n%s\n' "$version" | sort -t. -k1,1n -k2,2n | head -n 1)" = 15.4 ]; then
    say "Building the browser's support for Chrome extensions"
    (cd "$webext" && swift build -c release --product lispm_browser_webext >"$LISPM_HOME/webext.log" 2>&1) ||
      note "It did not build (see $LISPM_HOME/webext.log); LispM tries again when you add an extension."
  fi
  fetch_terminals
}

fetch_terminals() {
  say "Fetching LispM's terminals"
  "$EMACS_PROGRAM" -Q --batch -L "$DESKTOP/lisp" -l lispm-desktop-term \
    -f lispm-desktop-term-batch-download >"$LISPM_HOME/terminal.log" 2>&1 ||
    note "They could not be fetched (see $LISPM_HOME/terminal.log); LispM tries again at its next start."
}

# Install with apt the packages, or .deb files, named.
linux_apt() {
  $SUDO apt-get update -q || fail "apt could not read the packages there are"
  $SUDO env DEBIAN_FRONTEND=noninteractive apt-get install -y -q "$@" ||
    fail "apt could not install $*"
}

# Install LispM's Emacs, and git, with apt: the .deb LISPM_EMACS_DEB names,
# as linux/build-emacs.sh builds one, else the one built for this system,
# downloaded and checked, unless it is installed already.
install_linux_emacs() {
  EMACS_VERSION=${LINUX_EMACS_VERSION%-*}
  if [ -z "${LISPM_EMACS_DEB:-}" ] &&
    [ "$(dpkg-query -W -f '${Version}' lispm-emacs 2>/dev/null || true)" = "$LINUX_EMACS_VERSION~$SYSTEM" ]; then
    command -v git >/dev/null 2>&1 || linux_apt git
    return
  fi
  if [ -n "${LISPM_EMACS_DEB:-}" ]; then
    [ -f "$LISPM_EMACS_DEB" ] || fail "LISPM_EMACS_DEB names no file: $LISPM_EMACS_DEB"
    deb=$WORK/$(basename "$LISPM_EMACS_DEB")
    cp "$LISPM_EMACS_DEB" "$deb"
  else
    asset=lispm-emacs-$LINUX_EMACS_VERSION-$SYSTEM-$ARCH.deb
    sum=$(printf '%s\n' "$LINUX_EMACS_SHA256S" | sed -n "s/^$SYSTEM-$ARCH //p")
    [ -n "$sum" ] || fail "LispM's Emacs for $SYSTEM on $ARCH is not published yet"
    say "Downloading GNU Emacs $EMACS_VERSION for LispM"
    # Kept until whole and checked, as on macOS: a run cut short loses
    # none of it.
    cache=$LISPM_HOME/.cache
    part=$cache/$asset.part
    mkdir -p "$cache"
    for old in "$cache"/*.deb.part; do
      [ "$old" = "$part" ] || rm -f "$old"
    done
    download "$REPOSITORIES/lispm-desktop/releases/download/$LINUX_EMACS_RELEASE/$asset" "$part" ||
      fail "Emacs could not be downloaded; run this again to go on from where it stopped"
    if [ "$(sha256 "$part")" != "$sum" ]; then
      rm -f "$part"
      fail "the Emacs downloaded is not the one expected; nothing was installed"
    fi
    deb=$WORK/$asset
    mv "$part" "$deb"
  fi
  say "Installing GNU Emacs $EMACS_VERSION in /opt/lispm/emacs"
  note "apt installs it with the libraries it uses, as WebKitGTK; sudo may ask for your password."
  # apt reads it as a file only by a path with a slash.
  linux_apt "$deb" git ca-certificates
}

# The command lispm, in ~/.local/bin, and LispM in the applications menu,
# with its icon: both start LispM's Emacs on the checkout.
make_launcher() {
  data=${XDG_DATA_HOME:-$HOME/.local/share}
  mkdir -p "$HOME/.local/bin" "$data/applications" "$data/icons/hicolor/512x512/apps"
  LAUNCHER=$HOME/.local/bin/lispm
  cat >"$LAUNCHER" <<EOF
#!/bin/sh
# LispM: GNU Emacs, as install.sh installed it, on LispM Desktop.
exec "$LINUX_EMACS" --init-directory="$DESKTOP" "\$@"
EOF
  chmod 755 "$LAUNCHER"
  cp "$DESKTOP/linux/lispm.png" "$data/icons/hicolor/512x512/apps/lispm.png"
  cat >"$data/applications/lispm.desktop" <<EOF
[Desktop Entry]
Type=Application
Name=LispM
GenericName=Lisp Machine
Comment=The LispM desktop and its agents, on GNU Emacs
Exec="$LAUNCHER" %F
Icon=lispm
Terminal=false
Categories=Development;TextEditor;
StartupWMClass=Emacs
EOF
  update-desktop-database "$data/applications" >/dev/null 2>&1 || true
}

cleanup() {
  if [ -n "${MOUNT:-}" ]; then
    hdiutil detach -quiet -force "$MOUNT" 2>/dev/null || true
  fi
  if [ -n "${WORK:-}" ]; then
    rm -rf "$WORK"
  fi
}

begin_work() {
  WORK=$(mktemp -d "${TMPDIR:-/tmp}/lispm.XXXXXX")
  MOUNT=
  trap cleanup EXIT
  trap 'exit 130' INT TERM
}

# Make LispM.app of LISPM_EMACS's Emacs at STAGED, LispM running on.
stage() {
  check_tools
  settle_places
  choose_emacs
  [ -d "$DESKTOP" ] || fail "LispM is not installed in $LISPM_HOME"
  begin_work
  fetch_emacs
  make_app
  rm -rf "$STAGED"
  mkdir -p "$(dirname "$STAGED")"
  mv "$WORK/LispM.app" "$STAGED"
  say "LispM.app on $EMACS_NAME is ready"
}

# Once LispM has quit, keep LispM.app at BACKUP, put the one at STAGED in
# its place, compile the packages again for its Emacs if need be, and
# open it.  A switch that fails puts LispM.app back.
switch() {
  settle_places
  [ -d "$STAGED" ] || fail "no LispM.app is staged in $(dirname "$STAGED"); nothing was switched"
  refuse_running
  EMACS_BEFORE=$(installed_emacs)
  EMACS=$(installed_emacs "$STAGED")
  say "Switching LispM.app to Emacs $EMACS"
  if [ -d "$APP" ]; then
    rm -rf "$BACKUP"
    mkdir -p "$(dirname "$BACKUP")"
    mv "$APP" "$BACKUP"
  fi
  if ! mv "$STAGED" "$APP"; then
    [ -d "$BACKUP" ] && mv "$BACKUP" "$APP"
    fail "the staged LispM.app could not be put in place; LispM.app is as it was"
  fi
  "$LSREGISTER" -f "$APP" >/dev/null 2>&1 || true
  recompile_packages || note "The packages could not all be compiled again (see $LISPM_HOME/compile.log)"
  note "The one before is kept in $BACKUP"
  if [ "${LISPM_OPEN:-1}" != 0 ]; then
    open "$APP"
  fi
}

install_linux() {
  settle_places
  refuse_running
  begin_work
  install_linux_emacs
  fetch_projects
  EMACS_BEFORE=
  install_packages
  fetch_terminals
  make_launcher
  say "LispM is installed"
  case :$PATH: in
    *":$HOME/.local/bin:"*) ;;
    *) note "$HOME/.local/bin is not on your PATH yet: the command lispm runs once you log in again." ;;
  esac
  if [ "${LISPM_OPEN:-1}" != 0 ] && [ -n "${DISPLAY:-}${WAYLAND_DISPLAY:-}" ]; then
    note "Opening it now; next time, open it from the applications menu, or run lispm."
    nohup "$LAUNCHER" >/dev/null 2>&1 &
  else
    note "Open it from the applications menu, or run lispm (lispm -nw in a terminal)."
  fi
}

install() {
  check_tools
  settle_places
  refuse_running
  choose_emacs
  fetch_projects
  begin_work
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

main() {
  check_system
  case $OS.${LISPM_STEP:-} in
    linux.) install_linux ;;
    linux.*) fail "LISPM_STEP is for LispM.app, on macOS" ;;
    *.) install ;;
    *.stage) stage ;;
    *.switch) switch ;;
    *) fail "LISPM_STEP is stage or switch, not $LISPM_STEP" ;;
  esac
}

main "$@"
