#!/bin/sh
# Exercise install.sh's real dependency gate with isolated local Git fixtures.
# No user profile, installed application or network service is used.  EMACS
# selects the batch Emacs; fixture commits intentionally have local test authors.
set -eu

source_root=$(CDPATH='' cd -- "$(dirname -- "$0")/.." && pwd)
test_root=$(mktemp -d "${TMPDIR:-/tmp}/lispm-install-safety.XXXXXX")
trap 'rm -rf "$test_root"' 0
trap 'exit 130' INT TERM
test_emacs=${EMACS:-emacs}
test_emacs=$(command -v "$test_emacs")

# Use only install.sh's definitions, preserving the production entry point.
sed '/^main "\$@"$/d' "$source_root/install.sh" >"$test_root/functions.sh"

# Platform checks run before any installation, including under Rosetta.
# The generated definitions call these platform stubs indirectly.
# shellcheck disable=SC1091,SC2329
(
  . "$test_root/functions.sh"
  uname() { printf '%s\n' arm64; }
  sw_vers() { printf '%s\n' 14.0; }
  check_mac
  [ "$ARCH" = arm64 ]
)
# shellcheck disable=SC1091,SC2329
(
  . "$test_root/functions.sh"
  uname() { printf '%s\n' x86_64; }
  sysctl() { printf '%s\n' 1; }
  sw_vers() { printf '%s\n' 14.0; }
  check_mac
  [ "$ARCH" = arm64 ]
)
# shellcheck disable=SC1091,SC2329
if (
  . "$test_root/functions.sh"
  uname() { printf '%s\n' x86_64; }
  sysctl() { printf '%s\n' 0; }
  check_mac
) >"$test_root/intel.out" 2>&1; then
  printf '%s\n' 'FAIL: Intel Mac was accepted' >&2
  exit 1
fi
grep -q 'Intel Macs are no longer officially supported' "$test_root/intel.out"
printf '%s\n' 'PASS: Apple silicon and Rosetta accepted; Intel Mac rejected'

# Isolate Git configuration and the batch Emacs cache/config directories.
# The production check sets user-emacs-directory and package-user-dir itself.
mkdir -p "$test_root/user" "$test_root/remotes"
export XDG_CONFIG_HOME="$test_root/user/config"
export XDG_CACHE_HOME="$test_root/user/cache"
export GIT_CONFIG_NOSYSTEM=1
export GIT_CONFIG_GLOBAL=/dev/null
unset GIT_DIR GIT_WORK_TREE GIT_INDEX_FILE EMACSLOADPATH

# Commit all fixture sources with deterministic local identity and no signing.
fixture_commit() {
  git -C "$1" add .
  git -C "$1" -c user.name='LispM test' -c user.email='test@invalid' \
    -c commit.gpgsign=false commit -q -m "$2"
}

# Make a main-branch origin in a local directory, with no remote mutations.
fixture_repository() {
  mkdir -p "$1"
  git -C "$1" init -q -b main
}

# Write the only constrained dependency in this fixture, its version in source.
fixture_ui() {
  cat >"$1/lispm-ui.el" <<EOF
;;; lispm-ui.el --- Test UI -*- lexical-binding: t; -*-
;; Version: $2
;; Package-Requires: ((emacs "30.1"))
;;; Code:
(provide 'lispm-ui)
;;; lispm-ui.el ends here
EOF
}

# The candidate deliberately has a top-level error before its literal manifest.
# A correct preflight reads data; it never evaluates the incoming dependency file.
fixture_manifest() {
  cat >"$1/lisp/lispm-desktop-deps.el" <<EOF
;;; lispm-desktop-deps.el --- Test manifest -*- lexical-binding: t; -*-
(error "The candidate manifest must never be evaluated")
(defconst lispm-desktop-deps
  '((lispm-ui :checkout "lispm-ui" :version "$2")))
EOF
}

ui_origin=$test_root/remotes/lispm-ui.git
fixture_repository "$ui_origin"
fixture_ui "$ui_origin" 0.6.0
fixture_commit "$ui_origin" old
git -C "$ui_origin" tag v0.6.0
fixture_ui "$ui_origin" 0.7.0
fixture_commit "$ui_origin" new
git -C "$ui_origin" tag v0.7.0

desktop_origin=$test_root/remotes/lispm-desktop.git
fixture_repository "$desktop_origin"
mkdir -p "$desktop_origin/lisp"
printf 'elpa/\n' >"$desktop_origin/.gitignore"
printf 'original\n' >"$desktop_origin/desktop.txt"
fixture_manifest "$desktop_origin" 0.6.0
fixture_commit "$desktop_origin" old
git -C "$desktop_origin" tag v0.1.0
desktop_old=$(git -C "$desktop_origin" rev-parse HEAD)
fixture_manifest "$desktop_origin" 0.7.0
cp "$source_root/lisp/lispm-desktop-update-safety.el" \
   "$source_root/lisp/lispm-desktop-store-installer.el" "$desktop_origin/lisp/"
printf 'new release\n' >"$desktop_origin/new-file.txt"
fixture_commit "$desktop_origin" new
git -C "$desktop_origin" tag v0.2.0
desktop_new=$(git -C "$desktop_origin" rev-parse HEAD)

# The remaining siblings participate in the real production fetch loop.
for project in lispm-kit lispm-agent lispm-browser; do
  fixture_repository "$test_root/remotes/$project.git"
  printf 'fixture\n' >"$test_root/remotes/$project.git/source.txt"
  fixture_commit "$test_root/remotes/$project.git" initial
  git -C "$test_root/remotes/$project.git" tag v0.1.0
done

# Prepare an installed old Desktop following main and an old UI sibling.
fixture_profile() {
  fixture_home=$test_root/$1
  mkdir -p "$fixture_home"
  git clone -q "$desktop_origin" "$fixture_home/lispm-desktop"
  git -C "$fixture_home/lispm-desktop" checkout -q -B main v0.1.0
  git clone -q "$ui_origin" "$fixture_home/lispm-ui"
  git -C "$fixture_home/lispm-ui" checkout -q -B main v0.6.0
}

# Install a package descriptor/autoload file so real package-initialize selects
# this profile package before a sibling checkout, as the reported failure did.
fixture_active_ui() {
  active_ui=$fixture_home/lispm-desktop/elpa/lispm-ui
  mkdir -p "$(dirname "$active_ui")"
  git clone -q "$ui_origin" "$active_ui"
  git -C "$active_ui" checkout -q --detach "v$1"
  cat >"$active_ui/lispm-ui-pkg.el" <<EOF
(define-package "lispm-ui" "$1" "Test UI" '((emacs "30.1")))
EOF
  cat >"$active_ui/lispm-ui-autoloads.el" <<'EOF'
;;; -*- lexical-binding: t; -*-
(add-to-list 'load-path
             (directory-file-name
              (file-name-directory (or load-file-name buffer-file-name))))
EOF
}

# Run the unchanged production fetch orchestration against local fixture URLs.
# The sourced production functions consume these shell variables.
# shellcheck disable=SC1091,SC2034
fixture_update() (
  . "$test_root/functions.sh"
  LISPM_HOME=$fixture_home
  DESKTOP=$LISPM_HOME/lispm-desktop
  REPOSITORIES=$test_root/remotes
  EMACS_PROGRAM=$test_emacs
  fetch_projects
)

# Fail with a case-specific assertion while preserving the captured explanation.
assert_head() {
  [ "$(git -C "$fixture_home/lispm-desktop" rev-parse HEAD)" = "$1" ] || {
    cat "$fixture_home/run.log" >&2
    printf 'Unexpected Desktop HEAD in %s\n' "$fixture_home" >&2
    exit 1
  }
}

fixture_profile dirty-active-ui
fixture_active_ui 0.6.0
printf '\n;; Local preview must survive.\n' >>"$active_ui/lispm-ui.el"
printf 'untracked preview\n' >"$active_ui/lispm-ui-field.el"
cp "$active_ui/lispm-ui.el" "$fixture_home/source-before.el"
active_before=$(git -C "$active_ui" rev-parse HEAD)
if fixture_update >"$fixture_home/run.log" 2>&1; then
  printf 'Dirty old active UI unexpectedly permitted Desktop update\n' >&2
  exit 1
fi
assert_head "$desktop_old"
[ "$(git -C "$active_ui" rev-parse HEAD)" = "$active_before" ]
cmp "$active_ui/lispm-ui.el" "$fixture_home/source-before.el"
[ "$(cat "$active_ui/lispm-ui-field.el")" = 'untracked preview' ]
grep -q 'needs version 0.7.0' "$fixture_home/run.log"
[ "$(git -C "$fixture_home/lispm-ui" describe --tags --exact-match)" = v0.7.0 ]
printf 'PASS dirty old active package blocks Desktop despite updated sibling\n'

fixture_profile clean-old-active-ui
fixture_active_ui 0.6.0
if fixture_update >"$fixture_home/run.log" 2>&1; then
  printf 'Old active UI unexpectedly permitted Desktop update\n' >&2
  exit 1
fi
assert_head "$desktop_old"
grep -q 'needs version 0.7.0' "$fixture_home/run.log"
printf 'PASS old clean active package is not misrepresented as compatible\n'

fixture_profile new-active-ui
fixture_active_ui 0.7.0
fixture_update >"$fixture_home/run.log" 2>&1 || { cat "$fixture_home/run.log" >&2; exit 1; }
assert_head "$desktop_new"
printf 'PASS compatible active package permits Desktop update\n'

fixture_profile sibling-only
fixture_update >"$fixture_home/run.log" 2>&1 || { cat "$fixture_home/run.log" >&2; exit 1; }
assert_head "$desktop_new"
printf 'PASS sibling update completes before Desktop gate and advancement\n'

fixture_profile dirty-desktop
printf 'local Desktop edit\n' >"$fixture_home/lispm-desktop/desktop.txt"
if fixture_update >"$fixture_home/run.log" 2>&1; then
  printf 'Dirty Desktop unexpectedly updated\n' >&2
  exit 1
fi
assert_head "$desktop_old"
[ "$(cat "$fixture_home/lispm-desktop/desktop.txt")" = 'local Desktop edit' ]
printf 'PASS tracked Desktop changes remain untouched\n'

fixture_profile untracked-desktop
printf 'local new file\n' >"$fixture_home/lispm-desktop/new-file.txt"
if fixture_update >"$fixture_home/run.log" 2>&1; then
  printf 'Desktop update unexpectedly overwrote an untracked file\n' >&2
  exit 1
fi
assert_head "$desktop_old"
[ "$(cat "$fixture_home/lispm-desktop/new-file.txt")" = 'local new file' ]
printf 'PASS untracked Desktop collision is rejected before advancement\n'

# A genuinely new installation may fetch before an Emacs executable exists.
fixture_home=$test_root/fresh
mkdir -p "$fixture_home"
saved_emacs=$test_emacs
test_emacs=$test_root/no-emacs-yet
fixture_update >"$fixture_home/run.log" 2>&1 || { cat "$fixture_home/run.log" >&2; exit 1; }
test_emacs=$saved_emacs
assert_head "$desktop_new"
printf 'PASS fresh install still fetches before its Emacs is installed\n'
