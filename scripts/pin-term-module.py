#!/usr/bin/env python3
# Copyright (C) 2026  Ryan.K
# SPDX-License-Identifier: AGPL-3.0-or-later
"""Pin the SHA-256 of the terminals' published modules in lisp/lispm-desktop-term-sums.eld.

LispM Desktop downloads the terminal module of its sources from the
release term-module-VERSION and keeps it only when its SHA-256 is the one
that file pins, so the checksums travel in the repository's signed
history, not beside the binaries they vouch for.  This downloads every
library the release's SHA256SUMS lists, refuses any whose SHA-256 differs
from the listing or a release lacking a target LispM downloads for, and
writes the file for VERSION, the version Emacs computes for the sources
in native/lispm-desktop-term (`lispm-desktop-term-source-version').

Run it (`make pin') once the terminal module workflow has published the
release of changed sources, and commit the file: at the latest in the
"Version X.Y.Z" pull request, so that the release, which install.sh and
Software Update install, pins its own sources.

--check compares the file with the release instead of writing it, and
fails when they differ; a file pinning other sources than these passes,
with a notice, as it does between a change of the sources and its pin.
"""
import argparse
import hashlib
import os
from pathlib import Path
import re
import subprocess
import sys
import urllib.request

ROOT = Path(__file__).resolve().parents[1]
SUMS = ROOT / "lisp" / "lispm-desktop-term-sums.eld"
URL = "https://github.com/LispMachineLab/lispm-desktop/releases/download"
# The targets lispm-desktop-term.el downloads for, as the workflow builds them.
TARGETS = ("aarch64-apple-darwin", "x86_64-apple-darwin", "x86_64-unknown-linux-gnu")
ASSET = re.compile(r"liblispm_desktop_term-(.+)\.(dylib|so)")
LINE = re.compile(r"([0-9a-f]{64}) [ *](\S+)")
HEADER = """\
;;; lispm-desktop-term-sums.eld --- The terminal modules LispM downloads  -*- lisp-data -*-
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; The SHA-256 of each library of the release term-module-VERSION, the
;; module lispm-desktop-term.el downloads for the sources in
;; native/lispm-desktop-term: a library whose SHA-256 is not the one here
;; is refused.  VERSION must be the version of those sources.
;; scripts/pin-term-module.py writes it (make pin) once the terminal
;; module workflow has published that release.

"""


def source_version():
    """The version Emacs computes for the sources here."""
    output = subprocess.check_output(
        [os.environ.get("EMACS", "emacs"), "-Q", "--batch", "-L", str(ROOT / "lisp"),
         "-l", "lispm-desktop-term",
         "--eval", "(princ (or (lispm-desktop-term-source-version) \"\"))"],
        cwd=str(ROOT), universal_newlines=True, stderr=subprocess.DEVNULL)
    if not re.fullmatch(r"[0-9a-f]{16}", output):
        sys.exit("Emacs computes no source version here: {!r}".format(output))
    return output


def fetch(url):
    with urllib.request.urlopen(url) as response:
        return response.read()


def published(base, version):
    """Return {target: SHA-256} of the release of VERSION under BASE, each
    library downloaded and found to be the one its SHA256SUMS lists."""
    release = "{}/term-module-{}/".format(base.rstrip("/"), version)
    modules = {}
    for line in fetch(release + "SHA256SUMS").decode().splitlines():
        if not line.strip():
            continue
        match = LINE.fullmatch(line.strip())
        asset = match and ASSET.fullmatch(match.group(2))
        if not asset:
            sys.exit("SHA256SUMS of term-module-{} lists {!r}, not a terminal module".format(version, line))
        sha256, name = match.groups()
        target = asset.group(1)
        if asset.group(2) != ("dylib" if target.endswith("-darwin") else "so"):
            sys.exit("{} is misnamed for {}".format(name, target))
        if hashlib.sha256(fetch(release + name)).hexdigest() != sha256:
            sys.exit("{} is not the library SHA256SUMS lists: refusing to pin it".format(name))
        modules[target] = sha256
    missing = [target for target in TARGETS if target not in modules]
    if missing:
        sys.exit("term-module-{} has no library for {}".format(version, ", ".join(missing)))
    return modules


def render(version, modules):
    entries = "\n           ".join('("{}" . "{}")'.format(target, modules[target])
                                   for target in sorted(modules))
    return HEADER + '(:version "{}"\n :modules ({}))\n'.format(version, entries)


def pinned():
    """Return (VERSION, {target: SHA-256}) the file pins, VERSION None for none."""
    text = SUMS.read_text() if SUMS.exists() else ""
    version = re.search(r':version "([0-9a-f]{16})"', text)
    modules = dict(re.findall(r'\("([^"]+)" \. "([0-9a-f]{64})"\)', text))
    return (version.group(1) if version else None), modules


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--url", default=URL, help="where the releases are (default: GitHub's)")
    parser.add_argument("--version", help="the source version to pin (default: Emacs's for the sources here)")
    parser.add_argument("--check", action="store_true", help="compare the file with the release; write nothing")
    arguments = parser.parse_args()
    version = arguments.version or source_version()
    if arguments.check:
        pinned_version, pinned_modules = pinned()
        if pinned_version != version:
            print("::notice::{} pins {}, not the sources' {}: pin it once term-module-{} is published"
                  .format(SUMS.relative_to(ROOT),
                          "term-module-" + pinned_version if pinned_version else "nothing",
                          version, version))
            return
        if published(arguments.url, version) != pinned_modules:
            sys.exit("{} differs from the release term-module-{}".format(SUMS.relative_to(ROOT), version))
        print("{} pins term-module-{} as published".format(SUMS.relative_to(ROOT), version))
        return
    SUMS.write_text(render(version, published(arguments.url, version)))
    print("{} pins term-module-{}".format(SUMS.relative_to(ROOT), version))


if __name__ == "__main__":
    main()
