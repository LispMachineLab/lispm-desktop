// lispm-music-library: reads the Music app's library, and only reads it.
//
// Copyright (C) 2026  Ryan.K
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as published
// by the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// It reads the library through Apple's iTunesLibrary framework, which
// gives the library's songs and playlists to read and nothing to change
// them with; macOS asks once, in this program's name, whether it may
// (Privacy & Security, Media & Apple Music).  It is built into a small
// application of its own, LispM Music Library.app, whose Info.plist says
// why it asks, and is opened by LaunchServices, so macOS asks for it
// alone.  See lisp/lispm-desktop-app-emms.el.
//
// Usage: lispm-music-library OUTPUT.json
//
// OUTPUT receives {"total": N, "tracks": [...], "playlists": [...]}: every
// song, with its file's path when it has one on this computer, its tags,
// and whether it is protected; every playlist of songs, as their ids.  On
// a failure, {"error": WHY}.

import Foundation
import iTunesLibrary

let output = CommandLine.arguments.count > 1 ? CommandLine.arguments[1] : "/dev/stdout"

func write(_ object: [String: Any]) {
    let data = (try? JSONSerialization.data(withJSONObject: object)) ?? Data("{}".utf8)
    FileManager.default.createFile(atPath: output, contents: data)
}

func song(_ item: ITLibMediaItem) -> [String: Any] {
    var song: [String: Any] = [
        "id": item.persistentID.stringValue,
        "name": item.title,
        "album": item.album.title ?? "",
        "trackNumber": item.trackNumber,
        "discNumber": item.album.discNumber,
        "year": item.year,
        "genre": item.genre,
        "duration": Double(item.totalTime) / 1000.0,
        "kind": item.kind ?? "",
        "protected": item.isDRMProtected,
    ]
    if let artist = item.artist?.name { song["artist"] = artist }
    if let albumArtist = item.album.albumArtist { song["albumArtist"] = albumArtist }
    if !item.composer.isEmpty { song["composer"] = item.composer }
    if item.locationType == .file, let location = item.location { song["path"] = location.path }
    return song
}

do {
    let library = try ITLibrary(apiVersion: "1.0")
    let songs = library.allMediaItems.filter { $0.mediaKind == .kindSong }
    let ids = Set(songs.map { $0.persistentID })
    let playlists: [[String: Any]] = library.allPlaylists
        .filter { !$0.isMaster && $0.isVisible && $0.distinguishedKind == .kindNone && $0.kind != .folder }
        .map { playlist in
            ["name": playlist.name,
             "ids": playlist.items.filter { ids.contains($0.persistentID) }.map { $0.persistentID.stringValue }]
        }
    write(["total": songs.count, "tracks": songs.map(song), "playlists": playlists])
} catch {
    write(["error": error.localizedDescription])
    exit(1)
}
