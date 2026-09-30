/*
 * Copyright (c) 2026 Genworks International
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.  Distributed WITHOUT
 * ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.
 *
 * The prompt lab's service worker.  It keeps the page's SHELL and
 * nothing else:
 *
 *   the page          asked of the network first, so that it always
 *                     names the stylesheets of the day; the kept copy
 *                     answers only when the network does not
 *   the static files  kept -- the lab's own under <prefix>/static/,
 *                     and the tokens, skins and script it takes from
 *                     the sluice's: every such address carries its
 *                     file's date, so what is kept under an address is
 *                     what that address will always mean
 *   everything else   the network's, untouched -- the doors under
 *                     <prefix>/api/, the viewer and all it asks for.
 *                     A model is built and drawn by the lab's engine;
 *                     nothing of it is ever answered from a cache.
 *
 * The four names in double braces are filled in by the server
 * (source/app.lisp), which serves this file at <prefix>/worker.  The
 * cache's name changes when any file of the shell does; that makes
 * this script a new one, the browser installs it in place of the old,
 * and it drops the old cache.
 */

var CACHE = {{cache}};
var PREFIX = {{prefix}};
var SHELL = {{shell}};
var KEPT = {{kept}};

// every cache of THIS lab begins so; another lab on the same host (a
// second engine, at a prefix of its own) keeps its own
var MINE = 'prompt-lab:' + PREFIX + ':';

self.addEventListener('install', function (event) {
  event.waitUntil(
    caches.open(CACHE).then(function (cache) {
      // one at a time, and a file that will not come is no reason to
      // refuse the rest: the page works without an icon
      return Promise.all(SHELL.map(function (address) {
        return cache.add(address).catch(function () {});
      }));
    }).then(function () { return self.skipWaiting(); })
  );
});

self.addEventListener('activate', function (event) {
  event.waitUntil(
    caches.keys().then(function (names) {
      return Promise.all(names.filter(function (name) {
        return name.indexOf(MINE) === 0 && name !== CACHE;
      }).map(function (name) { return caches.delete(name); }));
    }).then(function () { return self.clients.claim(); })
  );
});

self.addEventListener('fetch', function (event) {
  var request = event.request;
  if (request.method !== 'GET') return;
  var url = new URL(request.url);
  if (url.origin !== self.location.origin) return;

  // the page, whatever its query: the network's while there is one
  if (url.pathname === PREFIX && request.mode === 'navigate') {
    event.respondWith(
      fetch(request).then(function (response) {
        if (response.ok) {
          var copy = response.clone();
          caches.open(CACHE).then(function (cache) { cache.put(PREFIX, copy); });
        }
        return response;
      }).catch(function () {
        return caches.open(CACHE)
          .then(function (cache) { return cache.match(PREFIX); })
          .then(function (kept) { return kept || Response.error(); });
      })
    );
    return;
  }

  // the static files: kept, and fetched once when they are not
  if (KEPT.some(function (place) { return url.pathname.indexOf(place) === 0; })) {
    event.respondWith(
      caches.open(CACHE).then(function (cache) {
        return cache.match(request).then(function (kept) {
          return kept || fetch(request).then(function (response) {
            if (response.ok) cache.put(request, response.clone());
            return response;
          });
        });
      })
    );
  }
});
