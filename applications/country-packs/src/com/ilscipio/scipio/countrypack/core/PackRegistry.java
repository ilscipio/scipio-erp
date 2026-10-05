/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */
package com.ilscipio.scipio.countrypack.core;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * Reads the packs of a folder: every sub folder with a {@code pack.json}. The folder name must equal the pack id.
 * {@link #problems(Pack)} lists faults of a pack; the unit tests use it.
 *
 * <p>SCIPIO: 4.0.0: Added (W1-03).</p>
 */
public final class PackRegistry {
    private static final ObjectMapper MAPPER = new ObjectMapper().configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, true);
    private static final List<String> SCOPES = List.of("any", "home", "market");

    private final Path root;
    private final Map<String, Pack> packs = new TreeMap<>();

    public PackRegistry(Path root) {
        this.root = root;
        try (Stream<Path> dirs = Files.list(root)) {
            for (Path dir : dirs.sorted().collect(Collectors.toList())) {
                Path file = dir.resolve("pack.json");
                if (Files.isRegularFile(file)) {
                    Pack pack = MAPPER.readValue(file.toFile(), Pack.class);
                    if (!pack.id().equals(dir.getFileName().toString())) {
                        throw new IllegalStateException("Pack id " + pack.id() + " differs from the folder " + dir.getFileName());
                    }
                    packs.put(pack.id(), pack);
                }
            }
        } catch (IOException e) {
            throw new UncheckedIOException("Cannot read the packs in " + root, e);
        }
    }

    public Pack get(String id) {
        Pack p = id == null ? null : packs.get(id.trim().toLowerCase(Locale.ROOT));
        if (p == null) {
            throw new IllegalArgumentException("Unknown country pack: " + id + ". Known packs: " + packs.keySet());
        }
        return p;
    }

    public List<Pack> all() {
        return new ArrayList<>(packs.values());
    }

    /** The template file that belongs to the pack (it may not exist: the loader then falls back to the compliance templates). */
    public Path templateFile(Pack pack, String slug) {
        return root.resolve(pack.id()).resolve("templates").resolve(slug + ".html");
    }

    /** Faults of a pack: empty list when the pack is sound. */
    public List<String> problems(Pack p) {
        List<String> out = new ArrayList<>();
        if (p.version() < 1) {
            out.add("version must be 1 or more");
        }
        for (String f : new String[] {p.name(), p.country(), p.locale(), p.currency()}) {
            if (f == null || f.isBlank()) {
                out.add("name, country, locale and currency are required");
                break;
            }
        }
        if (p.jurisdictions().isEmpty()) {
            out.add("jurisdictions is empty");
        }
        Set<String> ids = new HashSet<>();
        for (Pack.Task t : p.tasks()) {
            if (t.id() == null || t.id().isBlank() || !ids.add(t.id())) {
                out.add("task id missing or duplicate: " + t.id());
            }
            if (t.title() == null || t.title().isBlank()) {
                out.add("task " + t.id() + " has no title");
            }
            if (!SCOPES.contains(t.scope())) {
                out.add("task " + t.id() + " has an unknown scope " + t.scope());
            }
            if (t.sinceVersion() > p.version()) {
                out.add("task " + t.id() + " is newer than the pack");
            }
        }
        ids.clear();
        for (Pack.Template t : p.legalTemplates()) {
            if (t.slug() == null || t.docTypeId() == null || !ids.add(t.slug())) {
                out.add("template slug or docTypeId missing or duplicate: " + t.slug());
            }
            if (!SCOPES.contains(t.scope())) {
                out.add("template " + t.slug() + " has an unknown scope " + t.scope());
            }
            if (t.sinceVersion() > p.version()) {
                out.add("template " + t.slug() + " is newer than the pack");
            }
        }
        return out;
    }
}
