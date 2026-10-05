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
package com.ilscipio.scipio.countrypack;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import com.ilscipio.scipio.countrypack.core.PackRegistry;
import com.ilscipio.scipio.countrypack.core.TemplateSource;

/** Test helpers: the pack folders of the component and the shipped templates of the compliance component. */
final class Packs {
    /** The component folder: the working directory of a Gradle test run is the project folder. */
    static final Path ROOT = Path.of("").toAbsolutePath();
    static final Path COMPLIANCE_TEMPLATES = ROOT.resolveSibling("compliance").resolve("data").resolve("templates");

    private Packs() {
    }

    static PackRegistry registry() {
        return new PackRegistry(ROOT);
    }

    /** Same fallback as the server: the compliance template of the language, or null. */
    static String complianceTemplate(String slug, String language) {
        Path f = COMPLIANCE_TEMPLATES.resolve(language).resolve(slug + ".html");
        try {
            return Files.isRegularFile(f) ? Files.readString(f, StandardCharsets.UTF_8) : null;
        } catch (java.io.IOException e) {
            throw new java.io.UncheckedIOException(e);
        }
    }

    static TemplateSource templates(PackRegistry registry) {
        return new TemplateSource(registry, Packs::complianceTemplate);
    }
}
