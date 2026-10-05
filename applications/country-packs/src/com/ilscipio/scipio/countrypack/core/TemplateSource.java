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
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;
import java.util.function.BiFunction;

/**
 * Finds the text of a legal template. First the file in the pack ({@code <id>/templates/<slug>.html}); then the fallback,
 * which is the shipped template of the compliance component for the language of the pack.
 *
 * <p>SCIPIO: 4.0.0: Added (W1-03).</p>
 */
public final class TemplateSource {
    /** The mark that every template text carries (D11). The dash is an en dash. */
    public static final String MARKER = "TEMPLATE – not legal advice";

    private final PackRegistry registry;
    private final BiFunction<String, String, String> fallback;

    /** @param fallback (slug, language) to text, or null when there is no such text */
    public TemplateSource(PackRegistry registry, BiFunction<String, String, String> fallback) {
        this.registry = registry;
        this.fallback = fallback;
    }

    public Optional<String> read(Pack pack, Pack.Template template) {
        Path file = registry.templateFile(pack, template.slug());
        try {
            if (Files.isRegularFile(file)) {
                return Optional.of(Files.readString(file, StandardCharsets.UTF_8));
            }
        } catch (IOException e) {
            throw new java.io.UncheckedIOException(e);
        }
        return Optional.ofNullable(fallback.apply(template.slug(), language(pack.locale())));
    }

    static String language(String locale) {
        int i = locale.indexOf('_');
        return i < 0 ? locale : locale.substring(0, i);
    }

    /** Puts the mark under the first heading when the text has no mark yet. The seller sees it on the live page until the seller removes it. */
    public static String withMarker(String body, String language) {
        if (body.contains(MARKER)) {
            return body;
        }
        String sentence = "de".equals(language)
                ? "Grundtext von Scipio. Sie haften für Ihre Rechtstexte. Prüfen Sie jeden Satz und ändern Sie ihn, wenn er nicht passt."
                : "Basic text from Scipio. You are liable for your legal texts. Check every sentence and change it when it does not fit.";
        String banner = "<p><strong>" + MARKER + ".</strong> " + sentence + "</p>";
        int end = body.toLowerCase(java.util.Locale.ROOT).indexOf("</h1>");
        return end < 0 ? banner + "\n" + body : body.substring(0, end + 5) + "\n" + banner + body.substring(end + 5);
    }

    /** The text of the first heading, or the fallback. */
    public static String title(String body, String fallbackTitle) {
        java.util.regex.Matcher m = java.util.regex.Pattern.compile("<h1[^>]*>(.*?)</h1>", java.util.regex.Pattern.CASE_INSENSITIVE | java.util.regex.Pattern.DOTALL).matcher(body);
        return m.find() ? m.group(1).replaceAll("<[^>]+>", "").trim() : fallbackTitle;
    }
}
