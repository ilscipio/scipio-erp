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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.junit.jupiter.api.Test;

import com.ilscipio.scipio.countrypack.core.Pack;
import com.ilscipio.scipio.countrypack.core.PackRegistry;
import com.ilscipio.scipio.countrypack.core.TemplateSource;

/** Checks the pack files: they parse, they are sound, and every legal text exists, carries the mark and uses only known placeholders. */
class PackFilesTest {
    /** The placeholders that LegalDocumentWorker.getTokenValues fills. */
    private static final Set<String> TOKENS = Set.of("cookies.table", "doc.lastUpdated", "doc.version", "dpo.contact", "epr.numbers",
            "guarantee.years", "retention.years", "returns.days", "services.table", "store.addressLine", "store.address", "store.city",
            "store.country", "store.email", "store.legalName", "store.name", "store.phone", "store.postalCode", "store.registerCourt",
            "store.registerNumber", "store.representedBy", "store.vatId", "supervisory.authority", "withdrawal.days");
    private static final Pattern TOKEN = Pattern.compile("\\{\\{\\s*([a-zA-Z][a-zA-Z0-9_.]*)\\s*}}");

    private final PackRegistry registry = Packs.registry();

    @Test
    void packsUsAtDeExistAndAreSound() {
        assertEquals(Set.of("at", "de", "us"), registry.all().stream().map(Pack::id).collect(java.util.stream.Collectors.toSet()));
        for (Pack p : registry.all()) {
            assertEquals(List.of(), registry.problems(p), p.id());
            assertFalse(p.verify().isEmpty(), p.id() + " lists the facts to verify");
            assertFalse(p.legalTemplates().isEmpty(), p.id());
        }
    }

    @Test
    void lucidTaskOfGermany() {
        Pack.Task lucid = registry.get("de").tasks().stream().filter(t -> t.id().equals("lucid")).findFirst().orElseThrow();
        assertTrue(lucid.required());
        assertEquals("registration", lucid.kind());
        assertNotNull(lucid.numberLabel(), "a number field");
        assertNotNull(lucid.link(), "a link");
        assertEquals("EPR_PACKAGING", lucid.eprScheme());
        assertEquals("DEU", lucid.eprCountry());
        assertTrue(registry.get("at").tasks().stream().noneMatch(t -> t.id().equals("lucid")), "Austria has its own scheme");
    }

    @Test
    void everyLegalTextExistsInThePackLanguageAndCarriesTheMark() throws Exception {
        Set<String> docTypes = docTypeIds();
        TemplateSource source = Packs.templates(registry);
        for (Pack p : registry.all()) {
            String language = p.locale();
            for (Pack.Template t : p.legalTemplates()) {
                assertTrue(docTypes.contains(t.docTypeId()), p.id() + "/" + t.slug() + ": unknown doc type " + t.docTypeId());
                String text = source.read(p, t).orElse(null);
                assertNotNull(text, p.id() + "/" + t.slug() + " has no text");
                Path own = registry.templateFile(p, t.slug());
                if (!Files.exists(own)) {
                    assertNotNull(Packs.complianceTemplate(t.slug(), language), p.id() + "/" + t.slug() + ": no compliance template in " + language);
                } else {
                    assertTrue(text.contains(TemplateSource.MARKER), own + " carries the mark in the file");
                }
                String marked = TemplateSource.withMarker(text, language);
                assertTrue(marked.contains(TemplateSource.MARKER), p.id() + "/" + t.slug());
                assertTrue(marked.toLowerCase().contains("<h1"), p.id() + "/" + t.slug() + " has a heading");
                assertTrue(marked.indexOf(TemplateSource.MARKER) < marked.indexOf("</h1>") + 400, "the mark is at the top of " + p.id() + "/" + t.slug());
                Matcher m = TOKEN.matcher(text);
                while (m.find()) {
                    assertTrue(TOKENS.contains(m.group(1)), p.id() + "/" + t.slug() + ": unknown placeholder " + m.group(1));
                }
            }
        }
    }

    @Test
    void withMarkerIsIdempotentAndPutsTheMarkUnderTheHeading() {
        String once = TemplateSource.withMarker("<h1>Terms</h1><p>x</p>", "en");
        assertTrue(once.startsWith("<h1>Terms</h1>"));
        assertTrue(once.contains(TemplateSource.MARKER));
        assertEquals(once, TemplateSource.withMarker(once, "en"));
        assertEquals("Terms", TemplateSource.title(once, "x"));
        assertTrue(TemplateSource.MARKER.contains("–"), "en dash as in the instruction");
    }

    /** The doc types that compliance seeds. */
    private static Set<String> docTypeIds() throws Exception {
        String xml = Files.readString(Packs.ROOT.resolveSibling("compliance").resolve("data").resolve("ComplianceTypeData.xml"), StandardCharsets.UTF_8);
        Set<String> ids = new HashSet<>();
        Matcher m = Pattern.compile("enumId=\"(LEGDOC_[A-Z_]+)\"").matcher(xml);
        while (m.find()) {
            ids.add(m.group(1));
        }
        return ids;
    }
}
