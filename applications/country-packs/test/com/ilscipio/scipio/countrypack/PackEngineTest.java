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
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.ilscipio.scipio.countrypack.core.PackEngine;
import com.ilscipio.scipio.countrypack.core.PackRegistry;
import com.ilscipio.scipio.countrypack.core.PackStore;
import com.ilscipio.scipio.countrypack.core.Role;
import com.ilscipio.scipio.countrypack.core.TaskStatus;
import com.ilscipio.scipio.countrypack.core.TemplateSource;

/** The W1-03 rules on a memory store: a new DE store shows the LUCID task and live legal texts from the templates. */
class PackEngineTest {
    private final PackRegistry registry = Packs.registry();
    private MemoryPackStore store;
    private PackEngine engine;

    @BeforeEach
    void setUp() {
        store = new MemoryPackStore("S1", "S2");
        engine = new PackEngine(registry, store, Packs.templates(registry));
    }

    private PackStore.TaskRow task(String storeId, String packId, String taskId) {
        return store.tasks(storeId, packId).stream().filter(t -> t.taskId().equals(taskId)).findFirst().orElseThrow();
    }

    @Test
    void newDeStoreShowsTheLucidTaskAndLiveLegalTexts() {
        PackEngine.ApplyResult r = engine.apply("S1", "de", null);

        assertEquals(Role.HOME, r.role());
        PackStore.TaskRow lucid = task("S1", "de", "lucid");
        assertEquals(TaskStatus.NEEDS_YOU, lucid.status());
        assertTrue(lucid.required());
        assertEquals("LUCID registration number", lucid.numberLabel());
        assertFalse(r.marketOpen(), "the market is closed while required tasks need you");

        assertEquals(List.of("imprint", "terms", "privacy", "cookies", "withdrawal", "returns", "accessibility"), r.publishedDocuments());
        for (String docType : List.of("LEGDOC_IMPRINT", "LEGDOC_TERMS", "LEGDOC_PRIVACY", "LEGDOC_COOKIES", "LEGDOC_WITHDRAWAL", "LEGDOC_RETURNS", "LEGDOC_ACCESSIBILITY")) {
            String body = store.documents.get(MemoryPackStore.docKey("S1", docType, "de"));
            assertNotNull(body, docType + " is live");
            assertTrue(body.contains(TemplateSource.MARKER), docType + " is marked as a template");
        }
        assertEquals("Impressum", store.titles.get(MemoryPackStore.docKey("S1", "LEGDOC_IMPRINT", "de")));
        assertTrue(store.jurisdictions.containsAll(List.of("EU", "DE")));
        assertEquals("EU_OPT_IN", store.profile.get("consentMode"));
        assertEquals("DEU", store.profile.get("countryGeoId"));
    }

    @Test
    void finishingLucidNeedsTheNumberAndSavesAnEprRegistration() {
        engine.apply("S1", "de", null);
        assertThrows(IllegalArgumentException.class, () -> engine.completeTask("S1", "de", "lucid", null, " ", null));

        engine.completeTask("S1", "de", "lucid", TaskStatus.DONE, "DE3000000000000", null);
        assertEquals(TaskStatus.DONE, task("S1", "de", "lucid").status());
        assertEquals("DE3000000000000", task("S1", "de", "lucid").value());
        assertEquals(List.of("EPR_PACKAGING|DEU|DE3000000000000"), store.registrations);

        // the market opens when every required task is closed
        assertFalse(engine.marketOpen("S1", "de"));
        engine.completeTask("S1", "de", "seller-data", null, null, null);
        engine.completeTask("S1", "de", "dual-system", null, null, null);
        assertFalse(engine.marketOpen("S1", "de"), "gpsr still needs you");
        engine.completeTask("S1", "de", "gpsr", TaskStatus.NOT_NEEDED, null, "digital goods only");
        assertTrue(engine.marketOpen("S1", "de"));

        // the task can go back to NEEDS_YOU
        engine.completeTask("S1", "de", "gpsr", TaskStatus.NEEDS_YOU, null, null);
        assertFalse(engine.marketOpen("S1", "de"));
    }

    @Test
    void numberStaysInTheTaskWhenTheStoreHasNoOwnerParty() {
        store.hasOwnerParty = false;
        engine.apply("S1", "de", null);
        PackStore.TaskRow row = engine.completeTask("S1", "de", "lucid", TaskStatus.DONE, "DE1", null);
        assertEquals("DE1", row.value());
        assertTrue(row.note().contains("no owner party"));
        assertTrue(store.registrations.isEmpty());
    }

    @Test
    void applyIsSafeToRepeatAndNeverChangesASellerText() {
        engine.apply("S1", "de", null);
        String key = MemoryPackStore.docKey("S1", "LEGDOC_IMPRINT", "de");
        store.documents.put(key, "<h1>Impressum</h1><p>Meine eigene Fassung</p>");
        engine.completeTask("S1", "de", "lucid", TaskStatus.DONE, "DE1", null);

        PackEngine.ApplyResult again = engine.apply("S1", "de", null);

        assertEquals(List.of(), again.publishedDocuments());
        assertEquals(List.of(), again.createdTasks());
        assertEquals("<h1>Impressum</h1><p>Meine eigene Fassung</p>", store.documents.get(key));
        assertEquals(TaskStatus.DONE, task("S1", "de", "lucid").status(), "a finished task stays finished");
    }

    @Test
    void oneHomePackAndAnyNumberOfMarkets() {
        engine.apply("S1", "us", null);
        assertEquals(Role.HOME, store.assignments("S1").get(0).role());
        PackEngine.ApplyResult de = engine.apply("S1", "de", null);
        assertEquals(Role.MARKET, de.role(), "the second pack is a market");
        assertThrows(IllegalArgumentException.class, () -> engine.apply("S1", "at", Role.HOME));
        assertThrows(IllegalArgumentException.class, () -> engine.apply("S1", "de", Role.HOME), "de is a market already");
        assertThrows(IllegalArgumentException.class, () -> engine.apply("S1", "xx", null));
        assertThrows(IllegalArgumentException.class, () -> engine.apply("NOPE", "de", null));

        // a market brings its tasks (LUCID) and the texts in its language; the US texts in English stay
        assertEquals(TaskStatus.NEEDS_YOU, task("S1", "de", "lucid").status());
        assertTrue(store.documents.containsKey(MemoryPackStore.docKey("S1", "LEGDOC_TERMS", "en")));
        assertTrue(store.documents.containsKey(MemoryPackStore.docKey("S1", "LEGDOC_WITHDRAWAL", "de")));
        // a market takes the jurisdictions and the stricter consent mode, and fills the empty withdrawal fields
        assertEquals("EU_OPT_IN", store.profile.get("consentMode"));
        assertEquals("14", store.profile.get("withdrawalDays"));
        assertEquals("30", store.profile.get("returnDays"), "the home value stays");
        assertEquals("USA", store.profile.get("countryGeoId"), "the market does not change the home country");
        assertTrue(store.jurisdictions.containsAll(List.of("US", "EU", "DE")));
        // the market is closed until its required tasks are closed; the home pack is open (US has no required task after seller-data)
        assertFalse(de.marketOpen());
    }

    @Test
    void austriaHasItsOwnPackagingSchemeAndNoLucid() {
        engine.apply("S2", "at", null);
        assertEquals(TaskStatus.NEEDS_YOU, task("S2", "at", "packaging-scheme").status());
        assertTrue(store.tasks("S2", "at").stream().noneMatch(t -> t.taskId().equals("lucid")));
        assertTrue(store.documents.get(MemoryPackStore.docKey("S2", "LEGDOC_IMPRINT", "de")).contains("Firmenbuch"));
    }

    @Test
    void usPackPublishesEnglishTextsWithTheMark() {
        PackEngine.ApplyResult r = engine.apply("S1", "us", null);
        assertEquals(List.of("terms", "privacy", "returns", "notice-at-collection", "accessibility"), r.publishedDocuments());
        for (Map.Entry<String, String> d : store.documents.entrySet()) {
            assertTrue(d.getValue().contains(TemplateSource.MARKER), d.getKey());
            assertTrue(d.getKey().endsWith("|en"), d.getKey());
        }
        assertEquals("30", store.profile.get("returnDays"));
    }

    @Test
    void newPackVersionAddsTasksAndReviewTasksButChangesNoText(@TempDir Path dir) throws Exception {
        // a copy of the de pack at version 1 with an imprint, then the same pack at version 2 with a changed imprint and a new task
        Path de = Files.createDirectories(dir.resolve("de"));
        Files.createDirectories(de.resolve("templates"));
        Files.writeString(de.resolve("templates").resolve("imprint.html"), "<h1>Impressum</h1><p>" + TemplateSource.MARKER + " v1</p>");
        String v1 = "{\"id\":\"de\",\"version\":1,\"name\":\"Germany\",\"country\":\"DE\",\"locale\":\"de\",\"currency\":\"EUR\",\"jurisdictions\":[\"EU\"],"
                + "\"legalTemplates\":[{\"slug\":\"imprint\",\"docTypeId\":\"LEGDOC_IMPRINT\"}],"
                + "\"tasks\":[{\"id\":\"a\",\"title\":\"Task A\",\"required\":true}]}";
        Files.writeString(de.resolve("pack.json"), v1);
        MemoryPackStore s = new MemoryPackStore("S1");
        PackRegistry r1 = new PackRegistry(dir);
        new PackEngine(r1, s, new TemplateSource(r1, (a, b) -> null)).apply("S1", "de", null);
        String published = s.documents.get(MemoryPackStore.docKey("S1", "LEGDOC_IMPRINT", "de"));

        Files.writeString(de.resolve("templates").resolve("imprint.html"), "<h1>Impressum</h1><p>" + TemplateSource.MARKER + " v2</p>");
        Files.writeString(de.resolve("pack.json"), v1.replace("\"version\":1", "\"version\":2")
                .replace("\"docTypeId\":\"LEGDOC_IMPRINT\"", "\"docTypeId\":\"LEGDOC_IMPRINT\",\"sinceVersion\":2")
                .replace("]}", ",{\"id\":\"b\",\"title\":\"Task B\",\"sinceVersion\":2}]}"));
        PackRegistry r2 = new PackRegistry(dir);
        List<PackEngine.ApplyResult> ups = new PackEngine(r2, s, new TemplateSource(r2, (a, b) -> null)).upgradeAll();
        assertEquals(1, ups.size(), "one store is behind");
        PackEngine.ApplyResult up = ups.get(0);

        assertEquals(1, up.fromVersion());
        assertEquals(2, up.toVersion());
        assertEquals(List.of("b"), up.createdTasks());
        assertEquals(List.of("review-imprint-v2"), up.reviewTasks());
        assertEquals(List.of(), up.publishedDocuments());
        assertEquals(published, s.documents.get(MemoryPackStore.docKey("S1", "LEGDOC_IMPRINT", "de")), "the seller text stays");
        assertEquals(2, s.assignments("S1").get(0).version());
        // upgradeAll finds no store that is behind now
        assertEquals(0, new PackEngine(r2, s, new TemplateSource(r2, (a, b) -> null)).upgradeAll().size());
        // a second apply adds no second review task
        assertEquals(List.of(), new PackEngine(r2, s, new TemplateSource(r2, (a, b) -> null)).apply("S1", "de", null).reviewTasks());
    }

    @Test
    void statusListsPacksAndTasks() {
        engine.apply("S1", "de", null);
        Map<String, Object> status = engine.status("S1");
        assertEquals("S1", status.get("productStoreId"));
        assertEquals(7, status.get("needsYou"));
        assertEquals(4, status.get("blockingMarkets"));
    }

    @Test
    void aSellerTextInARegionalLocaleCountsAsATextOfTheLanguage() {
        store.documents.put(MemoryPackStore.docKey("S1", "LEGDOC_IMPRINT", "de_DE"), "<h1>Impressum</h1><p>Mein Text</p>");

        PackEngine.ApplyResult r = engine.apply("S1", "de", null);

        assertFalse(r.publishedDocuments().contains("imprint"), "no template text behind the seller text");
        assertFalse(store.documents.containsKey(MemoryPackStore.docKey("S1", "LEGDOC_IMPRINT", "de")));
        assertTrue(store.documents.containsKey(MemoryPackStore.docKey("S1", "LEGDOC_TERMS", "de")));
        assertEquals(TaskStatus.NEEDS_YOU, task("S1", "de", "check-texts-de").status());
    }

    @Test
    void theAustrianMarketAfterGermanyGetsARequiredCheckTaskAndKeepsTheGermanTexts() {
        engine.apply("S1", "de", null);
        String imprint = store.documents.get(MemoryPackStore.docKey("S1", "LEGDOC_IMPRINT", "de"));

        PackEngine.ApplyResult at = engine.apply("S1", "at", null);

        assertEquals(Role.MARKET, at.role());
        assertEquals(List.of(), at.publishedDocuments(), "both packs use the language de: nothing is published twice");
        assertEquals(imprint, store.documents.get(MemoryPackStore.docKey("S1", "LEGDOC_IMPRINT", "de")));
        PackStore.TaskRow check = task("S1", "at", "check-texts-at");
        assertTrue(check.required());
        assertEquals(TaskStatus.NEEDS_YOU, check.status());
        assertTrue(check.detail().contains("imprint"));
        assertFalse(engine.marketOpen("S1", "at"));
        // a second apply adds no second task
        assertEquals(List.of(), engine.apply("S1", "at", null).createdTasks());
    }

    @Test
    void aMarketWithoutSellerConsentGetsTheStricterModeButAnAutoModeStays() {
        store.profile.put("consentMode", "AUTO");
        engine.apply("S1", "us", null);
        engine.apply("S1", "de", null);
        assertEquals("AUTO", store.profile.get("consentMode"));
    }

    @Test
    void doneByScipioIsRefusedAndARequiredTaskNeedsANoteToBeNotNeeded() {
        engine.apply("S1", "de", null);
        assertThrows(IllegalArgumentException.class, () -> engine.completeTask("S1", "de", "lucid", TaskStatus.DONE_BY_SCIPIO, "DE1", "x"));
        assertEquals(TaskStatus.NEEDS_YOU, task("S1", "de", "lucid").status());

        assertThrows(IllegalArgumentException.class, () -> engine.completeTask("S1", "de", "lucid", TaskStatus.NOT_NEEDED, null, null));
        assertThrows(IllegalArgumentException.class, () -> engine.completeTask("S1", "de", "lucid", TaskStatus.NOT_NEEDED, null, "  "));
        assertEquals(TaskStatus.NEEDS_YOU, task("S1", "de", "lucid").status());

        engine.completeTask("S1", "de", "lucid", TaskStatus.NOT_NEEDED, null, "digital goods only");
        assertEquals(TaskStatus.NOT_NEEDED, task("S1", "de", "lucid").status());
        // a task that is not required needs no note
        engine.completeTask("S1", "de", "weee", TaskStatus.NOT_NEEDED, null, null);
        assertEquals(TaskStatus.NOT_NEEDED, task("S1", "de", "weee").status());
    }

    @Test
    void leavingDoneClosesTheRegistrationAndAnUpgradeRefreshesOpenTasksOnly() {
        engine.apply("S1", "de", null);
        engine.completeTask("S1", "de", "lucid", TaskStatus.DONE, "DE1", null);
        engine.completeTask("S1", "de", "lucid", TaskStatus.NEEDS_YOU, null, null);
        assertEquals(List.of("EPR_PACKAGING|DEU|DE1"), store.closedRegistrations);

        // the pack file changes a title; the open task shows it, the finished task does not change
        PackStore.TaskRow open = task("S1", "de", "gpsr");
        PackStore.TaskRow old = new PackStore.TaskRow("S1", "de", "gpsr", TaskStatus.NEEDS_YOU, false, "Old title", open.kind(), null, null, "Old", null, null, 1);
        store.saveTask(old);
        store.saveTask(new PackStore.TaskRow("S1", "de", "seller-data", TaskStatus.DONE, false, "Old", "info", null, null, "Old", null, "mine", 1));

        engine.apply("S1", "de", null);

        assertEquals(open.title(), task("S1", "de", "gpsr").title());
        assertTrue(task("S1", "de", "gpsr").required());
        assertEquals("Old", task("S1", "de", "seller-data").title());
        assertEquals(TaskStatus.DONE, task("S1", "de", "seller-data").status());
    }

    @Test
    void statusSurvivesARemovedPackFile() {
        engine.apply("S1", "de", null);
        store.saveAssignment(new PackStore.Assignment("S1", "gone", Role.MARKET, 1));
        Map<String, Object> status = engine.status("S1");
        assertEquals(2, ((List<?>) status.get("packs")).size());
        assertEquals(0, engine.upgradeAll("S1").size(), "upgradeAll skips the missing pack file");
        assertThrows(IllegalArgumentException.class, () -> engine.upgradeAll("NOPE"));
    }
}
