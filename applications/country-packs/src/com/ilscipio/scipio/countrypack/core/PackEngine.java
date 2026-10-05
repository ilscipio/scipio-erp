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

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The rules of the country-pack framework (blueprint section 6). All store access goes through {@link PackStore}.
 *
 * <ul>
 * <li>One home pack for each store; any number of market packs. A second home pack is refused.</li>
 * <li>Applying a pack is safe to repeat. It creates only what is missing: tasks, and legal texts for a type and locale that
 * the store has not published. It never changes a text that the seller edited or published (D11).</li>
 * <li>A new pack version adds its new tasks, and adds a review task for each template that changed. It changes no text.</li>
 * <li>A market opens only when its required tasks are closed (done, done by Scipio or not needed).</li>
 * </ul>
 *
 * <p>SCIPIO: 4.0.0: Added (W1-03).</p>
 */
public final class PackEngine {

    /** What one call of {@link #apply} did. */
    public static final class ApplyResult {
        private final String packId;
        private final Role role;
        private final int fromVersion;
        private final int toVersion;
        private final List<String> createdTasks;
        private final List<String> reviewTasks;
        private final List<String> publishedDocuments;
        private final boolean marketOpen;

        public ApplyResult(String packId, Role role, int fromVersion, int toVersion, List<String> createdTasks,
                           List<String> reviewTasks, List<String> publishedDocuments, boolean marketOpen) {
            this.packId = packId;
            this.role = role;
            this.fromVersion = fromVersion;
            this.toVersion = toVersion;
            this.createdTasks = createdTasks;
            this.reviewTasks = reviewTasks;
            this.publishedDocuments = publishedDocuments;
            this.marketOpen = marketOpen;
        }

        public String packId() {
            return packId;
        }

        public Role role() {
            return role;
        }

        public int fromVersion() {
            return fromVersion;
        }

        public int toVersion() {
            return toVersion;
        }

        public List<String> createdTasks() {
            return createdTasks;
        }

        public List<String> reviewTasks() {
            return reviewTasks;
        }

        public List<String> publishedDocuments() {
            return publishedDocuments;
        }

        public boolean marketOpen() {
            return marketOpen;
        }

        public Map<String, Object> toMap() {
            Map<String, Object> m = new LinkedHashMap<>();
            m.put("packId", packId);
            m.put("role", role.name());
            m.put("fromVersion", fromVersion);
            m.put("toVersion", toVersion);
            m.put("createdTasks", createdTasks);
            m.put("reviewTasks", reviewTasks);
            m.put("publishedDocuments", publishedDocuments);
            m.put("marketOpen", marketOpen);
            return m;
        }
    }

    /** Profile fields that a market pack fills when they are empty (the rules of the buyer country, not the home defaults). */
    private static final Set<String> MARKET_PROFILE_FIELDS = Set.of("withdrawalDays", "legalGuaranteeYears", "euOrderButton");
    /** One lock for each store id: two calls for one store run one after the other in this server. */
    private static final ConcurrentHashMap<String, Object> LOCKS = new ConcurrentHashMap<>();

    private static Object lock(String storeId) {
        return LOCKS.computeIfAbsent(String.valueOf(storeId), k -> new Object());
    }

    private final PackRegistry registry;
    private final PackStore store;
    private final TemplateSource templates;

    public PackEngine(PackRegistry registry, PackStore store, TemplateSource templates) {
        this.registry = registry;
        this.store = store;
        this.templates = templates;
    }

    /** @param requestedRole null: HOME when the store has no home pack, else MARKET */
    public ApplyResult apply(String storeId, String packId, Role requestedRole) {
        synchronized (lock(storeId)) {
            return applyLocked(storeId, packId, requestedRole);
        }
    }

    private ApplyResult applyLocked(String storeId, String packId, Role requestedRole) {
        if (!store.storeExists(storeId)) {
            throw new IllegalArgumentException("Unknown product store: " + storeId);
        }
        Pack pack = registry.get(packId);
        List<PackStore.Assignment> existing = store.assignments(storeId);
        PackStore.Assignment home = existing.stream().filter(a -> a.role() == Role.HOME).findFirst().orElse(null);
        PackStore.Assignment current = existing.stream().filter(a -> a.packId().equals(pack.id())).findFirst().orElse(null);
        Role role = requestedRole != null ? requestedRole : (current != null ? current.role() : (home == null ? Role.HOME : Role.MARKET));
        if (current != null && current.role() != role) {
            throw new IllegalArgumentException("Pack " + pack.id() + " is already a " + current.role() + " pack of this store.");
        }
        if (role == Role.HOME && home != null && !home.packId().equals(pack.id())) {
            throw new IllegalArgumentException("The store has the home pack " + home.packId() + ". Add " + pack.id() + " as a market.");
        }
        int from = current == null ? 0 : current.version();

        // 1. profile: jurisdictions of every pack; defaults from the home pack. A market fills the empty fields of the buyer
        //    rules (withdrawal, guarantee, order button) and makes the consent mode stricter (EU_OPT_IN over US_OPT_OUT).
        if (role == Role.HOME) {
            store.mergeProfile(storeId, pack.jurisdictions(), pack.profile());
        } else {
            Map<String, String> marketDefaults = new LinkedHashMap<>();
            pack.profile().forEach((k, v) -> {
                if (MARKET_PROFILE_FIELDS.contains(k)) {
                    marketDefaults.put(k, v);
                }
            });
            store.mergeProfile(storeId, pack.jurisdictions(), marketDefaults);
            if ("EU_OPT_IN".equals(pack.profile().get("consentMode"))) {
                store.raiseConsentMode(storeId);
            }
        }

        // 2. tasks: create the missing ones
        List<PackStore.TaskRow> rows = store.tasks(storeId, pack.id());
        List<String> created = new ArrayList<>();
        for (Pack.Task t : pack.tasks()) {
            if (!pack.appliesTo(t.scope(), role)) {
                continue;
            }
            PackStore.TaskRow row = rows.stream().filter(r -> r.taskId().equals(t.id())).findFirst().orElse(null);
            if (row == null) {
                store.saveTask(new PackStore.TaskRow(storeId, pack.id(), t.id(), TaskStatus.NEEDS_YOU, t.required(), t.title(), t.kind(),
                        t.link(), t.numberLabel(), t.detail(), null, null, t.sinceVersion()));
                created.add(t.id());
            } else if (row.status() == TaskStatus.NEEDS_YOU && changed(row, t)) {
                // a task that still needs the seller shows the current definition; status and value never change here
                store.saveTask(row.withDefinition(t.required(), t.title(), t.link(), t.numberLabel(), t.detail()));
            }
        }

        // 3. legal texts: publish the missing ones; a changed template of a newer version becomes a review task
        List<String> published = new ArrayList<>();
        List<String> review = new ArrayList<>();
        List<String> skipped = new ArrayList<>();
        String language = TemplateSource.language(pack.locale());
        for (Pack.Template t : pack.legalTemplates()) {
            if (!pack.appliesTo(t.scope(), role)) {
                continue;
            }
            if (!store.hasPublishedDocument(storeId, t.docTypeId(), language)) {
                Optional<String> text = templates.read(pack, t);
                if (text.isPresent()) {
                    String body = TemplateSource.withMarker(text.get(), language);
                    store.publishDocument(storeId, t.docTypeId(), language, TemplateSource.title(body, t.slug()), body,
                            "Template of the country pack " + pack.id() + " version " + pack.version() + ". " + TemplateSource.MARKER + ".");
                    published.add(t.slug());
                }
            } else if (from == 0) {
                skipped.add(t.slug()); // a text of this language exists already (another pack or the seller): the pack does not replace it
            } else if (t.sinceVersion() > from) {
                String id = "review-" + t.slug() + "-v" + t.sinceVersion();
                if (store.tasks(storeId, pack.id()).stream().noneMatch(r -> r.taskId().equals(id))) {
                    store.saveTask(new PackStore.TaskRow(storeId, pack.id(), id, TaskStatus.NEEDS_YOU, false,
                            "Check the changed template: " + t.slug(), "decision", null, null,
                            "The template of the country pack " + pack.id() + " changed in version " + t.sinceVersion()
                                    + ". Compare it with your text and decide. Scipio does not change your text.", null, null, t.sinceVersion()));
                    review.add(id);
                }
            }
        }

        if (!skipped.isEmpty()) {
            String id = "check-texts-" + pack.id();
            if (store.tasks(storeId, pack.id()).stream().noneMatch(r -> r.taskId().equals(id))) {
                store.saveTask(new PackStore.TaskRow(storeId, pack.id(), id, TaskStatus.NEEDS_YOU, role == Role.MARKET,
                        "Check the legal texts for " + pack.country(), "decision", null, null,
                        "The store has legal texts in the language \"" + language + "\" already: " + String.join(", ", skipped)
                                + ". The country pack " + pack.id() + " does not replace them, so buyers in " + pack.country()
                                + " see these texts. Check that they fit the law of " + pack.country() + " and publish your own text where they do not.",
                        null, null, pack.version()));
                created.add(id);
            }
        }

        store.saveAssignment(new PackStore.Assignment(storeId, pack.id(), role, Math.max(from, pack.version())));
        return new ApplyResult(pack.id(), role, from, Math.max(from, pack.version()), created, review, published, marketOpen(storeId, pack.id()));
    }

    /**
     * Applies each pack again to each store that uses an older version of it: a new pack version creates its tasks in the stores
     * that it concerns (blueprint section 6). A changed template becomes a review task; no text changes.
     */
    public List<ApplyResult> upgradeAll() {
        return upgradeAll(null);
    }

    /** @param onlyStoreId null: every store; else only this store (it must exist) */
    public List<ApplyResult> upgradeAll(String onlyStoreId) {
        if (onlyStoreId != null && !store.storeExists(onlyStoreId)) {
            throw new IllegalArgumentException("Unknown product store: " + onlyStoreId);
        }
        List<ApplyResult> out = new ArrayList<>();
        for (PackStore.Assignment a : onlyStoreId == null ? store.allAssignments() : store.assignments(onlyStoreId)) {
            Pack pack;
            try {
                pack = registry.get(a.packId());
            } catch (IllegalArgumentException e) {
                continue; // the pack file is gone: leave the store as it is
            }
            if (pack.version() > a.version()) {
                out.add(apply(a.storeId(), a.packId(), a.role()));
            }
        }
        return out;
    }

    /**
     * Changes the state of a task. DONE needs a number when the task asks for one. A task can go back to NEEDS_YOU.
     *
     * @param status null: DONE
     */
    public PackStore.TaskRow completeTask(String storeId, String packId, String taskId, TaskStatus status, String value, String note) {
        synchronized (lock(storeId)) {
            return completeTaskLocked(storeId, packId, taskId, status, value, note);
        }
    }

    private PackStore.TaskRow completeTaskLocked(String storeId, String packId, String taskId, TaskStatus status, String value, String note) {
        String pid = registry.get(packId).id();
        PackStore.TaskRow row = store.tasks(storeId, pid).stream().filter(r -> r.taskId().equals(taskId)).findFirst()
                .orElseThrow(() -> new IllegalArgumentException("Task " + taskId + " does not exist for pack " + pid + " in store " + storeId));
        TaskStatus target = status == null ? TaskStatus.DONE : status;
        if (target == TaskStatus.DONE_BY_SCIPIO) {
            throw new IllegalArgumentException("The state DONE_BY_SCIPIO is set by Scipio only.");
        }
        if (target == TaskStatus.NOT_NEEDED && row.required() && (note == null || note.isBlank())) {
            throw new IllegalArgumentException("Task " + taskId + " is required. Give a note that says why it is not needed.");
        }
        String v = value == null || value.isBlank() ? null : value.trim();
        if (target == TaskStatus.DONE && row.numberLabel() != null && v == null) {
            throw new IllegalArgumentException("Task " + taskId + " needs a number: " + row.numberLabel());
        }
        if (target == TaskStatus.NEEDS_YOU) {
            v = null;
        }
        String newNote = note != null ? note : row.note();
        Pack.Task def = registry.get(pid).tasks().stream().filter(t -> t.id().equals(taskId)).findFirst().orElse(null);
        if (target == TaskStatus.DONE && v != null && def != null && def.eprScheme() != null
                && !store.recordRegistration(storeId, def.eprScheme(), def.eprCountry(), v)) {
            newNote = "The store has no owner party, so the number is not in the imprint yet.";
        }
        if (row.status() == TaskStatus.DONE && target != TaskStatus.DONE && def != null && def.eprScheme() != null && row.value() != null) {
            store.closeRegistration(storeId, def.eprScheme(), def.eprCountry(), row.value());
        }
        PackStore.TaskRow updated = row.withStatus(target, v != null ? v : row.value(), newNote);
        store.saveTask(updated);
        return updated;
    }

    private static boolean changed(PackStore.TaskRow r, Pack.Task t) {
        return r.required() != t.required() || !java.util.Objects.equals(r.title(), t.title()) || !java.util.Objects.equals(r.detail(), t.detail())
                || !java.util.Objects.equals(r.link(), t.link()) || !java.util.Objects.equals(r.numberLabel(), t.numberLabel());
    }

    /** True when every required task of the pack in this store is closed. */
    public boolean marketOpen(String storeId, String packId) {
        String pid;
        try {
            pid = registry.get(packId).id();
        } catch (IllegalArgumentException e) {
            pid = packId; // the pack file is gone: read the rows under the id as given
        }
        return store.tasks(storeId, pid).stream().noneMatch(r -> r.required() && !r.status().closed());
    }

    /** The setup status of a store: packs, tasks, counts. This is the extension of {@code scipio://setup/status}. */
    public Map<String, Object> status(String storeId) {
        List<Map<String, Object>> packs = new ArrayList<>();
        int needsYou = 0;
        int blocking = 0;
        for (PackStore.Assignment a : store.assignments(storeId)) {
            Pack pack;
            try {
                pack = registry.get(a.packId());
            } catch (IllegalArgumentException e) {
                pack = null; // the pack file is gone: show the rows that the store has
            }
            List<Map<String, Object>> tasks = new ArrayList<>();
            for (PackStore.TaskRow r : store.tasks(storeId, a.packId())) {
                Map<String, Object> t = new LinkedHashMap<>();
                t.put("taskId", r.taskId());
                t.put("title", r.title());
                t.put("kind", r.kind());
                t.put("status", r.status().name());
                t.put("required", r.required());
                t.put("link", r.link());
                t.put("numberLabel", r.numberLabel());
                t.put("value", r.value());
                t.put("detail", r.detail());
                t.put("note", r.note());
                tasks.add(t);
                if (r.status() == TaskStatus.NEEDS_YOU) {
                    needsYou++;
                    if (r.required()) {
                        blocking++;
                    }
                }
            }
            Map<String, Object> p = new LinkedHashMap<>();
            p.put("packId", a.packId());
            p.put("name", pack != null ? pack.name() : a.packId());
            p.put("role", a.role().name());
            p.put("version", a.version());
            p.put("latestVersion", pack != null ? pack.version() : a.version());
            p.put("upgradeAvailable", pack != null && pack.version() > a.version());
            p.put("packFileMissing", pack == null);
            p.put("marketOpen", marketOpen(storeId, a.packId()));
            p.put("tasks", tasks);
            packs.add(p);
        }
        Map<String, Object> out = new LinkedHashMap<>();
        out.put("productStoreId", storeId);
        out.put("packs", packs);
        out.put("needsYou", needsYou);
        out.put("blockingMarkets", blocking);
        return out;
    }

    /** Short description of every pack, for the list tool. */
    public List<Map<String, Object>> list() {
        List<Map<String, Object>> out = new ArrayList<>();
        for (Pack p : registry.all()) {
            Map<String, Object> m = new LinkedHashMap<>();
            m.put("packId", p.id());
            m.put("name", p.name());
            m.put("version", p.version());
            m.put("country", p.country());
            m.put("locale", p.locale());
            m.put("currency", p.currency());
            m.put("jurisdictions", p.jurisdictions());
            m.put("tasks", p.tasks().size());
            m.put("legalTexts", p.legalTemplates().stream().map(Pack.Template::slug).collect(java.util.stream.Collectors.toList()));
            out.add(m);
        }
        return out;
    }
}
