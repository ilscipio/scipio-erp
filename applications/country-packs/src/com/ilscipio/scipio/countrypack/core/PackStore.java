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

import java.util.List;
import java.util.Map;

/**
 * The store data that the pack engine reads and writes. {@code EntityPackStore} implements it on the entity engine;
 * the unit tests use a memory version. The engine holds the rules, this interface holds no rule.
 *
 * <p>SCIPIO: 4.0.0: Added (W1-03).</p>
 */
public interface PackStore {

    final class Assignment {
        private final String storeId;
        private final String packId;
        private final Role role;
        private final int version;

        public Assignment(String storeId, String packId, Role role, int version) {
            this.storeId = storeId;
            this.packId = packId;
            this.role = role;
            this.version = version;
        }

        public String storeId() {
            return storeId;
        }

        public String packId() {
            return packId;
        }

        public Role role() {
            return role;
        }

        public int version() {
            return version;
        }
    }

    final class TaskRow {
        private final String storeId;
        private final String packId;
        private final String taskId;
        private final TaskStatus status;
        private final boolean required;
        private final String title;
        private final String kind;
        private final String link;
        private final String numberLabel;
        private final String detail;
        private final String value;
        private final String note;
        private final int sinceVersion;

        public TaskRow(String storeId, String packId, String taskId, TaskStatus status, boolean required, String title,
                       String kind, String link, String numberLabel, String detail, String value, String note, int sinceVersion) {
            this.storeId = storeId;
            this.packId = packId;
            this.taskId = taskId;
            this.status = status;
            this.required = required;
            this.title = title;
            this.kind = kind;
            this.link = link;
            this.numberLabel = numberLabel;
            this.detail = detail;
            this.value = value;
            this.note = note;
            this.sinceVersion = sinceVersion;
        }

        public TaskRow withStatus(TaskStatus s, String newValue, String newNote) {
            return new TaskRow(storeId, packId, taskId, s, required, title, kind, link, numberLabel, detail, newValue, newNote, sinceVersion);
        }

        /** The same row with the definition fields of the pack; status, value and note stay. */
        public TaskRow withDefinition(boolean newRequired, String newTitle, String newLink, String newNumberLabel, String newDetail) {
            return new TaskRow(storeId, packId, taskId, status, newRequired, newTitle, kind, newLink, newNumberLabel, newDetail, value, note, sinceVersion);
        }

        public String storeId() {
            return storeId;
        }

        public String packId() {
            return packId;
        }

        public String taskId() {
            return taskId;
        }

        public TaskStatus status() {
            return status;
        }

        public boolean required() {
            return required;
        }

        public String title() {
            return title;
        }

        public String kind() {
            return kind;
        }

        public String link() {
            return link;
        }

        public String numberLabel() {
            return numberLabel;
        }

        public String detail() {
            return detail;
        }

        public String value() {
            return value;
        }

        public String note() {
            return note;
        }

        public int sinceVersion() {
            return sinceVersion;
        }
    }

    boolean storeExists(String storeId);

    List<Assignment> assignments(String storeId);

    /** The assignments of all stores in this database. */
    List<Assignment> allAssignments();

    void saveAssignment(Assignment assignment);

    List<TaskRow> tasks(String storeId, String packId);

    /** Creates the row, or replaces the row with the same store, pack and task id. */
    void saveTask(TaskRow task);

    /**
     * Saves a registration number as an EPR registration of the store owner (compliance component).
     *
     * @return false when the store has no owner party; the number then stays in the task only
     */
    boolean recordRegistration(String storeId, String schemeId, String countryGeoId, String number);

    /**
     * Closes the EPR registration with this number, when this store is the only user of it (another store with the same
     * pay-to party can use the same registration). Does nothing when there is none.
     */
    void closeRegistration(String storeId, String schemeId, String countryGeoId, String number);

    /** Sets the consent mode EU_OPT_IN when the profile has none or has US_OPT_OUT; any other value stays. */
    void raiseConsentMode(String storeId);

    /** True when the store has a published text of the type in the language: the locale is the language or starts with "language_" (de_DE). */
    boolean hasPublishedDocument(String storeId, String docTypeId, String locale);

    /** Publishes a new version of a legal text. The engine calls it only when the store has no published text of that type and locale. */
    void publishDocument(String storeId, String docTypeId, String locale, String title, String body, String changeNote);

    /**
     * Makes sure the store has a compliance profile. Adds the jurisdictions to the list that exists. Sets a default only in a
     * field that is empty: the engine never changes a value that the seller has set.
     */
    void mergeProfile(String storeId, List<String> jurisdictions, Map<String, String> defaults);
}
