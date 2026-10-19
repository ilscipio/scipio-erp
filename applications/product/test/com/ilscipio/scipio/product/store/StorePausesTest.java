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
package com.ilscipio.scipio.product.store;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.sql.Timestamp;
import java.time.Instant;

import org.junit.jupiter.api.Test;

/** R4-15 (J5-01): the one rule of the store pause and its input checks. */
public class StorePausesTest {
    static final Instant NOW = Instant.parse("2026-10-10T10:00:00Z");

    private static Timestamp at(String instant) {
        return Timestamp.from(Instant.parse(instant));
    }

    @Test
    void aRowWithoutABackOnDayIsPaused() {
        assertTrue(StorePauses.isPaused(at("2026-10-09T08:00:00Z"), null, NOW));
    }

    @Test
    void theBackOnDayEndsThePauseForEachReader() {
        Timestamp since = at("2026-10-09T08:00:00Z");
        assertTrue(StorePauses.isPaused(since, at("2026-10-10T23:59:59Z"), NOW));
        assertFalse(StorePauses.isPaused(since, Timestamp.from(NOW), NOW), "the back-on time itself is live");
        assertFalse(StorePauses.isPaused(since, at("2026-10-10T09:00:00Z"), NOW));
    }

    @Test
    void noRowIsLive() {
        assertFalse(StorePauses.isPaused(null, null, NOW));
        assertFalse(StorePauses.isPaused((org.ofbiz.entity.GenericValue) null, NOW));
    }

    @Test
    void theNoteIsTrimmedAndLimited() {
        assertNull(StorePauses.note("   "));
        assertEquals("On holiday", StorePauses.note("  On holiday "));
        assertEquals(300, StorePauses.note("x".repeat(300)).length());
        assertThrows(IllegalArgumentException.class, () -> StorePauses.note("x".repeat(301)));
    }

    @Test
    void theBackOnDayLiesInTheFuture() {
        assertNull(StorePauses.untilDate(null, NOW));
        assertEquals(at("2026-10-11T23:59:59Z"), StorePauses.untilDate(at("2026-10-11T23:59:59Z"), NOW));
        assertThrows(IllegalArgumentException.class, () -> StorePauses.untilDate(at("2026-10-10T09:59:59Z"), NOW));
    }

    @Test
    void instantsTravelAsIsoWithAZone() {
        assertEquals(at("2026-10-12T23:59:59Z"), StorePauses.instant("2026-10-12T23:59:59Z"));
        assertEquals(at("2026-10-12T21:59:59Z"), StorePauses.instant("2026-10-12T23:59:59+02:00"), "review D7: the zone counts");
        assertNull(StorePauses.instant(" "));
        assertEquals("2026-10-12T23:59:59Z", StorePauses.iso(at("2026-10-12T23:59:59Z")));
        assertNull(StorePauses.iso(null));
    }

    @Test
    void aReconcileIsPendingUntilOneFinishesForTheNewestWrite() {
        assertFalse(StorePauses.reconcilePending(null, null), "no write yet");
        assertTrue(StorePauses.reconcilePending(1L, null), "review C2: a pause without a finished reconcile");
        assertTrue(StorePauses.reconcilePending(2L, 1L), "a resume after the reconcile of the pause");
        assertFalse(StorePauses.reconcilePending(2L, 2L));
    }

    @Test
    void twoRegistrationsOfOneHourShareTheJobId() {
        assertEquals("PSSWP_26101010", StorePauses.jobId("PSSWP", NOW));
        assertEquals(StorePauses.jobId("X", NOW), StorePauses.jobId("X", NOW.plusSeconds(59 * 60)), "review C5: one key for one hour");
    }
}
