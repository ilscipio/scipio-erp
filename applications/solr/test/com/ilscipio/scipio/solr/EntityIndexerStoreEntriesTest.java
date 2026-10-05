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
package com.ilscipio.scipio.solr;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericPK;

/**
 * W1-08d: the index queue is shared by all stores of a JVM. An entry of a store must be read with the delegator of
 * that store, not with the default delegator (the product is not in the base database: "product" is null).
 */
public class EntityIndexerStoreEntriesTest {

    private static EntityIndexer.Entry entry(String delegatorName) {
        GenericPK pk = mock(GenericPK.class);
        if (delegatorName != null) {
            Delegator d = mock(Delegator.class);
            when(d.getDelegatorName()).thenReturn(delegatorName);
            when(pk.getDelegator()).thenReturn(d);
        }
        return new EntityIndexer.Entry(pk, "P1", null, 1L, Collections.emptySet(), null);
    }

    @Test
    void entriesOfAStoreAreSplitFromTheOwnEntries() {
        EntityIndexer.Entry own = entry("default");
        EntityIndexer.Entry storeA = entry("default#t1");
        EntityIndexer.Entry storeA2 = entry("default#t1");
        EntityIndexer.Entry storeB = entry("default#t2");
        List<EntityIndexer.Entry> entries = new ArrayList<>(List.of(own, storeA, storeB, storeA2));

        Map<String, List<EntityIndexer.Entry>> others = EntityIndexer.extractOtherDelegatorEntries(entries, "default");

        assertEquals(List.of(own), entries);
        assertEquals(2, others.size());
        assertEquals(List.of(storeA, storeA2), others.get("default#t1"));
        assertEquals(List.of(storeB), others.get("default#t2"));
    }

    @Test
    void entriesWithoutDelegatorStayInTheList() {
        EntityIndexer.Entry noDelegator = entry(null);
        List<EntityIndexer.Entry> entries = new ArrayList<>(List.of(noDelegator));
        assertTrue(EntityIndexer.extractOtherDelegatorEntries(entries, "default").isEmpty());
        assertEquals(1, entries.size());
    }
}
