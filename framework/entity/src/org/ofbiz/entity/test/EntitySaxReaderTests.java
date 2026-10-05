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
package org.ofbiz.entity.test;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.ofbiz.base.util.Debug;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.model.ModelEntity;
import org.ofbiz.entity.util.EntitySaxReader;

public class EntitySaxReaderTests {
    private boolean logVerboseOn;

    @Before
    public void initialize() {
        logVerboseOn = Debug.isOn(Debug.VERBOSE); // save the current setting (to be restored after the tests)
        Debug.set(Debug.VERBOSE, false); // disable verbose logging: this is necessary to avoid a test error in the "parse" unit test
    }

    @After
    public void restore() {
        Debug.set(Debug.VERBOSE, logVerboseOn); // restore the verbose log setting
    }

    @Test
    public void constructorWithDefaultTimeout() {
        Delegator delegator = mock(Delegator.class);
        EntitySaxReader esr = new EntitySaxReader(delegator); // create a reader with default tx timeout
        verify(delegator).cloneDelegator();
        verifyNoMoreInteractions(delegator);
        assertEquals(EntitySaxReader.DEFAULT_TX_TIMEOUT, esr.getTransactionTimeout());
    }

    @Test
    public void constructorWithTimeout() {
        Delegator delegator = mock(Delegator.class);
        EntitySaxReader esr = new EntitySaxReader(delegator, 14400); // create a reader with a non default tx timeout
        verify(delegator).cloneDelegator();
        verifyNoMoreInteractions(delegator);
        assertEquals(14400, esr.getTransactionTimeout());
    }

    @Test
    public void parse() throws Exception {
        Delegator delegator = mock(Delegator.class);
        Delegator clonedDelegator = mock(Delegator.class);
        GenericValue genericValue = mock(GenericValue.class);
        ModelEntity modelEntity = mock(ModelEntity.class);
        when(delegator.cloneDelegator()).thenReturn(clonedDelegator);
        when(clonedDelegator.makeValue("EntityName")).thenReturn(genericValue);
        when(genericValue.getModelEntity()).thenReturn(modelEntity);
        when(genericValue.containsPrimaryKey()).thenReturn(true);
        when(modelEntity.isField("fieldName")).thenReturn(true);

        EntitySaxReader esr = new EntitySaxReader(delegator);
        String input = "<entity-engine-xml><EntityName fieldName=\"field value\"/></entity-engine-xml>";
        long recordsProcessed = esr.parse(input);
        verify(clonedDelegator).makeValue("EntityName");
        assertEquals(1, recordsProcessed);
    }
}
