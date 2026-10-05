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
package org.ofbiz.entity.util;

/**
 * Contains a number of variables used to select certain advanced options for storing GenericEntities.
 */
@SuppressWarnings("serial")
public class EntityStoreOptions implements java.io.Serializable {

    /** Option for creating missing referenced values as dummy (pk-only) entries */
    protected boolean createDummyFks = false;

    /**
     * Default constructor. Defaults are as follows: createDummyFks = false
     */
    public EntityStoreOptions() {
    }

    /**
     * Optional constructor with options to specify.
     *
     * @param createDummyFks
     */
    public EntityStoreOptions(boolean createDummyFks) {
        this.createDummyFks = createDummyFks;
    }

    /**
     * If true, missing entries in FK referenced entities will be created while storing the given GenericValues.
     *
     * @return boolean
     */
    public boolean isCreateDummyFks() {
        return createDummyFks;
    }

    /**
     * If true, missing entries in FK referenced entities will be created while storing the given GenericValues.
     *
     * @param createDummyFks
     */
    public void setCreateDummyFks(boolean createDummyFks) {
        this.createDummyFks = createDummyFks;
    }
}
