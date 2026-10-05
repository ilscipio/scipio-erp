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
package org.ofbiz.entity;

/**
 * SCIPIO: Now thrown instead of IllegalArgumentException when {@link GenericEntity#get(Object)}
 * is called using a field name that does not exist on the entity.
 * <p>
 * Note this extends IllegalArgumentException, NOT GenericEntityException.
 * <p>
 * Added 2018-09-29.
 */
@SuppressWarnings("serial")
public class EntityFieldNotFoundException extends IllegalArgumentException {
    public EntityFieldNotFoundException() {
        super();
    }

    public EntityFieldNotFoundException(String message, Throwable cause) {
        super(message, cause);
    }

    public EntityFieldNotFoundException(String s) {
        super(s);
    }

    public EntityFieldNotFoundException(Throwable cause) {
        super(cause);
    }
}