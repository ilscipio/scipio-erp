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
 * Generic entity data integrity exception, representing code-base data integrity checks.
 *
 * <p>NOTE: This differs from {@link GenericModelException} in that this one represents run-/store-time data errors,
 * whereas model errors refer to errors in xml definitions and such.</p>
 *
 * <p>SCIPIO: 3.0.0: Added to represent server-side data integrity checks, similar to {@link EntityConstraintException}.</p>
 */
public class EntityDataIntegrityException extends GenericEntityException {

    public EntityDataIntegrityException() {
    }

    public EntityDataIntegrityException(Throwable nested) {
        super(nested);
    }

    public EntityDataIntegrityException(String str) {
        super(str);
    }

    public EntityDataIntegrityException(String str, Throwable nested) {
        super(str, nested);
    }

}
