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
package com.ilscipio.scipio.cms.data;

/**
 * Implemented by the "major" entities, or in other words those that can
 * correspond to high-level objects or abstractions, like Pages or Templates.
 * <p>
 * NOTE: For technical reasons this also enforces the notion the major entities
 * must have a single simple physical representing entity.
 * They are assumed to extend CmsDataObject.
 * <p>
 * DEV NOTE: All data object classes implementing this should have their entity name
 * added to {@link CmsEntityInfo#majorCmsEntityNames}.
 */
public interface CmsMajorObject extends CmsEntityReadable, CmsEntityVisit.CmsEntityVisitee {
}
