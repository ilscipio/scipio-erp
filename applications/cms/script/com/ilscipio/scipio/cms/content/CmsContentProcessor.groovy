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
/**
 * Scipio CMS Content processor placeholder - can be used to put Variable content into
 * the context of the CMS Editor.
 * <p>
 * NOTE: 2017: the CMS-specific processor script is no longer of major importance in Scipio,
 * because Scipio supports custom system-wide and webapp-specific global scripts.
 * It is preferable to reuse those mechanisms, unless the script is truly CMS-specific.
 * <p>
 * WARNING: This script may not be running under any database transaction.
 * If you use this script and must query the entity engine, it is recommended
 * to query with the entity cache enabled only - any non-cached entity lookups
 * may impose an extra performance cost.
 */

