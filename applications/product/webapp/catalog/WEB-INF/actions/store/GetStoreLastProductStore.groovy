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
 * SCIPIO: gets the last viewed product store and puts it into context/parameters.
 * WARN: this might not be appropriate for all store screens. this version does NOT
 * set globalContext, only context, for safety.
 */

args = context.getStoreLastProductStore ?: [:];
 
useGlobal = args.global;
if (useGlobal == null) {
    useGlobal = false;
}
  
storeLastProductStoreId = session.getAttribute("storeLastProductStoreId");
context.storeLastProductStoreId = storeLastProductStoreId;
 
if (!parameters.productStoreId && !context.productStoreId && !globalContext.productStoreId) {
    productStoreId = args.overrideProductStoreId ?: session.getAttribute("storeLastProductStoreId") ?: args.defaultProductStoreId;

    parameters.productStoreId = productStoreId;
    if (useGlobal) {
        globalContext.productStoreId = productStoreId;
    } else {
        context.productStoreId = productStoreId;
    }
}
