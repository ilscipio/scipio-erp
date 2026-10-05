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
 * SCIPIO: gets the last viewed product and puts it into context/parameters.
 * By default this does NOT set in globalContext and MUST NOT unless flag
 * asking for it is passed.
 */

useGlobal = context.getProductLastProduct?.global;
if (useGlobal == null) {
    useGlobal = false;
}
 
productLastProductId = session.getAttribute("productLastProductId"); 
context.productLastProductId = productLastProductId;

if (!parameters.productId && !context.productId && !globalContext.productId) {
    productId = productLastProductId;
    parameters.productId = productId;
    if (useGlobal) {
        globalContext.productId = productId;
    } else {
        context.productId = productId;
    }
}
