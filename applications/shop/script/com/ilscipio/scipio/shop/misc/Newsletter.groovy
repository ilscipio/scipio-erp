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
import org.ofbiz.base.util.GroovyUtil
import org.ofbiz.base.util.UtilValidate
import org.ofbiz.entity.util.EntityQuery
import org.ofbiz.product.store.ProductStoreWorker

/**
 * SCIPIO NOTE: Currently this only supports maileon, but it could be extended to support other newsletter services.
 */

isMaileonComponentPresent = org.ofbiz.base.component.ComponentConfig.isComponentPresent("maileon")
if (UtilValidate.isEmpty(isMaileonComponentPresent)) {
    isMaileonComponentPresent = false
}

productStoreId = ProductStoreWorker.getProductStoreId(request)
productStoreMaileon = null
if (isMaileonComponentPresent && productStoreId) {
    productStoreMaileon = EntityQuery.use(delegator).from("ProductStoreMaileon").where("productStoreId", productStoreId).queryOne()
    GroovyUtil.runScriptAtLocation("component://maileon/script/MaileonCustomFields.groovy", null, context)
}
context.productStoreMaileon = productStoreMaileon
context.isMaileonComponentPresent = isMaileonComponentPresent
context.maileonRenderEmail = false
if (productStoreMaileon) {
    context.maileonRenderEmail = true
}