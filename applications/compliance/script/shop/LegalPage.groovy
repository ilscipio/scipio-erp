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
/*
 * SCIPIO: 4.0.0: shop action for legal?doc=<slug> (compliance component).
 * Sets legalDoc (title, bodyHtml, versionNum, publishedDate, isTemplate) and legalDocNav.
 */
import org.ofbiz.product.store.ProductStoreWorker
import com.ilscipio.scipio.compliance.LegalDocumentWorker

def productStoreId = ProductStoreWorker.getProductStoreId(request)
def slug = parameters.doc ?: "privacy"

def legalDoc = LegalDocumentWorker.getDisplayDocument(delegator, productStoreId, slug, locale)
context.legalDoc = legalDoc
context.legalDocNav = LegalDocumentWorker.getNavigation(delegator, productStoreId, locale)
context.legalDocCanManage = security.hasPermission("COMPLIANCE_VIEW", request.getSession())
if (legalDoc) {
    context.title = legalDoc.title
} else {
    response.setStatus(404)
}
