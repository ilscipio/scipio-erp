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
 * SCIPIO: 4.0.0: compliance overview: per store the detected third-party services and the state of each legal text.
 */
import org.ofbiz.entity.util.EntityQuery
import com.ilscipio.scipio.compliance.LegalDocumentWorker
import com.ilscipio.scipio.compliance.ThirdPartyServiceRegistry
import com.ilscipio.scipio.compliance.ComplianceChecklist
import com.ilscipio.scipio.compliance.PackagingReportWorker
import org.ofbiz.base.util.UtilDateTime

def stores = EntityQuery.use(delegator).from("ProductStore").orderBy("storeName").queryList()
def selectedStoreId = parameters.productStoreId ?: (stores.find { it.productStoreId == "ScipioShop" }?.productStoreId ?: stores[0]?.productStoreId)
context.complianceStores = stores
context.selectedStoreId = selectedStoreId
if (!selectedStoreId) return

def profile = LegalDocumentWorker.getProfile(delegator, selectedStoreId)
def services = ThirdPartyServiceRegistry.getServices(delegator, selectedStoreId)
def currentHash = ThirdPartyServiceRegistry.getRegistryHash(services)
def docLocale = parameters.docLocale ?: "en"
def loc = new Locale(docLocale)

def docs = []
LegalDocumentWorker.getDocTypes(delegator).each { type ->
    def published = LegalDocumentWorker.getPublished(delegator, selectedStoreId, type.enumId, loc)
    def usesServices = type.enumId in ["LEGDOC_PRIVACY", "LEGDOC_COOKIES", "LEGDOC_CA_NOTICE"]
    docs << [docTypeId: type.enumId, slug: type.enumCode, description: type.get("description", locale),
             published: published,
             hasTemplate: LegalDocumentWorker.getTemplateText(type.enumCode, loc) != null,
             outdated: published != null && usesServices && published.registryHash != currentHash]
}
context.complianceProfile = profile
context.complianceServices = services
context.complianceRegistryHash = currentHash
context.complianceDocs = docs
context.complianceDocLocale = docLocale
context.complianceJurisdictions = LegalDocumentWorker.getJurisdictions(profile)
context.complianceChecklist = ComplianceChecklist.run(delegator, selectedStoreId, loc)
def pkgThru = UtilDateTime.nowTimestamp()
def pkgFrom = UtilDateTime.adjustTimestamp(pkgThru, Calendar.DAY_OF_YEAR, -90)
context.compliancePackaging = PackagingReportWorker.placedOnMarket(delegator, selectedStoreId, pkgFrom, pkgThru)
context.compliancePackagingFrom = pkgFrom
context.complianceOpenRequests = EntityQuery.use(delegator).from("PrivacyRequest").where("productStoreId", selectedStoreId).orderBy("dueDate").queryList().findAll { it.statusId in ["PRS_UNVERIFIED", "PRS_RECEIVED", "PRS_IN_PROGRESS"] }
