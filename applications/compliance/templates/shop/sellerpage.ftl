<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
-->
<#-- SCIPIO: 4.0.0: marketplace seller page with trader data (DSA Art. 30-31, CRD Art. 6a, INFORM Act). -->
<#assign storeId = Static["org.ofbiz.product.store.ProductStoreWorker"].getProductStoreId(request)!>
<#assign seller = Static["com.ilscipio.scipio.compliance.MarketplaceWorker"].getSeller(delegator, storeId, parameters.sellerId!)!>
<#assign profile = Static["com.ilscipio.scipio.compliance.LegalDocumentWorker"].getProfile(delegator, storeId)!>
<#if seller?has_content>
<div class="scp-seller">
  <div class="scp-seller-head">
    <span class="scp-seller-mark" aria-hidden="true">${seller.letter}</span>
    <div>
      <p class="scp-eyebrow">${uiLabelMap.ComplianceSeller}<#if seller.category?has_content> &middot; ${seller.category}</#if></p>
      <h1>${seller.displayName}</h1>
      <p class="scp-seller-badges">
        <#if seller.sellerType == "SELLER_BUSINESS"><span class="scp-badge">${uiLabelMap.ComplianceBusinessSeller}</span><#else><span class="scp-badge scp-badge--warn">${uiLabelMap.CompliancePrivateSeller}</span></#if>
        <#if seller.verified><span class="scp-badge">${uiLabelMap.ComplianceVerified}</span></#if>
        <#if seller.joinedDate??><span>${uiLabelMap.ComplianceJoined} ${seller.joinedDate?date?string.medium}</span></#if>
      </p>
    </div>
  </div>
  <#if seller.shortBio?has_content><p class="scp-lead">${seller.shortBio}</p></#if>

  <section class="scp-card" aria-labelledby="scp-trader-title">
    <h2 id="scp-trader-title">${uiLabelMap.ComplianceTraderInformation}</h2>
    <dl class="scp-safety-list">
      <dt>${uiLabelMap.ComplianceLegalName}</dt><dd>${(seller.trader.name)!seller.displayName}</dd>
      <#if (seller.trader.address)??><dt>${uiLabelMap.ComplianceAddress}</dt><dd>${seller.trader.address}</dd></#if>
      <#if (seller.trader.email)??><dt>${uiLabelMap.ComplianceContact}</dt><dd><a href="mailto:${seller.trader.email}">${seller.trader.email}</a></dd></#if>
      <#if seller.tradeRegister?has_content><dt>${uiLabelMap.ComplianceTradeRegister}</dt><dd>${seller.tradeRegister}</dd></#if>
      <#if seller.vatId?has_content><dt>${uiLabelMap.ComplianceVatId}</dt><dd>${seller.vatId}</dd></#if>
      <dt>${uiLabelMap.ComplianceSelfCertification}</dt><dd><#if seller.selfCertified>${uiLabelMap.ComplianceSelfCertified}<#else>${uiLabelMap.ComplianceNotGiven}</#if></dd>
    </dl>
    <p><a href="mailto:${(profile.contactEmail)!""}?subject=${("Report seller " + seller.partyId)?url}">${uiLabelMap.ComplianceReportSeller}</a> &middot; <a href="<@ofbizUrl>legal?doc=sellers</@ofbizUrl>">${uiLabelMap.ComplianceHowWeCheckSellers}</a></p>
  </section>

  <#if seller.productIds?has_content>
  <section aria-labelledby="scp-seller-products">
    <h2 id="scp-seller-products">${uiLabelMap.ComplianceProductsOfSeller}</h2>
    <ul class="scp-seller-products">
      <#list seller.productIds as pid>
        <#assign sp = delegator.findOne("Product", {"productId": pid}, true)!>
        <#if sp?has_content><li><a href="<@ofbizUrl>product?product_id=${pid}</@ofbizUrl>">${sp.productName!pid}</a></li></#if>
      </#list>
    </ul>
  </section>
  </#if>
</div>
<#else>
  <@commonMsg type="error">${uiLabelMap.ComplianceSellerNotFound}</@commonMsg>
</#if>
