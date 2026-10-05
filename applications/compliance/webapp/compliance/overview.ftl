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
<#-- SCIPIO: 4.0.0: compliance overview (back office) -->
<@section>
  <form method="get" action="<@ofbizUrl>main</@ofbizUrl>">
    <@field type="select" name="productStoreId" label="Store" inline=true>
      <#list complianceStores as s>
        <option value="${s.productStoreId}"<#if s.productStoreId == (selectedStoreId!"")> selected="selected"</#if>>${s.storeName!s.productStoreId} [${s.productStoreId}]</option>
      </#list>
    </@field>
    <@field type="select" name="docLocale" label="Language" inline=true>
      <#list ["en", "de"] as l><option value="${l}"<#if l == complianceDocLocale!"en"> selected="selected"</#if>>${l}</option></#list>
    </@field>
    <@field type="submit" text="Show"/>
  </form>
</@section>

<#if selectedStoreId?has_content>
<@section title="Checklist">
  <@table type="data-list">
    <@thead><@tr><@th>Status</@th><@th>Check</@th><@th>Result</@th><@th>How to fix</@th></@tr></@thead>
    <#list complianceChecklist as c>
      <@tr>
        <@td><strong class="scp-status scp-status--${c.status?lower_case}">${c.status}</strong></@td>
        <@td>${c.title}</@td><@td>${c.detail}</@td><@td>${c.fix}</@td>
      </@tr>
    </#list>
  </@table>
</@section>

<@section title="Open privacy requests">
  <#if complianceOpenRequests?has_content>
  <@table type="data-list">
    <@thead><@tr><@th>Id</@th><@th>Type</@th><@th>Status</@th><@th>E-mail / party</@th><@th>Due</@th></@tr></@thead>
    <#list complianceOpenRequests as r>
      <@tr><@td>${r.privacyRequestId}</@td><@td>${r.requestTypeId}</@td><@td>${r.statusId}</@td><@td>${r.emailAddress!r.partyId!""}</@td><@td>${(r.dueDate?string("yyyy-MM-dd"))!""}</@td></@tr>
    </#list>
  </@table>
  <#else><p>None.</p></#if>
</@section>

<@section title="Packaging placed on the market, last 90 days (kg)">
  <#if compliancePackaging?has_content>
  <@table type="data-list">
    <@thead><@tr><@th>Country</@th><@th>Material</@th><@th>kg</@th></@tr></@thead>
    <#list compliancePackaging?keys as country><#list compliancePackaging[country]?keys as mat>
      <@tr><@td>${country}</@td><@td>${mat}</@td><@td>${compliancePackaging[country][mat]}</@td></@tr>
    </#list></#list>
  </@table>
  <#else><p>No shipped packaging with material data in this period.</p></#if>
</@section>

<@section title="Profile">
  <#if complianceProfile??>
    <p>Jurisdictions: <strong>${complianceJurisdictions?join(", ")}</strong> &middot; Legal name: ${complianceProfile.legalName!"-"} &middot;
      Withdrawal: ${complianceProfile.withdrawalDays!"-"} days &middot; Legal guarantee: ${complianceProfile.legalGuaranteeYears!"-"} years &middot;
      Marketplace: ${complianceProfile.marketplaceMode!"N"}</p>
  <#else>
    <@alert type="warning">This store has no compliance profile yet. The shop uses defaults (EU and US rules, 14 days withdrawal, 2 years guarantee).</@alert>
  </#if>
</@section>

<@section title="Legal texts (${complianceDocLocale})">
  <@table type="data-list">
    <@thead><@tr><@th>Text</@th><@th>State</@th><@th>Version</@th><@th>Published</@th><@th></@th></@tr></@thead>
    <#list complianceDocs as d>
      <@tr>
        <@td><a href="<@ofbizInterWebappUrl>/shop/control/legal?doc=${d.slug}</@ofbizInterWebappUrl>" target="_blank">${d.description!d.docTypeId}</a></@td>
        <@td>
          <#if d.published??>Published<#if d.outdated> &middot; <strong>older than the service list</strong></#if>
          <#elseif d.hasTemplate>Template (not published)<#else>Missing</#if>
        </@td>
        <@td>${(d.published.versionNum)!""}</@td>
        <@td>${(d.published.publishedDate?string("yyyy-MM-dd HH:mm"))!""}</@td>
        <@td>
          <#if d.hasTemplate>
          <form method="post" action="<@ofbizUrl>publishLegalDocument</@ofbizUrl>">
            <input type="hidden" name="productStoreId" value="${selectedStoreId}"/>
            <input type="hidden" name="docTypeId" value="${d.docTypeId}"/>
            <input type="hidden" name="localeString" value="${complianceDocLocale}"/>
            <input type="hidden" name="changeNote" value="Published from the shipped template"/>
            <@field type="submit" text=(d.published??)?then("Publish template again", "Publish template") class="+${styles.link_run_sys!} ${styles.action_add!}"/>
          </form>
          </#if>
        </@td>
      </@tr>
    </#list>
  </@table>
</@section>

<@section title="Third-party services (hash ${complianceRegistryHash})">
  <@table type="data-list">
    <@thead><@tr><@th>Service</@th><@th>Provider</@th><@th>Category</@th><@th>Cookies</@th><@th>Script domains</@th><@th>Source</@th></@tr></@thead>
    <#list complianceServices as s>
      <@tr>
        <@td>${s.name}</@td><@td>${s.provider}</@td><@td>${s.category}</@td><@td>${s.cookies}</@td>
        <@td>${s.scriptDomains?join(", ")}</@td><@td>${s.source}</@td>
      </@tr>
    </#list>
  </@table>
</@section>
</#if>
