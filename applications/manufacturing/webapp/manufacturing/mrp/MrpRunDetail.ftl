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

<@section title=uiLabelMap.PageTitleMrpRunDetail>
  <#if mrpRun?has_content>
    <@row>
      <@cell columns=4>
        <@field type="display" label=uiLabelMap.CommonId>${mrpRun.mrpId!}</@field>
        <@field type="display" label=uiLabelMap.ManufacturingMrpName>${mrpRun.mrpName!}</@field>
        <@field type="display" label=uiLabelMap.ProductFacility>${mrpRun.facilityId!}</@field>
        <@field type="display" label=uiLabelMap.ProductFacilityGroup>${mrpRun.facilityGroupId!}</@field>
      </@cell>
      <@cell columns=4>
        <@field type="display" label=uiLabelMap.CommonStatus>${mrpRun.statusId!}</@field>
        <@field type="display" label=uiLabelMap.CommonStartDate>${mrpRun.startDate!}</@field>
        <@field type="display" label=uiLabelMap.CommonEndDate>${mrpRun.finishDate!}</@field>
        <@field type="display" label=uiLabelMap.ManufacturingRunByUser>${mrpRun.runByUserLoginId!}</@field>
      </@cell>
      <@cell columns=4>
        <@field type="display" label=uiLabelMap.ManufacturingEventCount><a href="<@pageUrl>FindInventoryEventPlan?mrpId=${mrpRun.mrpId!}</@pageUrl>" class="${styles.link_nav_info_id!}">${mrpRun.eventCount!}</a></@field>
        <@field type="display" label=uiLabelMap.ManufacturingProposedProductionRuns><a href="<@pageUrl>MrpProposals?facilityId=${mrpRun.facilityId!}</@pageUrl>" class="${styles.link_nav_info_id!}">${mrpRun.proposedProductionRuns!}</a></@field>
        <@field type="display" label=uiLabelMap.ManufacturingProposedPurchases><a href="<@pageUrl>MrpProposals?facilityId=${mrpRun.facilityId!}</@pageUrl>" class="${styles.link_nav_info_id!}">${mrpRun.proposedPurchases!}</a></@field>
        <@field type="display" label=uiLabelMap.ManufacturingErrorCount>${mrpRun.errorCount!}</@field>
      </@cell>
    </@row>
    <#if (mrpRun.message)?has_content>
      <@row>
        <@cell>
          <@field type="display" label=uiLabelMap.CommonMessage>${mrpRun.message!}</@field>
        </@cell>
      </@row>
    </#if>
    <@row>
      <@cell>
        <a href="<@pageUrl>MrpProposals?facilityId=${mrpRun.facilityId!}</@pageUrl>" class="${styles.link_nav!} ${styles.action_view!}">${uiLabelMap.PageTitleMrpProposals}</a>
        <a href="<@pageUrl>FindInventoryEventPlan</@pageUrl>" class="${styles.link_nav!} ${styles.action_view!}">${uiLabelMap.PageTitleFindInventoryEventPlan}</a>
      </@cell>
    </@row>
  <#else>
    <@commonMsg type="result-norecord"/>
  </#if>
</@section>
