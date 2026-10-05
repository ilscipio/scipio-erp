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
<#-- SCIPIO: 4.0.0: confirmation of receipt of a withdrawal (compliance component). This e-mail is the durable record. -->
<h1>${uiLabelMap.ComplianceWithdrawReceivedTitle}</h1>
<p>${rawString(uiLabelMap.ComplianceWithdrawReceivedAt)?replace("{0}", receivedDate?string("yyyy-MM-dd HH:mm:ss z"))}</p>
<table cellpadding="4" cellspacing="0" border="0">
  <tr><td><strong>${uiLabelMap.ComplianceOrder}</strong></td><td>${orderId}<#if orderDate??> (${orderDate?string("yyyy-MM-dd")})</#if></td></tr>
  <tr><td><strong>${uiLabelMap.ComplianceReference}</strong></td><td>${withdrawalRef!}</td></tr>
  <#if customerName?has_content><tr><td><strong>${uiLabelMap.ComplianceName}</strong></td><td>${customerName}</td></tr></#if>
</table>
<p><strong>${uiLabelMap.ComplianceWithdrawStatement}</strong></p>
<ul>
  <#list withdrawnItems![] as it><li>${it.description!it.productId} &times; ${it.quantity}</li></#list>
  <#list pendingItems![] as it><li>${it.description!it.productId} &times; ${it.quantity}</li></#list>
</ul>
<p>${uiLabelMap.ComplianceWithdrawNextSteps}</p>
<p>${storeName!}</p>
