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
<#-- SCIPIO: 4.0.0: store legal page (compliance component). Body HTML is sanitized by LegalDocumentWorker. -->
<#if legalDoc??>
<div class="scp-legal">
  <nav class="scp-legal-nav" aria-label="${uiLabelMap.ComplianceOnThisPage}">
    <ul>
    <#list legalDocNav as n>
      <li<#if n.slug == legalDoc.slug> class="is-active"</#if>><a href="<@ofbizUrl>legal?doc=${n.slug}</@ofbizUrl>"<#if n.slug == legalDoc.slug> aria-current="page"</#if>>${n.label}</a></li>
    </#list>
    </ul>
  </nav>
  <article class="scp-legal-doc">
    <#if legalDoc.isTemplate && (legalDocCanManage!false)>
      <@alert type="warning">${uiLabelMap.ComplianceTemplateNotice}</@alert>
    </#if>
    <h1>${legalDoc.title}</h1>
    <#if legalDoc.versionNum?? && legalDoc.publishedDate??>
      <p class="scp-legal-meta">${uiLabelMap.ComplianceVersion} ${legalDoc.versionNum} &middot; ${uiLabelMap.ComplianceInEffectSince} ${legalDoc.publishedDate?date?string.long}</p>
    </#if>
    <#if legalDoc.changeNote?has_content><p class="scp-legal-change">${legalDoc.changeNote}</p></#if>
    ${rawString(legalDoc.bodyHtml)}
  </article>
</div>
<#else>
  <@commonMsg type="error">${uiLabelMap.ComplianceDocumentNotFound}</@commonMsg>
</#if>
