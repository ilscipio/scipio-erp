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
<#include "component://shop/webapp/shop/customer/customercommon.ftl">

<@section title=((survey.surveyName)!)>
    <#-- Render the survey -->
    <#if surveyWrapper?has_content>
        <#-- SCIPIO: 2019-03-06: Now supports surveyAction and surveyMarkup error/missing fallback
            NOTE: surveyWrapper.render(context) may return null/void/missing/empty upon error or upon empty output -->
        <form method="post" enctype="multipart/form-data" action="<@pageUrl uri=surveyAction!'profilesurvey' escapeAs='html'/>">
          <input type="hidden" name="surveyAction" value="${surveyAction!'profilesurvey'}"/><#-- SCIPIO: need for error case -->
          <#assign surveyMarkup = surveyWrapper.render(context)!>
          <#if surveyMarkup?has_content>
            ${surveyMarkup}
          <#else>
            <@commonMsg type="result">${uiLabelMap.OrderNothingToDoHere}</@commonMsg>
            <a href="<@pageUrl uri='main'/>" class="${styles.link_nav} ${styles.action_view}">${uiLabelMap.CommonHome}</a>
          </#if>
        </form>
    <#else>
        <@commonMsg type="result">${uiLabelMap.OrderNothingToDoHere}</@commonMsg>
        <a href="<@pageUrl uri='main'/>" class="${styles.link_nav} ${styles.action_view}">${uiLabelMap.CommonHome}</a>
    </#if>
</@section>
