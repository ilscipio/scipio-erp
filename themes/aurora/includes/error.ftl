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
<#assign errorMessage = requestAttributes._ERROR_MESSAGE_!>
<#if requestAttributes.errorMessageList?has_content><#assign errorMessageList=requestAttributes.errorMessageList></#if>

<#-- SCIPIO: NOTE: 2018-02-26: The error message variables below must now be HTML-escaped by this ftl file.
    They will no longer be hard-escaped by ControlServlet - the mechanism here is more thorough and does not interfere with javascript. -->

<#-- Aurora ("Ledger"): the error is a sheet with a red margin; it says what went wrong and
     offers the way back. -->
<div class="au-error-sheet" role="alert">
    <h1 class="au-error-title">${getLabel('PageTitleError')!}</h1>
    <#if errorMessage?has_content || errorMessageList?has_content>
        <p>${getLabel('CommonFollowingErrorsOccurred')}</p>
        <ol>
            <#if errorMessage?has_content>
                <li>${escapeEventMsg(errorMessage, 'htmlmarkup')}</li>
            </#if>
            <#if errorMessageList?has_content>
                <#list errorMessageList as errorMsg>
                    <li>${escapeEventMsg(errorMsg, 'htmlmarkup')}</li>
                </#list>
            </#if>
        </ol>
    <#else>
        <p>${getLabel('CommonErrorOccurredContactSupport')}</p>
    </#if>
    <p class="au-error-actions">
        <a class="button" href="javascript:history.back()">${getLabel('CommonGoBack')}</a>
        <a class="button is-primary" href="<@pageUrl>main</@pageUrl>">${getLabel('CommonMain')}</a>
    </p>
</div>
