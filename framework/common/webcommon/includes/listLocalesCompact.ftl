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
<@section title=uiLabelMap.CommonLanguageTitle>

<#if !setLocalesTarget?has_content>
  <#assign setLocalesTarget = "setSessionLocale">
</#if>

<#assign setLocalesTargetViewStr = "">
<#if setLocalesTargetView?has_content>
  <#assign setLocalesTargetViewStr = "/" + setLocalesTargetView>
</#if>

<form method="get" action="<@pageUrl>${setLocalesTarget}${setLocalesTargetViewStr}</@pageUrl>">
<@fields type="default-nolabelarea">
  <@field type="select" name="newLocale">
    <#assign altRow = true>
    <#assign availableLocales = availableLocales!UtilMisc.availableLocales()/>
    
    <#list availableLocales as availableLocale>
        <#assign altRow = !altRow>
        <#assign langAttr = availableLocale.toString()?replace("_", "-")>
        <#assign langDir = "ltr">
        <#if "ar.iw"?contains(langAttr?substring(0, 2))>
            <#assign langDir = "rtl">
        </#if>
        <option value="${availableLocale.toString()}" lang="${langAttr}" dir="${langDir}"<#if (locale?has_content) && (locale.getLanguage() == availableLocale.getLanguage())> selected="selected"</#if>>${availableLocale.getDisplayName(availableLocale)} &nbsp;&nbsp;&nbsp;-&nbsp;&nbsp;&nbsp; [${langAttr}]</option>
    </#list>
  </@field>
  <@field type="submit" text=uiLabelMap.CommonSubmit/>
</@fields>
</form>
</@section>
