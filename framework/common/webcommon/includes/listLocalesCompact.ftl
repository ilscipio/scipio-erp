<#--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
-->
<@section title=uiLabelMap.CommonLanguageTitle>

<#if !setLocalesTarget?has_content>
  <#assign setLocalesTarget = "setSessionLocale">
</#if>

<#assign setLocalesTargetViewStr = "">
<#if setLocalesTargetView?has_content>
  <#assign setLocalesTargetViewStr = "/" + setLocalesTargetView>
</#if>

<form method="get" action="<@ofbizUrl>${setLocalesTarget}${setLocalesTargetViewStr}</@ofbizUrl>">
<@fields type="default-nolabelarea">
  <@field type="select" name="newLocale">
    <#assign altRow = true>
    <#assign availableLocales = Static["org.ofbiz.base.util.UtilMisc"].availableLocales()/>
    
    <#list availableLocales as availableLocale>
        <#assign altRow = !altRow>
        <#assign langAttr = availableLocale.toString()?replace("_", "-")>
        <#assign langDir = "ltr">
        <#if "ar.iw"?contains(langAttr?substring(0, 2))>
            <#assign langDir = "rtl">
        </#if>
        <option value="${availableLocale.toString()}" lang="${langAttr}" dir="${langDir}"<#if (locale?has_content) && (locale.getLanguage() == availableLocale.getLanguage())> selected="selected"</#if>>${availableLocale.getDisplayName(availableLocale)} &nbsp;&nbsp;&nbsp;-&nbsp;&nbsp;&nbsp; [${availableLocale.toString()}]</option>
    </#list>
  </@field>
  <@field type="submit" text=uiLabelMap.CommonSubmit/>
</@fields>
</form>
</@section>
