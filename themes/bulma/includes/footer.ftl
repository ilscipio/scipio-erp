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
    </div>
</div>
<@scripts output=true> <#-- ensure @script elems here will always output -->
    <#-- New in scipio; priority footer javascripts (before screen footer javascripts) -->
    <#if layoutSettings.VT_FTPR_JAVASCRIPT?has_content>
        <#--layoutSettings.javaScripts is a list of java scripts. -->
        <#-- use a Set to make sure each javascript is declared only once, but iterate the list to maintain the correct order -->
        <#assign javaScriptsSet = toSet(layoutSettings.VT_FTPR_JAVASCRIPT)/>
        <#list layoutSettings.VT_FTPR_JAVASCRIPT as javaScript>
            <#if javaScriptsSet.contains(javaScript)>
                <#assign nothing = javaScriptsSet.remove(javaScript)/>
                <@script src=makeOfbizContentUrl(javaScript) />
            </#if>
        </#list>
    </#if>

    <#-- New in scipio; for app scripts that aren't (exclusively) styling but must go at end of page -->
    <#if layoutSettings.javaScriptsFooter?has_content>
        <#assign javaScriptsSet = toSet(layoutSettings.javaScriptsFooter)/>
        <#list layoutSettings.javaScriptsFooter as javaScript>
            <#if javaScriptsSet.contains(javaScript)>
                <#assign nothing = javaScriptsSet.remove(javaScript)/>
                <@script src=makeOfbizContentUrl(javaScript) />
            </#if>
        </#list>
    </#if>

    <#-- For theme styling-related scripts -->
    <#if layoutSettings.VT_FTR_JAVASCRIPT?has_content>
        <#--layoutSettings.javaScripts is a list of java scripts. -->
        <#-- use a Set to make sure each javascript is declared only once, but iterate the list to maintain the correct order -->
        <#assign javaScriptsSet = toSet(layoutSettings.VT_FTR_JAVASCRIPT)/>
        <#list layoutSettings.VT_FTR_JAVASCRIPT as javaScript>
            <#if javaScriptsSet.contains(javaScript)>
                <#assign nothing = javaScriptsSet.remove(javaScript)/>
                <@script src=makeOfbizContentUrl(javaScript) />
            </#if>
        </#list>
    </#if>

    <#-- New in scipio; always-bottom guaranteed-last javascripts -->
    <#assign scpScriptBuffer = getRequestVar("scipioScriptBuffer")!"">
    <#if scpScriptBuffer?has_content><#-- output of merged inline scripts -->
        <@script merge=false>${scpScriptBuffer}</@script>
    </#if>
    <#if layoutSettings.VT_BTM_JAVASCRIPT?has_content>
        <#--layoutSettings.javaScripts is a list of java scripts. -->
        <#-- use a Set to make sure each javascript is declared only once, but iterate the list to maintain the correct order -->
        <#assign javaScriptsSet = toSet(layoutSettings.VT_BTM_JAVASCRIPT)/>
        <#list layoutSettings.VT_BTM_JAVASCRIPT as javaScript>
            <#if javaScriptsSet.contains(javaScript)>
                <#assign nothing = javaScriptsSet.remove(javaScript)/>
                <@script src=makeOfbizContentUrl(javaScript) />
            </#if>
        </#list>
    </#if>
</@scripts>
</body>
</html>
