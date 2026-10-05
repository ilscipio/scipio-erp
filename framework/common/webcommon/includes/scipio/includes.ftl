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
<#compress>
<#--
* 
* Scipio main include script.
*
* Ensures include order, #compress, keeps other templates clean and (TODO) per-context include loading.
* Automatically included at all times.
*
* DEV NOTES: 
* * Should try to avoid #compress in ALL OTHER macros and templates in general because sometimes behaves oddly in Ofbiz.
*   The one in this file should (hopefully) be OK because this include produces no markup.
* * This essentially takes the place of a renderer prep hook.
*   Maybe should be done from java/renderer prep for performance reasons
*   (FreeMarkerWorker maybe) but currently this is only surefire way to ensure include 
*   script at least always runs (in reality there could be a higher level hook than FreeMarkerWorker
*   but doesn't seem to exist in ofbiz).
* * Variables like "request" are unavailable here in general in OOTB ofbiz,
*   but minor scipio renderer patches ("initial context mod", survey renderer) try to make some available 
*   so they can be used. however, only use the major one likes "delegator" and "request"; 
*   rest of context may not be current. note "request" not available in emails.
*
-->

<#assign scipioOrigMainNsNamesSet = toSet(.main?keys)/>

<#-- utilities (included early for use in this file, meant to be everything-agnostic) -->
<#include "component://common/webcommon/includes/scipio/lib/utilities.ftl"/>

<#-- variables (htmlVariables.groovy, styles hash: scpLibVars.styles) -->
<#assign scpLibVars = getRequestVar("scpLibVars")!false/>
<#if scpLibVars?is_boolean>
    <#assign scpLibVarsRaw = Static["org.ofbiz.widget.renderer.VisualThemeWorker"].getFtlLibVariables(context)!false/>
    <#-- unnecessary
    <#assign scpLibVarsRaw = rewrapMap(scpLibVarsRaw, "basic-adapter", "always-deep")/>-->
    <#if !scpLibVarsRaw?is_boolean>
        <#assign scpLibVars = rewrapMap(scpLibVarsRaw, "basic-copy", "always-deep")/>
        <#assign dummy = setRequestVar("scpLibVars", scpLibVars, "w")/>
    <#else>
        <#assign scpLibVars = {}/>
    </#if>
</#if>
<#assign dummy = globalsPutAll(scpLibVars)>

<#-- template (htmlTemplate.ftl) -->
<#assign scpLibTmplPath = getRequestVar("scpLibTmplPath")!""/>
<#if scpLibTmplPath?has_content>
    <#include scpLibTmplPath/>
<#else>
    <#assign scpLibTmplPath = raw(Static["org.ofbiz.widget.renderer.VisualThemeWorker"].getFtlLibTemplatePath(context)!"")/>
    <#if scpLibTmplPath?has_content>
        <#include scpLibTmplPath/>
    </#if>
</#if>

<#-- Copy/dump all scipio macro and function defs from main namespace into the global namespace manually,
    because Scipio FTL library uses an #include pattern rather than #import namespaces. -->
<#assign dummy = globalsPutAll(.main, "e", scipioOrigMainNsNamesSet)/>

</#compress>