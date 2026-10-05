<#ftl ns_prefixes={"ask":"http://www.automationgroups.com/dtd/ask/"}>
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

<#recurse doc>


<#macro "ask:document">
  <#recurse .node>
</#macro>

<#macro "ask:title">
<h2>${.node}</h2>
<p/>
</#macro>

<#macro "ask:body">
  <#recurse .node>
</#macro>

<#macro "ask:section">
  <#list .node?children as kid>
    <#if kid?node_name == "sectionTitle">
      <h3><#recurse kid></h3>
    <#else>
      ${kid.@@markup}
    </#if>
  </#list>
</#macro>

<#macro "ask:sectionTitle">
</#macro>

<#macro @text>
${.node?html}
</#macro>
<#--
<#macro content>
    <#assign contentId="ECMC" + .node.@id[0]/>
    <DataResource dataResourceId="${contentId}" dataResourceTypeId="ELECTRONIC_TEXT" dataTemplateTypeId="NONE" dataSourceId="ECM" mimeTypeId="text/html"/>
    <ElectronicText dataResourceId="${contentId}">
<textData><![CDATA[
${.node.content_text}
]]></textData>
    </ElectronicText>
    <Content contentId="${contentId}" contentTypeId="DOCUMENT" dataResourceId="${contentId}" contentName="${.node.@name[0]}" description="${.node.content_desc?html}" mimeTypeId="text/xml" templateDataResourceId="TPL_XML_MB"/>
</#macro>
-->

<#macro @element>
</#macro>
