<#ftl ns_prefixes={"ask":"http://www.automationgroups.com/dtd/ask/"}>
<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
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
