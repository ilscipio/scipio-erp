<?xml version="1.0" encoding="UTF-8"?>
<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<entity-engine-xml>
<#recurse doc>
</entity-engine-xml>

<#macro topic_tree>
<#recurse .node>
</#macro>

<#macro topic>
    <#assign contentId="ECMT" + .node.@id[0]/>
    <#recurse .node>
</#macro>

<#macro child>
    <#assign contentIdTo="ECMT" + .node.@id[0]/>
    <#assign assocType="SUB_CONTENT"/>
    <#if .node.@type == "content">
        <#assign contentIdTo="ECMC" + .node.@id[0]/>
        <#assign assocType="PUBLISH_LINK"/>
    </#if>
    <#assign nowStamp=Static["org.ofbiz.base.util.UtilDateTime"].nowTimestamp()/>
    <ContentAssoc contentIdTo="${contentIdTo}" contentId="${contentId}" contentAssocTypeId="${assocType}" fromDate="${nowStamp?string("yyyy-MM-dd HH:mm:ss")}"/>
</#macro>

<#macro @element>
</#macro>
