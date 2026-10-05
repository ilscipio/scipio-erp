<?xml version="1.0" encoding="UTF-8"?>
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
    <#assign nowStamp=UtilDateTime.nowTimestamp()/>
    <ContentAssoc contentIdTo="${contentIdTo}" contentId="${contentId}" contentAssocTypeId="${assocType}" fromDate="${nowStamp?string("yyyy-MM-dd HH:mm:ss")}"/>
</#macro>

<#macro @element>
</#macro>
