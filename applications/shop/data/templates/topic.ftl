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

<#macro topics>
<#recurse .node>
</#macro>

<#macro topic>
    <#assign contentId="ECMT" + .node.@id[0]/>
    <Content contentId="${contentId}" contentTypeId="WEB_SITE_PUB_PT" contentName="${.node.topic_heading}" description="${.node.topic_desc?html}" ownerContentId=""/>
    <#assign internalName=.node.@name[0]/>
    <#assign internalNameParts=internalName?split(".")/>
    <#assign firstPart=internalNameParts[0] />
    <#assign nowStamp=UtilDateTime.nowTimestamp()/>
    <#if firstPart == "ELTRN">
        <ContentAssoc contentId="CNTELTRN" contentIdTo="${contentId}" contentAssocTypeId="SUB_CONTENT" fromDate="${nowStamp?string("yyyy-MM-dd HH:mm:ss")}"/>
    </#if>
</#macro>

<#macro @element>
</#macro>
