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

<#macro leads>
<#recurse .node>
</#macro>

<#macro lead>
    <#assign partyId=.node.@id[0]/>

    <#assign companyLeadId=.node.@companyLeadId/>
    <#assign companyName=.node.@companyName/>
    <#assign isOFBizUser=.node.@isOFBizUser/>
    <#assign ofbizUrl=.node.@ofbizUrl/>
    <#assign companyWebsite=.node.@companyWebsite/>
    <#assign firstName=.node.@firstName/>
    <#assign lastName=.node.@lastName/>
    <#assign email=.node.@email/>
    <#assign note=.node.@note/>
    <#assign source=.node.@source/>
    <#if .node.@employeeId?has_content && .node.@employeeId[0]?has_content>
        <#assign employeeId="${partyId}_00"+.node.@employeeId[0]/>
    <#else>
        <#assign employeeId="${partyId}_001"/>
    </#if>
    <#if !companyLeadId?has_content>
        <Party partyId="${partyId}" partyTypeId="PARTY_GROUP" statusId="PARTY_ENABLED" createdDate="2019-01-01 00:00:00.000"/>
        <PartyGroup partyId="${partyId}" groupName="${companyName}"/>
            <#if isOFBizUser?has_content>
        <PartyAttribute partyId="${partyId}" attrName="isOFBizUser" attrValue="${isOFBizUser}" attrDescription="Company uses OFBiz?"/>
        </#if>
            <#if ofbizUrl?has_content>
                <PartyAttribute partyId="${partyId}" attrName="ofbizUrl" attrValue="${ofbizUrl}" attrDescription="OFBiz Installation"/>
        </#if>
            <#if companyWebsite?has_content>
                <PartyAttribute partyId="${partyId}" attrName="companyWebsite" attrValue="${companyWebsite}" attrDescription="Company Website"/>
        </#if>
        <PartyRole partyId="${partyId}" roleTypeId="ACCOUNT"/>
        <PartyRole partyId="${partyId}" roleTypeId="ACCOUNT_LEAD"/>
        <PartyRole partyId="${partyId}" roleTypeId="_NA_"/>
        <#else>
        <#assign partyId=companyLeadId/>
    </#if>
    <#if firstName?has_content || lastName?has_content>
        <Party partyId="${employeeId}" partyTypeId="PERSON" statusId="PARTY_ENABLED" createdDate="2019-01-01 00:00:00.000"/>
        <Person partyId="${employeeId}" firstName="${firstName!""}" lastName="${lastName!""}"/>
        <PartyRole partyId="${employeeId}" roleTypeId="_NA_"/>
        <PartyRole partyId="${employeeId}" roleTypeId="CONTACT"/>
        <PartyRole partyId="${employeeId}" roleTypeId="LEAD"/>
        <PartyRelationship partyIdFrom="${partyId}" roleTypeIdFrom="ACCOUNT_LEAD" partyIdTo="${employeeId}" roleTypeIdTo="LEAD" fromDate="2000-01-01 00:00:00.000" partyRelationshipTypeId="EMPLOYMENT"/>
        <PartyRelationship partyIdFrom="${partyId}" roleTypeIdFrom="ACCOUNT" partyIdTo="${employeeId}" roleTypeIdTo="CONTACT" fromDate="2000-01-01 00:00:00.000" partyRelationshipTypeId="EMPLOYMENT"/>
        <ContactMech contactMechId="${employeeId}" contactMechTypeId="EMAIL_ADDRESS" infoString="${email!"_NA_"}"/>
        <PartyContactMech partyId="${employeeId}" contactMechId="${employeeId}" fromDate="2019-01-01 00:00:00.000"/>
        <PartyContactMechPurpose partyId="${employeeId}" contactMechId="${employeeId}" contactMechPurposeTypeId="PRIMARY_EMAIL" fromDate="2019-01-01 00:00:00.000"/>
    </#if>
    <#if note?has_content>
        <NoteData noteId="${employeeId}" noteName="Known Info" noteInfo="${note}" noteDateTime="2019-01-01 00:00:00.000"/>
        <PartyNote partyId="${partyId}" noteId="${employeeId}"/>
    </#if>
    <#if source?has_content>
        <PartyDataSource partyId="${partyId}" fromDate="2019-01-01 00:00:00.000" dataSourceId="${source}"/>
    </#if>
</#macro>

<#macro @element>
</#macro>
