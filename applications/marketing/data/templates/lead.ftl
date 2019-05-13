<?xml version="1.0" encoding="UTF-8"?>
<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<entity-engine-xml>
<#recurse doc>
</entity-engine-xml>

<#macro leads>
<#recurse .node>
</#macro>

<#macro lead>
    <#assign partyId=.node.@id[0]/>
    <#assign companyName=.node.@companyName/>
    <#assign isOFBizUser=.node.@isOFBizUser/>
    <#assign ofbizUrl=.node.@ofbizUrl/>
    <#assign companyWebsite=.node.@companyWebsite/>
    <#assign firstName=.node.@firstName/>
    <#assign lastName=.node.@lastName/>
    <#assign email=.node.@email/>
    <#assign note=.node.@note/>
    <#assign source=.node.@source/>
    <Party partyId="${partyId}" partyTypeId="PARTY_GROUP" statusId="PARTY_ENABLED"/>
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
    <#if firstName?has_content || lastName?has_content>
        <Party partyId="${partyId}_001" partyTypeId="PERSON" statusId="PARTY_ENABLED"/>
        <Person partyId="${partyId}_001" firstName="${firstName!""}" lastName="${lastName!""}"/>
        <PartyRole partyId="${partyId}_001" roleTypeId="_NA_"/>
        <PartyRole partyId="${partyId}_001" roleTypeId="CONTACT"/>
        <PartyRole partyId="${partyId}_001" roleTypeId="LEAD"/>
        <PartyRelationship partyIdFrom="${partyId}" roleTypeIdFrom="ACCOUNT_LEAD" partyIdTo="${partyId}_001" roleTypeIdTo="LEAD" fromDate="2000-01-01 00:00:00.000" partyRelationshipTypeId="EMPLOYMENT"/>
        <PartyRelationship partyIdFrom="${partyId}" roleTypeIdFrom="ACCOUNT" partyIdTo="${partyId}_001" roleTypeIdTo="CONTACT" fromDate="2000-01-01 00:00:00.000" partyRelationshipTypeId="EMPLOYMENT"/>
        <ContactMech contactMechId="${partyId}_001" contactMechTypeId="EMAIL_ADDRESS" infoString="${email!"_NA_"}"/>
        <PartyContactMech partyId="${partyId}_001" contactMechId="${partyId}_001" fromDate="2019-01-01 00:00:00.000"/>
        <PartyContactMechPurpose partyId="${partyId}_001" contactMechId="${partyId}_001" contactMechPurposeTypeId="PRIMARY_EMAIL" fromDate="2019-01-01 00:00:00.000"/>
    </#if>
    <#if note?has_content>
        <NoteData noteId="${partyId}_001" noteName="Known Info" noteInfo="${note}" noteDateTime="2019-01-01 00:00:00.000"/>
        <PartyNote partyId="${partyId}" noteId="${partyId}_001"/>
    </#if>
    <#if source?has_content>
        <PartyDataSource partyId="${partyId}" fromDate="2019-01-01 00:00:00.000" dataSourceId="${source}"/>
    </#if>
</#macro>

<#macro @element>
</#macro>
