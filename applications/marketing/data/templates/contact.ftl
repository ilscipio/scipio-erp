<?xml version="1.0" encoding="UTF-8"?>
<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<entity-engine-xml>
<#recurse doc>
</entity-engine-xml>

<#macro contacts>
<#recurse .node>
</#macro>

<#macro contact>
    <#assign partyId=.node.@id[0]/>
    <Party partyId="${partyId!}" partyTypeId="PARTY_GROUP" statusId="PARTY_ENABLED"/>
    <Party partyId="${partyId!}"/>
    <PartyRole partyId="${partyId!}" roleTypeId="CONTACT"/>
    <PartyRelationship partyIdFrom="${.node.@account[0]!}" roleTypeIdFrom="ACCOUNT" partyIdTo="${partyId!}" roleTypeIdTo="CONTACT" fromDate="2000-01-01 00:00:00.000" partyRelationshipTypeId="EMPLOYMENT"/>
</#macro>

<#macro @element>
</#macro>
