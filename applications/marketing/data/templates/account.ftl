<?xml version="1.0" encoding="UTF-8"?>
<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<entity-engine-xml>
<#recurse doc>
</entity-engine-xml>

<#macro accounts>
<#recurse .node>
</#macro>

<#macro account>
    <#assign acocuntId=.node.@id[0]/>
    <Party partyId="${accountId!}" partyTypeId="PARTY_GROUP" statusId="PARTY_ENABLED"/>
    <PartyGroup partyId="${accountId!}" groupName="${.node.@name!""}"/>
    <PartyRole partyId="${accountId!}" roleTypeId="_NA_"/>
    <PartyRole partyId="${accountId!}" roleTypeId="ACCOUNT"/>
    <ContactMech contactMechId="${accountId!}_001" contactMechTypeId="EMAIL_ADDRESS" infoString="${.node.@email!""}"/>
    <PartyContactMech partyId="${accountId!}" contactMechId="${accountId!}_001" fromDate="2000-01-01 00:00:00.000"/>
    <PartyContactMechPurpose partyId="${accountId!}" contactMechId="${accountId!}_001" contactMechPurposeTypeId="PRIMARY_EMAIL" fromDate="2019-01-01 00:00:00.000"/>
</#macro>

<#macro @element>
</#macro>
