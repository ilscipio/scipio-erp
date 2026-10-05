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
