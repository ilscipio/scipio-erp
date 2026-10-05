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
