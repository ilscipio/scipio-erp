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
<#if prodCatalogId?has_content>
    <@section title=sectionTitle>    
        <form method="post" action="<@pageUrl>addProdCatalogToParty</@pageUrl>" name="AddProdCatalogToParty">    
            <input type="hidden" name="prodCatalogId" value="${prodCatalogId}"/>
            <@row>
                <@cell columns=12>
                    <@field type="lookup" id="partyId" name="partyId" label=uiLabelMap.CommonParty required=true formName="AddProdCatalogToParty" fieldFormName="LookupPartyName" value=((requestParameters.partyId)!)/>
                </@cell>
            </@row>
            <@row>
                <@cell columns=12>
                    <@field type="select" label=uiLabelMap.PartyRole name="roleTypeId" size="1" required=true>
                        <#assign selectedKey = "">
                        <#list roleTypes as roleType>
                            <#if requestParameters.roleTypeId?has_content>
                                <#assign selectedKey = requestParameters.roleTypeId>                               
                            </#if>                           
                            <option <#if selectedKey == (roleType.roleTypeId!)> selected="selected"</#if> value="${roleType.roleTypeId}">${roleType.get("description",locale)}</option>
                        </#list>
                    </@field>
                </@cell>
            </@row>
             <@row>
                <@cell columns=12>
                    <@field type="datetime" label=uiLabelMap.CommonFrom required=true name="fromDate" value=((requestParameters.fromDate)!) size="25" maxlength="30" id="fromDate1"/>
                </@cell>
            </@row>
            <@row>
                <@cell columns=12>
                    <@field type="datetime" label=uiLabelMap.CommonThru name="thruDate" value="" size="25" maxlength="30" id="fromDate2" value=((requestParameters.thruDate)!)/>                      
                </@cell>
            </@row>
            <@row>
                <@cell columns=12>
                    <@field type="input" name="sequenceNum" label=uiLabelMap.CommonSequenceNum value=((requestParameters.sequenceNum)!) size=20 maxlength=40 />
                </@cell>
            </@row>

            <@row>
                <@cell>
                    <@field type="submit" name="Add" text=uiLabelMap.CommonAdd class="+${styles.link_run_sys!} ${styles.action_update!}"/>
                </@cell>
            </@row>            
        </form>
    </@section>
</#if>