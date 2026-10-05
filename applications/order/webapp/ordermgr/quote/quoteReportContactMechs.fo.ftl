<#--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
-->
<#--
Changes to this file: Copyright (C) Ilscipio GmbH. The changes are licensed
under the GNU Affero General Public License, version 3, or a commercial
license from Ilscipio GmbH (file LICENSE). The original code stays under
the Apache License, version 2.0, as stated above.
-->
<#escape x as x?xml>
    <fo:block content-width="85mm" font-size="10pt" margin-top="45mm" margin-bottom="5mm">
        <fo:block-container height="5mm" font-size="6pt">
            <fo:block>
                <#-- Return Address -->
                ${companyName!""}
            </fo:block>
        </fo:block-container>
        <fo:block margin-bottom="2mm">
            <fo:table border-spacing="3pt">
                <fo:table-column column-width="3.75in"/>
                <fo:table-column column-width="3.75in"/>
                <fo:table-body>
                    <fo:table-row>
                        <fo:table-cell>
                            <fo:block>
                                <fo:block font-weight="bold">${uiLabelMap.OrderAddress}: </fo:block>
                                <#if quote.partyId?has_content>
                                    <#assign getPartyNameForDateCtx = {"userLogin":userLogin}>
                                    <#if quote.partyId?has_content>
                                        <#assign getPartyNameForDateCtx += {"partyId":quote.partyId}>
                                    </#if>
                                    <#if quote.issueDate?has_content>
                                        <#assign getPartyNameForDateCtx += {"compareDate":quote.issueDate?date}>
                                    </#if>
                                    <#assign quotePartyNameResult = runService("getPartyNameForDate", getPartyNameForDateCtx)/>
                                    <fo:block>${quotePartyNameResult.fullName?default("[${uiLabelMap.OrderPartyNameNotFound}]")}</fo:block>
                                <#else>
                                    <fo:block>[${uiLabelMap.OrderPartyNameNotFound}]</fo:block>
                                </#if>
                            </fo:block>
                        </fo:table-cell>
                    </fo:table-row>
                    <fo:table-row>
                        <fo:table-cell>
                            <fo:block>
                                <#if toPostalAddress??>
                                    <#assign dummy = setContextField("postalAddress", toPostalAddress)>
                                    <@render resource="component://party/widget/partymgr/PartyScreens.xml#postalAddressPdfFormatter" />
                                </#if>
                            </fo:block>
                        </fo:table-cell>
                    </fo:table-row>
                </fo:table-body>
            </fo:table>
        </fo:block>
    </fo:block>
</#escape>
