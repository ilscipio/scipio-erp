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
<#escape x as x?xml>
<fo:block font-size="5pt" text-align="left" color="#999999">
    <fo:table table-layout="fixed" width="100%">
        <fo:table-column column-width="proportional-column-width(25)"/>
        <fo:table-column column-width="proportional-column-width(25)"/>
        <fo:table-column column-width="proportional-column-width(25)"/>
        <fo:table-column column-width="proportional-column-width(25)"/>
        
        <fo:table-body>
            <fo:table-row>
              
              <#-- Company Info -->
              <fo:table-cell>
                <fo:block>
                    <fo:block>${companyName!}</fo:block>
                    <#if postalAddress??>
                        <#if postalAddress?has_content>
                            <#assign dummy = setContextField("postalAddress", postalAddress)>
                            <@render resource="component://party/widget/partymgr/PartyScreens.xml#postalAddressPdfFormatter" />
                        </#if>
                    <#else>
                        <fo:block>${uiLabelMap.CommonNoPostalAddress}</fo:block>
                    </#if>
                </fo:block>
              </fo:table-cell>
              
              <#-- Contact Info -->
              <fo:table-cell>
                  <fo:block>
                      <#if phone?? || email?? || website??>
                            <#if phone??>
                                <fo:block>${uiLabelMap.CommonTelephoneAbbr}:</fo:block>
                                <fo:block><#if phone.countryCode??>${phone.countryCode}-</#if><#if phone.areaCode??>${phone.areaCode}-</#if>${phone.contactNumber!}</fo:block>
                                <fo:block></fo:block>                            
                            </#if>
                            <#if email??>
                                <fo:block>${uiLabelMap.CommonEmail}:</fo:block>
                                <fo:block>${email.infoString!}</fo:block>
                                <fo:block></fo:block>
                            </#if>
                            <#if website??>
                                <fo:block>${uiLabelMap.CommonWebsite}:</fo:block>
                                <fo:block>${website.infoString!}</fo:block>
                                <fo:block></fo:block>   
                            </#if>
                      </#if>
                  </fo:block>       
              </fo:table-cell>
              
              <#-- Tax Detail -->
              <fo:table-cell>
                <fo:block>
                    <#if sendingPartyTaxId??>
                        <fo:block>${uiLabelMap.PartyTaxId}:</fo:block>
                        <fo:block>${sendingPartyTaxId!}</fo:block>
                    </#if>
                </fo:block>
              </fo:table-cell>
              
              <#-- Bank Detail -->
              <fo:table-cell>
                <fo:block>
                    <#if eftAccount??>
                    <fo:block>${uiLabelMap.CommonFinBankName}:</fo:block>
                    <fo:block>${eftAccount.bankName!}</fo:block>
                    <fo:block></fo:block>
                    <fo:block>${uiLabelMap.CommonRouting}:</fo:block>
                    <fo:block>${eftAccount.routingNumber!}</fo:block>
                    <fo:block></fo:block>
                    <fo:block>${uiLabelMap.CommonBankAccntNrAbbr}:</fo:block>
                    <fo:block>${eftAccount.accountNumber!}</fo:block>
                    </#if>
                </fo:block>
              </fo:table-cell>
            </fo:table-row>
        </fo:table-body>
    </fo:table>
</fo:block>
</#escape>
