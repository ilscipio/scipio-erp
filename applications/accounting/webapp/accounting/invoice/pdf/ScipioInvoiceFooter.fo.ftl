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
