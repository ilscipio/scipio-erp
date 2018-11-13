<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<#escape x as x?xml>
    <fo:root xmlns:fo="http://www.w3.org/1999/XSL/Format">
        <fo:layout-master-set>
            <fo:simple-page-master master-name="main" page-height="8in" page-width="8.5in"
                    margin-top="0.1in" margin-bottom="1in" margin-left="0.2in" margin-right="0.5in">
                <fo:region-body margin-top="1in"/>
            </fo:simple-page-master>
        </fo:layout-master-set>
        <fo:page-sequence master-reference="main">
            <fo:flow flow-name="xsl-region-body" font-family="Helvetica">
                <fo:block text-align="center"><@render resource="component://order/widget/ordermgr/OrderPrintScreens.xml#CompanyLogo" /></fo:block>
                <#if acctgTransList?has_content>
                    <fo:block>${uiLabelMap.AccountingAcctgTransFor}
                        <#if (organizationPartyId)??>
                            <#assign partyName = (delegator.findOne("PartyNameView", {"partyId" : organizationPartyId}, false))!>
                            <#if partyName.partyTypeId == "PERSON">
                                ${(partyName.firstName)!} ${(partyName.lastName)!}
                            <#elseif ((partyName.partyTypeId)!) == "PARTY_GROUP">
                                ${(partyName.groupName)!}
                            </#if>
                        </#if>
                    </fo:block>
                    <fo:block>
                        <fo:table>
                            <fo:table-column column-width="15mm"/>
                            <fo:table-column column-width="20mm"/>
                            <fo:table-column column-width="30mm"/>
                            <fo:table-column column-width="20mm"/>
                            <fo:table-column column-width="15mm"/>
                            <fo:table-column column-width="25mm"/>
                            <fo:table-column column-width="20mm"/>
                            <fo:table-column column-width="20mm"/>
                            <fo:table-column column-width="10mm"/>
                            <fo:table-column column-width="25mm"/>
                            <fo:table-header>
                                <fo:table-cell border="1pt solid" border-width=".1mm">
                                    <fo:block text-align="left" text-indent="0.5em" font-size="6pt">${uiLabelMap.CommonId}</fo:block>
                                </fo:table-cell>
                                <fo:table-cell border="2pt solid" border-width=".1mm">
                                    <fo:block text-align="left" text-indent="0.5em" font-size="6pt">${uiLabelMap.FormFieldTitle_transactionDate}</fo:block>
                                </fo:table-cell>
                                <fo:table-cell border="1pt solid" border-width=".1mm">
                                    <fo:block text-align="left" text-indent="0.5em" font-size="6pt">${uiLabelMap.CommonType}</fo:block>
                                </fo:table-cell>
                                <fo:table-cell border="1pt solid" border-width=".1mm">
                                    <fo:block text-align="left" text-indent="0.5em" font-size="6pt">${uiLabelMap.FormFieldTitle_glFiscalTypeId}</fo:block>
                                </fo:table-cell>
                                <fo:table-cell border="1pt solid" border-width=".1mm">
                                    <fo:block text-align="left" text-indent="0.5em" font-size="6pt">${uiLabelMap.AccountingInvoice}</fo:block>
                                </fo:table-cell>
                                <fo:table-cell border="1pt solid" border-width=".1mm">
                                    <fo:block text-align="left" text-indent="0.5em" font-size="6pt">${uiLabelMap.AccountingPayment}</fo:block>
                                </fo:table-cell>
                                <fo:table-cell border="1pt solid" border-width=".1mm">
                                    <fo:block text-align="left" text-indent="0.5em" font-size="6pt">${uiLabelMap.WorkEffort}</fo:block>
                                </fo:table-cell>
                                <fo:table-cell border="1pt solid" border-width=".1mm">
                                    <fo:block text-align="left" text-indent="0.5em" font-size="6pt">${uiLabelMap.CommonShipment}</fo:block>
                                </fo:table-cell>
                                <fo:table-cell border="1pt solid" border-width=".1mm">
                                    <fo:block text-align="center" text-indent="0.5em" font-size="6pt">${uiLabelMap.FormFieldTitle_isPosted}</fo:block>
                                </fo:table-cell>
                                <fo:table-cell border="1pt solid" border-width=".1mm">
                                    <fo:block text-align="left" text-indent="0.5em" font-size="6pt">${uiLabelMap.FormFieldTitle_postedDate}</fo:block>
                                </fo:table-cell>
                            </fo:table-header>
                            <fo:table-body>
                                <#list acctgTransList as acctgTrans>
                                    <fo:table-row>
                                        <fo:table-cell border="1pt solid" border-width=".1mm">
                                            <fo:block text-align="left" text-indent="0.5em" font-size="5pt">${(acctgTrans.acctgTransId)!}</fo:block>
                                        </fo:table-cell>
                                        <fo:table-cell border="1pt solid" border-width=".1mm">
                                            <fo:block text-align="left" text-indent="0.5em" font-size="5pt">
                                                <#assign dateFormat = Static["java.text.DateFormat"].LONG/>
                                                <#assign transactionDate = Static["java.text.DateFormat"].getDateInstance(dateFormat, locale).format((acctgTrans.transactionDate)!)/>
                                                ${(transactionDate)!}
                                            </fo:block>
                                        </fo:table-cell>
                                        <fo:table-cell border="1pt solid" border-width=".1mm">
                                            <fo:block text-align="left" text-indent="0.5em" font-size="5pt">
                                                <#if (acctgTrans.acctgTransTypeId)??>
                                                    <#assign acctgTransType = (delegator.findOne("AcctgTransType", {"acctgTransTypeId" : (acctgTrans.acctgTransTypeId)!}, false))!/>
                                                    <#if acctgTransType?has_content>${acctgTransType.description}</#if>
                                                </#if>
                                            </fo:block>
                                        </fo:table-cell>
                                        <fo:table-cell border="1pt solid" border-width=".1mm">
                                            <fo:block text-align="center" font-size="5pt">
                                                <#if (acctgTrans.glFiscalTypeId)??>
                                                    <#assign glFiscalType = (delegator.findOne("GlFiscalType", {"glFiscalTypeId" : (acctgTrans.glFiscalTypeId)!}, false))!/>
                                                    ${(glFiscalType.description)!}
                                                </#if>
                                            </fo:block>
                                        </fo:table-cell>
                                        <fo:table-cell border="1pt solid" border-width=".1mm">
                                            <fo:block text-align="left" text-indent="0.5em" font-size="5pt">
                                                ${(acctgTrans.invoiceId)!}
                                            </fo:block>
                                        </fo:table-cell>
                                        <fo:table-cell border="1pt solid" border-width=".1mm">
                                            <fo:block text-align="left" text-indent="0.5em" font-size="5pt">
                                                <#if (acctgTrans.paymentId)??>
                                                    <#assign paymentType = (delegator.findOne("Payment", {"paymentId" : (acctgTrans.paymentId)!}, false)).getRelatedOne("PaymentType", false)/>
                                                    ${(acctgTrans.paymentId)!}<#if (paymentType?has_content)> - ${paymentType.description!}</#if>
                                                </#if>
                                            </fo:block>
                                        </fo:table-cell>
                                        <fo:table-cell border="1pt solid" border-width=".1mm">
                                            <fo:block text-align="left" text-indent="0.5em" font-size="5pt">
                                                ${(acctgTrans.workEffortId)!}
                                            </fo:block>
                                        </fo:table-cell>
                                        <fo:table-cell border="1pt solid" border-width=".1mm">
                                            <fo:block text-align="left" text-indent="0.5em" font-size="5pt">
                                                ${(acctgTrans.shipmentId)!}
                                            </fo:block>
                                        </fo:table-cell>
                                        <fo:table-cell border="1pt solid" border-width=".1mm">
                                            <fo:block text-align="center" font-size="5pt">
                                                ${(acctgTrans.isPosted)!}
                                            </fo:block>
                                        </fo:table-cell>
                                        <fo:table-cell border="1pt solid" border-width=".1mm">
                                            <fo:block text-align="left" text-indent="0.5em" font-size="5pt">
                                                <#if acctgTrans.postedDate?has_content>
                                                    <#assign dateFormat = Static["java.text.DateFormat"].LONG/>
                                                    <#assign postedDate = Static["java.text.DateFormat"].getDateInstance(dateFormat, locale).format((acctgTrans.postedDate)!)/>
                                                    ${(postedDate)!}
                                                </#if>
                                            </fo:block>
                                        </fo:table-cell>
                                    </fo:table-row>
                                </#list>
                            </fo:table-body>
                        </fo:table>
                    </fo:block>
                <#else>
                    <fo:block text-align="center">${uiLabelMap.AccountingNoAcctgTransFound}</fo:block>
                </#if>
            </fo:flow>
        </fo:page-sequence>
    </fo:root>
</#escape>
