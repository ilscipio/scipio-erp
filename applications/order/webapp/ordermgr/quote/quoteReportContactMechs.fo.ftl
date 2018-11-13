<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<#escape x as x?xml>

        <fo:table border-spacing="3pt">
            <fo:table-column column-width="3.75in"/>
            <fo:table-column column-width="3.75in"/>
            <fo:table-body>
                <fo:table-row>
                    <fo:table-cell>
                        <fo:block>
                            <fo:block font-weight="bold">${uiLabelMap.OrderAddress}: </fo:block>
                            <#if quote.partyId?has_content>
                                <#assign quotePartyNameResult = dispatcher.runSync("getPartyNameForDate", {"partyId":quote.partyId, "compareDate":quote.issueDate?now, "userLogin":userLogin})/>
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


        <fo:table border-spacing="3pt" space-before="0.5in" space-after="0.5in">
            <fo:table-column column-width="1.5in"/>
            <fo:table-column column-width="3.75in"/>
            <fo:table-body>
                <fo:table-row>
                    <fo:table-cell><fo:block>${uiLabelMap.OrderOrderQuoteName}:</fo:block></fo:table-cell>
                    <fo:table-cell><fo:block>${quote.quoteName!}</fo:block></fo:table-cell>
                </fo:table-row>
                <fo:table-row>
                    <fo:table-cell><fo:block>${uiLabelMap.CommonDescription}:</fo:block></fo:table-cell>
                    <fo:table-cell><fo:block>${quote.description!}</fo:block></fo:table-cell>
                </fo:table-row>
                <fo:table-row>
                    <fo:table-cell><fo:block>${uiLabelMap.CommonCurrency}:</fo:block></fo:table-cell>
                    <fo:table-cell><fo:block><#if currency??>${currency.get("description",locale)?default(quote.currencyUomId!)}</#if></fo:block></fo:table-cell>
                </fo:table-row>
                <fo:table-row>
                    <fo:table-cell><fo:block>${uiLabelMap.CommonValidFromDate}:</fo:block></fo:table-cell>
                    <fo:table-cell><fo:block>${(quote.validFromDate.toString())!}</fo:block></fo:table-cell>
                </fo:table-row>
                <fo:table-row>
                    <fo:table-cell><fo:block>${uiLabelMap.CommonValidThruDate}:</fo:block></fo:table-cell>
                    <fo:table-cell><fo:block>${(quote.validThruDate.toString())!}</fo:block></fo:table-cell>
                </fo:table-row>
                <fo:table-row>
                    <fo:table-cell><fo:block>${uiLabelMap.CommonQuoteTerms}:</fo:block></fo:table-cell>
                    <#assign quoteLevelTerms = Static["org.ofbiz.entity.util.EntityUtil"].filterByAnd(quoteTerms, {"quoteItemSeqId": "_NA_"})!>
                    <#if quoteLevelTerms?has_content && quoteLevelTerms.size() gt 0>
                        <fo:table-cell>
                            <#list quoteLevelTerms as quoteLevelTerm>
                                <fo:block>
                                    ${quoteLevelTerm.getRelatedOne("TermType", false).get("description",locale)} ${quoteLevelTerm.termValue?default("")} ${quoteLevelTerm.termDays?default("")} ${quoteLevelTerm.textValue?default("")}
                                </fo:block>
                            </#list>
                        </fo:table-cell>
                    </#if>
                </fo:table-row>
            </fo:table-body>
        </fo:table>
</#escape>
