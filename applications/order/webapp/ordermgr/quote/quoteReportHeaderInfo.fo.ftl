<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#escape x as x?xml>

    <fo:list-block provisional-distance-between-starts="35mm" font-size="10pt">
        <fo:list-item>
            <fo:list-item-label>
                <fo:block font-weight="bold">${uiLabelMap.OrderOrderQuoteType}</fo:block>
            </fo:list-item-label>
            <fo:list-item-body start-indent="body-start()">
                <fo:block font-weight="bold">${(quoteType.get("description",locale))?default(quote.quoteTypeId!)}</fo:block>
            </fo:list-item-body>
        </fo:list-item>
        <fo:list-item>
            <fo:list-item-label>
                <fo:block>${uiLabelMap.OrderOrderQuoteIssueDate}</fo:block>
            </fo:list-item-label>
            <fo:list-item-body start-indent="body-start()">
                <fo:block>${(quote.issueDate.toString())!}</fo:block>
            </fo:list-item-body>
        </fo:list-item>
        <fo:list-item>
            <fo:list-item-label>
                <fo:block>${uiLabelMap.OrderOrderQuoteId}</fo:block>
            </fo:list-item-label>
            <fo:list-item-body start-indent="body-start()">
                <fo:block>${quote.quoteId}</fo:block>
            </fo:list-item-body>
        </fo:list-item>
        <fo:list-item>
            <fo:list-item-label>
                <fo:block>${uiLabelMap.CommonStatus}</fo:block>
            </fo:list-item-label>
            <fo:list-item-body start-indent="body-start()">
                <fo:block font-weight="bold">${(statusItem.get("description", locale))?default(quote.statusId!)}</fo:block>
            </fo:list-item-body>
        </fo:list-item>
    </fo:list-block>

    <fo:table border-spacing="3pt" space-before="0.5in" space-after="0.5in" font-size="10pt">
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
