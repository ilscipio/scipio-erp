<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
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
</#escape>
