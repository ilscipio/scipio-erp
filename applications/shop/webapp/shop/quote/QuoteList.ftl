<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<@section title=uiLabelMap.EcommerceQuoteHistory>
    <#if quoteList?has_content>
        <@table type="data-list">
          <@thead>
            <@tr>
                <@th width="10%"><span style="white-space: nowrap;">${uiLabelMap.OrderQuote} ${uiLabelMap.CommonNbr}</span></@th>
                <@th width="20%">${uiLabelMap.CommonName}</@th>
                <@th width="40%">${uiLabelMap.CommonDescription}</@th>
                <@th width="10%">${uiLabelMap.CommonStatus}</@th>
                <@th width="20%">
                    <div>${uiLabelMap.OrderOrderQuoteIssueDate}</div>
                    <div>${uiLabelMap.CommonValidFromDate}</div>
                    <div>${uiLabelMap.CommonValidThruDate}</div>
                </@th>
                <@th width="10">&nbsp;</@th>
            </@tr>
          </@thead>
          <@tbody>
            <#list quoteList as quote>
                <#assign status = quote.getRelatedOne("StatusItem", true)>
                <@tr>
                    <@td>${quote.quoteId}</@td>
                    <@td>${quote.quoteName!}</@td>
                    <@td>${quote.description!}</@td>
                    <@td>${status.get("description",locale)}</@td>
                    <@td>
                        <div><span style="white-space: nowrap;">${quote.issueDate!}</span></div>
                        <div><span style="white-space: nowrap;">${quote.validFromDate!}</span></div>
                        <div><span style="white-space: nowrap;">${quote.validThruDate!}</span></div>
                    </@td>
                    <@td align="right">
                        <a href="<@ofbizUrl>ViewQuote?quoteId=${quote.quoteId}</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_view!}">${uiLabelMap.CommonView}</a>
                    </@td>
                </@tr>
            </#list>
          </@tbody>
        </@table>
    <#else>
        <@commonMsg type="result-norecord">${uiLabelMap.OrderNoQuoteFound}</@commonMsg>
    </#if>
</@section>

