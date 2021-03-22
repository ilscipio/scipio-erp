<@section title=uiLabelMap.OrderCustomerInfo>
    <#assign orderType = orderHeader.getRelatedOne("OrderType", false)/>
    <#assign orderPrintAttribute = (delegator.findOne("OrderPrintAttribute", {"orderId", orderId!}, false))! />
    <@table type="fields">
        <@tr>
            <@td class="${styles.grid_large!}3">${uiLabelMap.OrderCustomerOrdersReturns}
            </@td>
            <@td colspan="3">${orderCount!0} / ${returnCount!0}</@td>
        </@tr>
        <@tr>
            <@td class="${styles.grid_large!}3">${uiLabelMap.OrderCustomerOrderReturnValue}
            </@td>
            <@td colspan="3"><@ofbizCurrency amount=orderItemValue!0 /> / <@ofbizCurrency amount=returnItemValue!0 /></@td>
        </@tr>
        <@tr>
            <@td class="${styles.grid_large!}3">${uiLabelMap.OrderRFM}
            </@td>
            <@td colspan="3"><span data-toggle="tooltip" data-placement="top" title="RFM score based on Relevancy, Frequency & Monetary segmentation">${rfmRecencyScore}-${rfmFrequencyScore}-${rfmMonetaryScore}</span></@td>
        </@tr>
        <@tr>
            <@td class="${styles.grid_large!}3">${uiLabelMap.OrderCustomerOrderReplacementRatio}
            </@td>
            <#if returnItemRatio gte 0.5>
                <@td colspan="3" class="${styles.text_color_error}"> <i class="${styles.icon!} ${styles.icon_error!} "></i> ${returnItemRatio!0} <span id="orderReturnItems">(${returnItemCount!0} / ${orderItemCount!0})</span></@td>
            <#elseif returnItemRatio gte 0.25>
                <@td colspan="3" class="${styles.text_color_warning}">${returnItemRatio!0} <span id="orderReturnItems">(${returnItemCount!0} / ${orderItemCount!0})</span></@td>
            <#else>
                <@td colspan="3" class="${styles.text_color_success}">${returnItemRatio!0} <span id="orderReturnItems">(${returnItemCount!0} / ${orderItemCount!0})</span></@td>
            </#if>
        </@tr>
    </@table>
</@section>