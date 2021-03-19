<@section title=uiLabelMap.OrderCustomerInfo>
    <#assign orderType = orderHeader.getRelatedOne("OrderType", false)/>
    <#assign orderPrintAttribute = (delegator.findOne("OrderPrintAttribute", {"orderId", orderId!}, false))! />
    <@table type="fields">
        <@tr>
            <@td class="${styles.grid_large!}3">${uiLabelMap.OrderOrders}
            </@td>
            <@td colspan="3">${orderCount!0}</@td>
        </@tr>
        <@tr>
            <@td class="${styles.grid_large!}3">${uiLabelMap.OrderOrderReturns}
            </@td>
            <@td colspan="3">${returnCount!0}</@td>
        </@tr>
        <@tr>
            <@td class="${styles.grid_large!}3">${uiLabelMap.OrderCustomerOrderValue}
            </@td>
            <@td colspan="3"><@ofbizCurrency amount=orderItemValue!0 /></@td>
        </@tr>
        <@tr>
            <@td class="${styles.grid_large!}3">${uiLabelMap.OrderCustomerReturnValue}
            </@td>
            <@td colspan="3"><@ofbizCurrency amount=returnItemValue!0 /></@td>
        </@tr>
        <@tr>
            <@td class="${styles.grid_large!}3">${uiLabelMap.OrderCustomerOrderReplacementRatio}
            </@td>
            <#if returnItemRatio gte 0.5>
                <@td colspan="3" class="${styles.text_color_error}"> <i class="${styles.icon!} ${styles.icon_error!} "></i> ${returnItemRatio!0} <span id="orderReturnItems">(${returnItemCount!0} / ${orderItemCount!0})</span</@td>
            <#else>
                <@td colspan="3">${returnItemRatio!0} <span id="orderReturnItems">(${returnItemCount!0} / ${orderItemCount!0})</span></@td>
            </#if>
        </@tr>
    </@table>
</@section>