<#--
SCIPIO: Local subscriptions template common defs
-->

<#macro subscriptionLinkContent productSubscriptionResource subscriptionResource orderItem index>
  <#if !subscriptionResource?has_content>
      <#local subscriptionResource = productSubscriptionResource.getRelatedOne("SubscriptionResource", true)>   
  </#if> 
  <#assign subscriptionModalLabel>
    <#if orderItem.subscriptionId?has_content>[${orderItem.subscriptionId!}] - </#if> ${rawString(subscriptionResource.description)}
  </#assign>   
  <@modal id="row_orderitem_subscription_${index}_${orderItem.orderItemSeqId}" label="${subscriptionModalLabel}">
    <@section title="${rawString(subscriptionResource.description)}: ${rawString(orderItem.itemDescription!)}">
        <@table>
            <@tr>
                <@td>Max Time</@td>
                <#local maxLifeTimeUom = productSubscriptionResource.getRelatedOne("MaxLifeTimeUom", true)!>
                <@td>${productSubscriptionResource.maxLifeTime!} <#if maxLifeTimeUom?has_content>${maxLifeTimeUom.description!}</#if></@td>
            </@tr>
            <@tr>
                <@td>Available Time</@td>
                <#local availableTimeUom = productSubscriptionResource.getRelatedOne("AvailableTimeUom", true)!>
                <@td>${productSubscriptionResource.availableTime!} <#if availableTimeUom?has_content>${availableTimeUom.description!}</#if></@td>
            </@tr>
            <@tr>
                <@td>Use Count Limit</@td>                
                <@td>${productSubscriptionResource.useCountLimit!}</@td>
            </@tr>
            <@tr>
                <@td>Use Time</@td>
                <#local useTimeUom = productSubscriptionResource.getRelatedOne("UseTimeUom", true)!>
                <@td>${productSubscriptionResource.useTime!} <#if useTimeUom?has_content>${useTimeUom.description!}</#if></@td>
            </@tr>
            <@tr>
                <@td>Automatic Extend</@td>
                <@td>${productSubscriptionResource.automaticExtend!}</@td>
            </@tr>
            <@tr>
                <@td>Cancel Automatic Extended Time</@td>
                <#local cancelTimeUom = productSubscriptionResource.getRelatedOne("CancelTimeUom", true)!>
                <@td>${productSubscriptionResource.canclAutmExtTime!} <#if cancelTimeUom?has_content>${cancelTimeUom.description!}</#if></@td>
            </@tr>
            <@tr>
                <@td>Period On Expiry</@td>
                <#local gracePeriodUom = productSubscriptionResource.getRelatedOne("GracePeriodUom", true)!>
                <@td>${productSubscriptionResource.gracePeriodOnExpiry!} <#if gracePeriodUom?has_content>${gracePeriodUom.description!}</#if></@td>
            </@tr>
        </@table>               
    </@section>
  </@modal>
</#macro>