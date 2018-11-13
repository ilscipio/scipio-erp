<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#if productIds?has_content>
  <@section title=uiLabelMap.OrderProductsForPromotion>
    <#if (listSize > 0)>
      <@menu type="button">
        <@menuitem type="link" href=makeOfbizUrl("showPromotionDetails?productPromoId=${productPromoId!}&VIEW_SIZE=${viewSize}&VIEW_INDEX=${viewIndex-1}") text=uiLabelMap.CommonPrevious disabled=(!(viewIndex > 0)) class="${styles.action_nav!}" />
        <@menuitem type="text" text="${lowIndex+1} - ${highIndex} ${rawLabel('CommonOf')} ${listSize}" />
        <@menuitem type="link" href=makeOfbizUrl("showPromotionDetails?productPromoId=${productPromoId!}&VIEW_SIZE=${viewSize}&VIEW_INDEX=${viewIndex+1}") text=uiLabelMap.CommonNext disabled=(!(listSize > highIndex)) class="${styles.action_nav!}" />
      </@menu>
    </#if>

    <#if (listSize > 0)>
      <@table type="data-list" autoAltRows=false width="100%" class="+boxbottom"> <#-- orig: class="boxbottom" --> <#-- orig: cellspacing="0" --> <#-- orig: cellpadding="0" --> <#-- orig: border="0" -->
        <@thead>
          <@tr>
            <@td>${uiLabelMap.CommonQualifier}</@td>
            <@td>${uiLabelMap.CommonBenefit}</@td>
            <@td>&nbsp;</@td>
          </@tr>
        </@thead>
        <@tbody>
        <#list productIds[lowIndex..highIndex-1] as productId>
          <@tr>
            <@td>[<#if productIdsCond.contains(productId)>x<#else>&nbsp;</#if>]</@td>
            <@td>[<#if productIdsAction.contains(productId)>x<#else>&nbsp;</#if>]</@td>
            <@td>
              <@render resource=productsummaryScreen reqAttribs={"optProductId":productId, "listIndex":productId_index}/>
            </@td>
          </@tr>
        </#list>
        </@tbody>
      </@table>
    <#else>
      <@commonMsg type="result-norecord"/>
    </#if>
      
  </@section>
</#if>
