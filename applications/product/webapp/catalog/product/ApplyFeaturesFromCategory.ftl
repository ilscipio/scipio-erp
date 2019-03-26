<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<@menu type="button">
<#if curProductFeatureCategory??>
  <@menuitem type="link" href=makePageUrl("EditFeature?productFeatureCategoryId=${productFeatureCategoryId!}") text=uiLabelMap.ProductCreateNewFeature class="+${styles.action_nav!} ${styles.action_add!}" />
<#elseif productFeatureGroup??>
  <@menuitem type="link" href=makePageUrl("EditFeatureGroupAppls?productFeatureGroupId=${productFeatureGroup.productFeatureGroupId!}") text="${rawLabel('CommonEdit')} ${raw(productFeatureGroup.description!)}" class="+${styles.action_nav!} ${styles.action_add!}"/>
</#if>
<#if productId?has_content>
  <@menuitem type="link" href=makePageUrl("EditProduct?productId=${productId}") text=uiLabelMap.ProductReturnToEditProduct class="+${styles.action_nav!} ${styles.action_cancel!}" />
  <@menuitem type="link" href=makePageUrl("EditProductFeatures?productId=${productId}") text=uiLabelMap.ProductReturnToEditProductFeatures class="+${styles.action_nav!} ${styles.action_cancel!}"/>
</#if>
</@menu>


<#if (listSize > 0)>

<#assign selectedFeatureApplTypeId = selFeatureApplTypeId!>
<#-- SCIPIO: NOTE: productFeatureGroupId was not in stock; has been added by us. 
    NOTE: we added a productFeaturesPaginated flag because pagination only partly implemented by stock depending on search options (productFeatures list only paginated if productFeatureGroupId is not set) -->
<#assign paramStr = addParamsToStr("", {"productFeatureCategoryId": productFeatureCategoryId!"", "productFeatureApplTypeId": selectedFeatureApplTypeId!"", "productId": productId!"", "productFeatureGroupId": productFeatureGroupId!""}, "&amp;", false)>
<@paginate mode="content" url=makePageUrl("ApplyFeaturesFromCategory") paramStr=paramStr viewSize=viewSize!1 viewIndex=viewIndex!0 listSize=listSize!0 paginateOn=((productFeaturesPaginated!true)==true)>
<form method="post" action="<@pageUrl>ApplyFeaturesToProduct</@pageUrl>" name="selectAllForm">
  <@fields type="default-manual-widgetonly">
    <input type="hidden" name="_useRowSubmit" value="Y" />
    <input type="hidden" name="_checkGlobalScope" value="Y" />
    <input type="hidden" name="productId" value="${productId}" />
  
    <@table type="data-list" autoAltRows=true>
      <@thead>
      <@tr class="header-row">
        <@th>${uiLabelMap.CommonId}</@th>
        <@th>${uiLabelMap.CommonDescription}</@th>
        <@th>${uiLabelMap.ProductFeatureType}</@th>
        <@th>${uiLabelMap.ProductApplType}</@th>
        <@th>${uiLabelMap.CommonFromDate}</@th>
        <@th>${uiLabelMap.CommonThruDate}</@th>
        <@th>${uiLabelMap.ProductAmount}</@th>
        <@th>${uiLabelMap.CommonSequence}</@th>
        <@th>${uiLabelMap.CommonAll}<input type="checkbox" name="selectAll" value="${uiLabelMap.CommonY}" onclick="javascript:toggleAll(this, 'selectAllForm');highlightAllRows(this, 'productFeatureId_tableRow_', 'selectAllForm');" /></@th>
      </@tr>
      </@thead>
    <#assign rowCount = 0>
    <#if (listSize > 0)>
    <#list productFeatures as productFeature>
      <#assign curProductFeatureType = productFeature.getRelatedOne("ProductFeatureType", true)>
        <@tr id="productFeatureId_tableRow_${rowCount}" valign="middle">
            <input type="hidden" name="productFeatureId_o_${rowCount}" value="${productFeature.productFeatureId}" />
            <@td><a href="<@pageUrl>EditFeature?productFeatureId=${productFeature.productFeatureId}</@pageUrl>" class="${styles.link_nav_info_id!}">${productFeature.productFeatureId}</a></@td>
            <@td>${productFeature.description!}</@td>
            <@td><#if curProductFeatureType??>${curProductFeatureType.description!}<#else> [${productFeature.productFeatureTypeId}]</#if></@td>
            <@td>
              <select name="productFeatureApplTypeId_o_${rowCount}" size="1">
                <#list productFeatureApplTypes as productFeatureApplType>
                  <option value="${productFeatureApplType.productFeatureApplTypeId}"<#if (selectedFeatureApplTypeId?has_content) && (productFeatureApplType.productFeatureApplTypeId == selectedFeatureApplTypeId)> selected="selected"</#if>>${productFeatureApplType.get("description", locale)}</option>
                </#list>
              </select>
            </@td>
            <@td>
                <@field type="datetime" name="fromDate_o_${rowCount}" value="" size="25" maxlength="30" id="fromDate_o_${rowCount}" />
            </@td>
            <@td>
               <@field type="datetime" name="thruDate_o_${rowCount}" value="" size="25" maxlength="30" id="thruDate_o_${rowCount}" />
            </@td>
            <@td><input type="text" size="6" name="amount_o_${rowCount}" value="${productFeature.defaultAmount!}" /></@td>
            <@td><input type="text" size="5" name="sequenceNum_o_${rowCount}" value="${productFeature.defaultSequenceNum!}" /></@td>
            <@td align="right">
                <input type="checkbox" name="_rowSubmit_o_${rowCount}" value="Y" onclick="javascript:checkToggle(this, 'selectAllForm');highlightRow(this,'productFeatureId_tableRow_${rowCount}');" />
            </@td>
        </@tr>
        <#assign rowCount = rowCount + 1>
    </#list>
      <@tfoot>
        <@tr><@td colspan="9" align="center"><input type="submit" value="${uiLabelMap.CommonApply}" class="${styles.link_run_sys!} ${styles.action_update!}" /></@td></@tr>
      </@tfoot>
    </#if>
    </@table>

    <input type="hidden" name="_rowCount" value="${rowCount!}"/>
  </@fields>
</form>
</@paginate>

<#else>
  <@commonMsg type="result-norecord"/>
</#if>
