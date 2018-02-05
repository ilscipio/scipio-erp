<#--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
-->
<#if productId??>

<#-- SCIPIO: 2017-05-22: added link to new feature -->
<#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
        <@menuitem type="link" href=makeOfbizUrl("EditFeature") text=uiLabelMap.ProductNewFeature class="+${styles.action_nav!} ${styles.action_add!}"/>
    </@menu>
</#macro>
<@section title=uiLabelMap.PageTitleEditProductFeatures menuContent=menuContent>
  <#if productFeatureAndAppls?has_content>
    <form method="post" action="<@ofbizUrl>UpdateFeatureToProductApplication</@ofbizUrl>" name="selectAllForm">
    <@fields type="default-manual-widgetonly">
      <input type="hidden" name="_useRowSubmit" value="Y"/>
      <input type="hidden" name="_checkGlobalScope" value="Y"/>
      <input type="hidden" name="productId" value="${productId}"/>
      <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
        <@thead>
          <@tr class="header-row">
            <@th>${uiLabelMap.CommonId}</@th>
            <@th>${uiLabelMap.CommonDescription}</@th>
            <@th>${uiLabelMap.ProductUomId}</@th>
            <@th>${uiLabelMap.ProductType}</@th>
            <@th>${uiLabelMap.ProductCategory}</@th>
            <@th>${uiLabelMap.CommonFromDate}</@th>
            <@th>${uiLabelMap.ProductThruDateAmountSequenceApplicationType}</@th>
            <@th>${uiLabelMap.CommonAll}<input type="checkbox" name="selectAll" value="${uiLabelMap.CommonY}" onclick="javascript:toggleAll(this, 'selectAllForm');highlightAllRows(this, 'productFeatureId_tableRow_', 'selectAllForm');"/></@th>
            <@th></@th>
          </@tr>
        </@thead>
        <@tbody>
  <#list productFeatureAndAppls as productFeatureAndAppl>
    <#if productFeatureAndAppl.uomId??>
        <#assign curProductFeatureUom = delegator.findOne("Uom",{"uomId",productFeatureAndAppl.uomId}, true)>
    </#if>
    <#assign curProductFeatureType = productFeatureAndAppl.getRelatedOne("ProductFeatureType", true)>
    <#assign curProductFeatureApplType = productFeatureAndAppl.getRelatedOne("ProductFeatureApplType", true)>
    <#assign curProductFeatureCategory = (productFeatureAndAppl.getRelatedOne("ProductFeatureCategory", true)!)>
        <@tr id="productFeatureId_tableRow_${productFeatureAndAppl_index}" valign="middle">
          <@td>
            <input type="hidden" name="productId_o_${productFeatureAndAppl_index}" value="${(productFeatureAndAppl.productId)!}" />
            <input type="hidden" name="productFeatureId_o_${productFeatureAndAppl_index}" value="${(productFeatureAndAppl.productFeatureId)!}" />
            <input type="hidden" name="fromDate_o_${productFeatureAndAppl_index}" value="${(productFeatureAndAppl.fromDate)!}" />
            <a href="<@ofbizUrl>EditFeature?productFeatureId=${(productFeatureAndAppl.productFeatureId)!}</@ofbizUrl>" class="${styles.link_nav_info_id!}">
                ${(productFeatureAndAppl.productFeatureId)!}</a></@td>
          <@td>${(productFeatureAndAppl.get("description",locale))!}</@td>
          <@td><#if productFeatureAndAppl.uomId??>${curProductFeatureUom.abbreviation!}</#if></@td>
          <@td>${(curProductFeatureType.get("description",locale))?default((productFeatureAndAppl.productFeatureTypeId)!)}</@td>
          <@td><a href="<@ofbizUrl>EditFeatureCategoryFeatures?productFeatureCategoryId=${(productFeatureAndAppl.productFeatureCategoryId)!}&amp;productId=${(productFeatureAndAppl.productId)!}</@ofbizUrl>" class="${styles.link_nav_info_desc!}">
              ${(curProductFeatureCategory.description)!}
              [${(productFeatureAndAppl.productFeatureCategoryId)!}]</a></@td>
    <#assign hasntStarted = false>
    <#if (productFeatureAndAppl.getTimestamp("fromDate"))?? && Static["org.ofbiz.base.util.UtilDateTime"].nowTimestamp().before(productFeatureAndAppl.getTimestamp("fromDate"))> <#assign hasntStarted = true></#if>
          <#assign cellClass><#if hasntStarted>+${styles.text_color_alert!}</#if></#assign>
          <@td class=cellClass>${(productFeatureAndAppl.fromDate)!}</@td>
          <@td>
    <#assign hasExpired = false>
    <#if (productFeatureAndAppl.getTimestamp("thruDate"))?? && Static["org.ofbiz.base.util.UtilDateTime"].nowTimestamp().after(productFeatureAndAppl.getTimestamp("thruDate"))> <#assign hasExpired = true></#if>
            <#if hasExpired><#assign class="alert"><#else><#assign class=""></#if>
            <@field type="datetime" name="thruDate_o_${productFeatureAndAppl_index}" value=((productFeatureAndAppl.thruDate)!) size="25" maxlength="30" id="thruDate_o_${productFeatureAndAppl_index}" />
            <@field type="text" size="6" name="amount_o_${productFeatureAndAppl_index}" value="${(productFeatureAndAppl.amount)!}" />
            <@field type="text" size="5" name="sequenceNum_o_${productFeatureAndAppl_index}" value="${(productFeatureAndAppl.sequenceNum)!}" />
            <@field type="select" name="productFeatureApplTypeId_o_${productFeatureAndAppl_index}" size="1">
            <#if (productFeatureAndAppl.productFeatureApplTypeId)??>
              <option value="${(productFeatureAndAppl.productFeatureApplTypeId)!}"><#if curProductFeatureApplType??> ${(curProductFeatureApplType.get("description",locale))!} <#else> [${productFeatureAndAppl.productFeatureApplTypeId}]</#if></option>
              <option value="${productFeatureAndAppl.productFeatureApplTypeId}"> </option>
            </#if>
            <#list productFeatureApplTypes as productFeatureApplType>
              <option value="${(productFeatureApplType.productFeatureApplTypeId)!}">${(productFeatureApplType.get("description",locale))!} </option>
            </#list>
            </@field>
          </@td>
          <@td align="right">
            <input type="checkbox" name="_rowSubmit_o_${productFeatureAndAppl_index}" value="Y" onclick="javascript:checkToggle(this, 'selectAllForm');highlightRow(this,'productFeatureId_tableRow_${productFeatureAndAppl_index}');" />
          </@td>
          <@td>
            <a href="javascript:document.RemoveFeatureFromProduct_o_${productFeatureAndAppl_index}.submit()" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonDelete}</a>
          </@td>
        </@tr>
  </#list>
        </@tbody>
        <@tfoot>
          <@tr>
            <@td colspan="8" align="center">
              <input type="hidden" name="_rowCount" value="${productFeatureAndAppls.size()}"/>
              <input type="submit" value="${uiLabelMap.CommonUpdate}" class="${styles.link_run_sys!} ${styles.action_update!}"/>
            </@td>
          </@tr>
        </@tfoot>
      </@table>
    </@fields>
    </form>
  <#list productFeatureAndAppls as productFeatureAndAppl>
    <form name="RemoveFeatureFromProduct_o_${productFeatureAndAppl_index}" method="post" action="<@ofbizUrl>RemoveFeatureFromProduct</@ofbizUrl>">
      <input type="hidden" name="productId" value="${(productFeatureAndAppl.productId)!}"/>
      <input type="hidden" name="productFeatureId" value="${(productFeatureAndAppl.productFeatureId)!}"/>
      <input type="hidden" name="fromDate" value="${(productFeatureAndAppl.fromDate)!}"/>
    </form>
  </#list>
  <#else>
    <@commonMsg type="result-norecord"/>
  </#if>
</@section>

<@section title=uiLabelMap.ProductAddProductFeatureFromCategory>
    <form method="post" action="<@ofbizUrl>ApplyFeaturesFromCategory</@ofbizUrl>">
      <input type="hidden" name="productId" value="${productId}"/>
      
      <@field type="select" label=uiLabelMap.ProductFeatureCategory size=1 name="productFeatureCategoryId">
        <option value="" selected="selected">${uiLabelMap.ProductChooseFeatureCategory}</option>
      <#list productFeatureCategories as productFeatureCategory>
        <option value="${(productFeatureCategory.productFeatureCategoryId)!}">${(productFeatureCategory.description)!} [${(productFeatureCategory.productFeatureCategoryId)!}]</option>
      </#list>
      </@field>
      
      <@field type="select" label=uiLabelMap.ProductFeatureGroup size=1 name="productFeatureGroupId">
        <option value="" selected="selected">${uiLabelMap.ProductChooseFeatureGroup}</option>
      <#list productFeatureGroups as productFeatureGroup>
        <option value="${(productFeatureGroup.productFeatureGroupId)!}">${(productFeatureGroup.description)!} [${(productFeatureGroup.productFeatureGroupId)!}]</option>
      </#list>
      </@field>
      
      <@field type="select" label=uiLabelMap.ProductFeatureApplicationType size=1 name="productFeatureApplTypeId">
      <#list productFeatureApplTypes as productFeatureApplType>
        <option value="${(productFeatureApplType.productFeatureApplTypeId)!}"
          <#if (productFeatureApplType.productFeatureApplTypeId?? && product?? && product.isVirtual == 'Y' && (productFeatureApplType.productFeatureApplTypeId!) =="SELECTABLE_FEATURE")>selected="selected"</#if>
          <#if (productFeatureApplType.productFeatureApplTypeId?? && product?? && product.isVirtual == 'N' && (productFeatureApplType.productFeatureApplTypeId!) =="STANDARD_FEATURE")>selected="selected"</#if>
            >${(productFeatureApplType.get("description",locale))!} </option>
      </#list>
      </@field>
      
      <@field type="submit" text=uiLabelMap.CommonAdd class="+${styles.link_run_sys!} ${styles.action_add!}"/>
    </form>
</@section>

<@section title=uiLabelMap.ProductAddProductFeatureTypeId>
    <form method="post" action="<@ofbizUrl>ApplyFeatureToProductFromTypeAndCode</@ofbizUrl>" name="addFeatureByTypeIdCode">
      <input type="hidden" name="productId" value="${productId}"/>
      
      <@field type="select" label=uiLabelMap.ProductFeatureType size=1 name="productFeatureTypeId">
      <#list productFeatureTypes as productFeatureType>
        <option value="${(productFeatureType.productFeatureTypeId)!}">${(productFeatureType.get("description",locale))!} </option>
      </#list>
      </@field>
      
      <@field type="input" label=uiLabelMap.CommonIdCode size=10 name="idCode" value="" />
    
      <@field type="select" label=uiLabelMap.ProductFeatureApplicationType size=1 name="productFeatureApplTypeId">
      <#list productFeatureApplTypes as productFeatureApplType>
        <option value="${(productFeatureApplType.productFeatureApplTypeId)!}"
          <#if (productFeatureApplType.productFeatureApplTypeId?? && product?? && product.isVirtual == 'Y' && productFeatureApplType.productFeatureApplTypeId =="SELECTABLE_FEATURE")>selected="selected"</#if>
          <#if (productFeatureApplType.productFeatureApplTypeId?? && product?? && product.isVirtual == 'N' && productFeatureApplType.productFeatureApplTypeId =="STANDARD_FEATURE")>selected="selected"</#if>
            >${(productFeatureApplType.get("description",locale))!} </option>
      </#list>
      </@field>
   
      <@field type="datetime" dateType="date" label=uiLabelMap.CommonFrom name="fromDate" size="25" maxlength="30" id="fromDate1" />
      <@field type="datetime" dateType="date" label=uiLabelMap.CommonThru name="thruDate" size="25" maxlength="30" id="thruDate1" />
      <@field type="input" label=uiLabelMap.CommonSequence size=5 name="sequenceNum" value="" />
    
      <@field type="submit" text=uiLabelMap.CommonAdd class="+${styles.link_run_sys!} ${styles.action_add!}"/>
    </form>
</@section>

<@section title=uiLabelMap.ProductAddProductFeatureID>
    <form method="post" action="<@ofbizUrl>ApplyFeatureToProduct</@ofbizUrl>" name="addFeatureById">
      <input type="hidden" name="productId" value="${productId}"/>
      
      <@field type="lookup" label=uiLabelMap.CommonId formName="addFeatureById" name="productFeatureId" id="productFeatureId" fieldFormName="LookupProductFeature" />
      
      <@field type="select" label=uiLabelMap.ProductFeatureApplicationType size=1 name="productFeatureApplTypeId">
      <#list productFeatureApplTypes as productFeatureApplType>
        <option value="${(productFeatureApplType.productFeatureApplTypeId)!}"
          <#if (productFeatureApplType.productFeatureApplTypeId?? && product?? && product.isVirtual == 'Y' && productFeatureApplType.productFeatureApplTypeId =="SELECTABLE_FEATURE")>selected="selected"</#if>
          <#if (productFeatureApplType.productFeatureApplTypeId?? && product?? && product.isVirtual == 'N' && productFeatureApplType.productFeatureApplTypeId =="STANDARD_FEATURE")>selected="selected"</#if>
            >${(productFeatureApplType.get("description",locale))!} </option>
      </#list>
      </@field>
   
      <@field type="datetime" dateType="date" label=uiLabelMap.CommonFrom name="fromDate" size="25" maxlength="30" id="fromDate2" />
      <@field type="datetime" dateType="date" label=uiLabelMap.CommonThru name="thruDate" size="25" maxlength="30" id="thruDate2" />
      <@field type="input" label=uiLabelMap.CommonSequence size=5 name="sequenceNum" value="" />
    
      <@field type="submit" text=uiLabelMap.CommonAdd class="+${styles.link_run_sys!} ${styles.action_add!}"/>
    </form>
</@section>

</#if>