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
<@section title="${uiLabelMap.PageTitleEditProductFeatures}">
    <form method="post" action="<@ofbizUrl>UpdateFeatureToProductApplication</@ofbizUrl>" name="selectAllForm">
      <input type="hidden" name="_useRowSubmit" value="Y"/>
      <input type="hidden" name="_checkGlobalScope" value="Y"/>
      <input type="hidden" name="productId" value="${productId}"/>
      <@table type="data-list" autoAltRows=true cellspacing="0" class="basic-table">
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
        </@tr>
        </@thead>
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
          <a href="<@ofbizUrl>EditFeature?productFeatureId=${(productFeatureAndAppl.productFeatureId)!}</@ofbizUrl>" class="${styles.button_default!}">
              ${(productFeatureAndAppl.productFeatureId)!}</a></@td>
          <@td>${(productFeatureAndAppl.get("description",locale))!}</@td>
          <@td><#if productFeatureAndAppl.uomId??>${curProductFeatureUom.abbreviation!}</#if></@td>
          <@td>${(curProductFeatureType.get("description",locale))?default((productFeatureAndAppl.productFeatureTypeId)!)}</@td>
          <@td><a href="<@ofbizUrl>EditFeatureCategoryFeatures?productFeatureCategoryId=${(productFeatureAndAppl.productFeatureCategoryId)!}&amp;productId=${(productFeatureAndAppl.productId)!}</@ofbizUrl>" class="${styles.button_default!}">
              ${(curProductFeatureCategory.description)!}
              [${(productFeatureAndAppl.productFeatureCategoryId)!}]</a></@td>
    <#assign hasntStarted = false>
    <#if (productFeatureAndAppl.getTimestamp("fromDate"))?? && Static["org.ofbiz.base.util.UtilDateTime"].nowTimestamp().before(productFeatureAndAppl.getTimestamp("fromDate"))> <#assign hasntStarted = true></#if>
          <#assign colorStyle><#if hasntStarted> style='color: red;'</#if></#assign>
          <@td style=colorStyle>${(productFeatureAndAppl.fromDate)!}</@td>
          <@td>
    <#assign hasExpired = false>
    <#if (productFeatureAndAppl.getTimestamp("thruDate"))?? && Static["org.ofbiz.base.util.UtilDateTime"].nowTimestamp().after(productFeatureAndAppl.getTimestamp("thruDate"))> <#assign hasExpired = true></#if>
            <#if hasExpired><#assign class="alert"></#if>
            <@htmlTemplate.renderDateTimeField name="thruDate_o_${productFeatureAndAppl_index}" event="" action="" className="${class!''}"  title="Format: yyyy-MM-dd HH:mm:ss.SSS" value="${(productFeatureAndAppl.thruDate)!}" size="25" maxlength="30" id="thruDate_o_${productFeatureAndAppl_index}" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
            <input type="text" size='6' name='amount_o_${productFeatureAndAppl_index}' value='${(productFeatureAndAppl.amount)!}' />
            <input type="text" size='5' name='sequenceNum_o_${productFeatureAndAppl_index}' value='${(productFeatureAndAppl.sequenceNum)!}' />
            <select name='productFeatureApplTypeId_o_${productFeatureAndAppl_index}' size="1">
    <#if (productFeatureAndAppl.productFeatureApplTypeId)??>
              <option value='${(productFeatureAndAppl.productFeatureApplTypeId)!}'><#if curProductFeatureApplType??> ${(curProductFeatureApplType.get("description",locale))!} <#else> [${productFeatureAndAppl.productFeatureApplTypeId}]</#if></option>
              <option value='${productFeatureAndAppl.productFeatureApplTypeId}'> </option>
    </#if>
      <#list productFeatureApplTypes as productFeatureApplType>
              <option value='${(productFeatureApplType.productFeatureApplTypeId)!}'>${(productFeatureApplType.get("description",locale))!} </option>
    </#list>
            </select>
          </@td>
          <@td align="right">
            <input type="checkbox" name="_rowSubmit_o_${productFeatureAndAppl_index}" value="Y" onclick="javascript:checkToggle(this, 'selectAllForm');highlightRow(this,'productFeatureId_tableRow_${productFeatureAndAppl_index}');" />
          </@td>
          <@td>
            <a href="javascript:document.RemoveFeatureFromProduct_o_${productFeatureAndAppl_index}.submit()" class="${styles.button_default!}">${uiLabelMap.CommonDelete}</a>
          </@td>
        </@tr>
  </#list>
        <@tfoot>
        <@tr>
          <@td colspan="8" align="center">
            <input type="hidden" name="_rowCount" value="${productFeatureAndAppls.size()}"/>
            <input type="submit" value='${uiLabelMap.CommonUpdate}'/>
          </@td>
        </@tr>
        </@tfoot>
      </@table>
    </form>
  <#list productFeatureAndAppls as productFeatureAndAppl>
    <form name= "RemoveFeatureFromProduct_o_${productFeatureAndAppl_index}" method= "post" action= "<@ofbizUrl>RemoveFeatureFromProduct</@ofbizUrl>">
      <input type= "hidden" name= "productId" value= "${(productFeatureAndAppl.productId)!}"/>
      <input type= "hidden" name= "productFeatureId" value= "${(productFeatureAndAppl.productFeatureId)!}"/>
      <input type= "hidden" name= "fromDate" value= "${(productFeatureAndAppl.fromDate)!}"/>
    </form>
  </#list>
</@section>

<@section title="${uiLabelMap.ProductAddProductFeatureFromCategory}">
    <form method="post" action="<@ofbizUrl>ApplyFeaturesFromCategory</@ofbizUrl>" style='margin: 0;'>
      <input type="hidden" name="productId" value="${productId}"/>
      <select name='productFeatureCategoryId' size="1">
        <option value='' selected="selected">${uiLabelMap.ProductChooseFeatureCategory}</option>
  <#list productFeatureCategories as productFeatureCategory>
        <option value='${(productFeatureCategory.productFeatureCategoryId)!}'>${(productFeatureCategory.description)!} [${(productFeatureCategory.productFeatureCategoryId)!}]</option>
  </#list>
      </select>
      <select name='productFeatureGroupId' size="1">
        <option value='' selected="selected">${uiLabelMap.ProductChooseFeatureGroup}</option>
  <#list productFeatureGroups as productFeatureGroup>
        <option value='${(productFeatureGroup.productFeatureGroupId)!}'>${(productFeatureGroup.description)!} [${(productFeatureGroup.productFeatureGroupId)!}]</option>
  </#list>
      </select>
      <span class='label'>${uiLabelMap.ProductFeatureApplicationType}: </span>
      <select name='productFeatureApplTypeId' size="1">
  <#list productFeatureApplTypes as productFeatureApplType>
        <option value='${(productFeatureApplType.productFeatureApplTypeId)!}'
    <#if (productFeatureApplType.productFeatureApplTypeId?? && product?? && product.isVirtual == 'Y' && productFeatureApplType.productFeatureApplTypeId =="SELECTABLE_FEATURE")>selected="selected"</#if>
    <#if (productFeatureApplType.productFeatureApplTypeId?? && product?? && product.isVirtual == 'N' && productFeatureApplType.productFeatureApplTypeId! =="STANDARD_FEATURE")>selected="selected"</#if>
            >${(productFeatureApplType.get("description",locale))!} </option>
  </#list>
      </select>
      <input type="submit" value='${uiLabelMap.CommonAdd}'/>
    </form>
</@section>

<@section title="${uiLabelMap.ProductAddProductFeatureTypeId}">
    <form method="post" action="<@ofbizUrl>ApplyFeatureToProductFromTypeAndCode</@ofbizUrl>" name='addFeatureByTypeIdCode'>
      <input type="hidden" name="productId" value="${productId}"/>
      <span class='label'>${uiLabelMap.ProductFeatureType}: </span>
      <select name='productFeatureTypeId' size="1">
  <#list productFeatureTypes as productFeatureType>
        <option value='${(productFeatureType.productFeatureTypeId)!}'>${(productFeatureType.get("description",locale))!} </option>
  </#list>
      </select>
      <span class='label'>${uiLabelMap.CommonIdCode}: </span><input type="text" size='10' name='idCode' value=''/>
      <br />
      <span class='label'>${uiLabelMap.ProductFeatureApplicationType}: </span>
      <select name='productFeatureApplTypeId' size="1">
  <#list productFeatureApplTypes as productFeatureApplType>
        <option value='${(productFeatureApplType.productFeatureApplTypeId)!}'
    <#if (productFeatureApplType.productFeatureApplTypeId?? && product?? && product.isVirtual == 'Y' && productFeatureApplType.productFeatureApplTypeId =="SELECTABLE_FEATURE")>selected="selected"</#if>
    <#if (productFeatureApplType.productFeatureApplTypeId?? && product?? && product.isVirtual == 'N' && productFeatureApplType.productFeatureApplTypeId =="STANDARD_FEATURE")>selected="selected"</#if>
            >${(productFeatureApplType.get("description",locale))!} </option>
  </#list>
      </select>
      <br />
      <span class='label'>${uiLabelMap.CommonFrom} : </span>
      <@htmlTemplate.renderDateTimeField name="fromDate" event="" action="" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" value="" size="25" maxlength="30" id="fromDate1" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
      <span class='label'>${uiLabelMap.CommonThru} : </span>
      <@htmlTemplate.renderDateTimeField name="thruDate" event="" action="" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" value="" size="25" maxlength="30" id="thruDate1" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
      <span class='label'>${uiLabelMap.CommonSequence} : </span><input type="text" size='5' name='sequenceNum'/>
      <input type="submit" value="${uiLabelMap.CommonAdd}"/>
    </form>
</@section>

<@section title="${uiLabelMap.ProductAddProductFeatureID}">
    <form method="post" action="<@ofbizUrl>ApplyFeatureToProduct</@ofbizUrl>" name="addFeatureById">
      <input type="hidden" name="productId" value="${productId}"/>
      <span>${uiLabelMap.CommonId}: </span>
      <span class='label'>
        <@htmlTemplate.lookupField formName="addFeatureById" name="productFeatureId" id="productFeatureId" fieldFormName="LookupProductFeature"/>
      </span>
      <span>${uiLabelMap.ProductFeatureApplicationType}: </span>
      <select name="productFeatureApplTypeId" size="1">
  <#list productFeatureApplTypes as productFeatureApplType>
        <option value='${(productFeatureApplType.productFeatureApplTypeId)!}'
    <#if (productFeatureApplType.productFeatureApplTypeId?? && product?? && product.isVirtual == 'Y' && productFeatureApplType.productFeatureApplTypeId =="SELECTABLE_FEATURE")>selected="selected"</#if>
    <#if (productFeatureApplType.productFeatureApplTypeId?? && product?? && product.isVirtual == 'N' && productFeatureApplType.productFeatureApplTypeId =="STANDARD_FEATURE")>selected="selected"</#if>
            >${(productFeatureApplType.get("description",locale))!} </option>
  </#list>
      </select>
      <br />
      <span>${uiLabelMap.CommonFrom} : </span><@htmlTemplate.renderDateTimeField name="fromDate" event="" action="" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" value="" size="25" maxlength="30" id="fromDate2" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
      <span>${uiLabelMap.CommonThru} : </span><@htmlTemplate.renderDateTimeField name="thruDate" event="" action="" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" value="" size="25" maxlength="30" id="thruDate2" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
      <span>${uiLabelMap.CommonSequence} : </span><input type="text" size="5" name="sequenceNum"/>
      <input type="submit" value="${uiLabelMap.CommonAdd}"/>
    </form>
</@section>
</#if>