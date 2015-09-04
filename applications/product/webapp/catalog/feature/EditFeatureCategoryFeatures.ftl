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
<#assign menuHtml>
  <@menu type="section" inlineItems=true>
  <@menuitem type="link" ofbizHref="CreateFeature?productFeatureCategoryId=${productFeatureCategoryId!}" text="${uiLabelMap.ProductCreateNewFeature}" contentClass="+create" />
  </@menu>
</#assign>
<@section title="${uiLabelMap.ProductEditFeaturesForFeatureCategory} \"${(curProductFeatureCategory.description)!}\"" menuHtml=menuHtml>
        <form action="<@ofbizUrl>QuickAddProductFeatures</@ofbizUrl>" method="post">
          <div>
            ${uiLabelMap.CommonAdd}
            <input type="text" name="featureNum" value="1" size="3" />
            ${uiLabelMap.ProductAddFeatureToCategory}
            <input class="smallSubmit ${styles.button_default!}" type="submit" value="${uiLabelMap.CommonCreate}" />
          </div>
          <input type="hidden" name="productFeatureCategoryId" value="${productFeatureCategoryId}" />
        </form>
</@section>

<@section title="${uiLabelMap.ProductProductFeatureMaintenance}">
        <#macro productFeatureMaintNav>
          <#if productId?has_content>
            <#local productString = "&amp;productId=" + productId>
          </#if>
          <@menu type="button">
            <@menuitem type="link" ofbizHref="EditFeatureCategoryFeatures?productFeatureCategoryId=${productFeatureCategoryId!}&amp;VIEW_SIZE=${viewSize}&amp;VIEW_INDEX=${viewIndex-1}${productString!}" text="[${uiLabelMap.CommonPrevious}]" disabled=((viewIndex <= 0)) />
            <li>${lowIndex+1} - ${highIndex} ${uiLabelMap.CommonOf} ${listSize}</li>
            <@menuitem type="link" ofbizHref="EditFeatureCategoryFeatures?productFeatureCategoryId=${productFeatureCategoryId!}&amp;VIEW_SIZE=${viewSize}&amp;VIEW_INDEX=${viewIndex+1}${productString!}" text="[${uiLabelMap.CommonNext}]" disabled=((listSize <= highIndex)) />
          </@menu>
        </#macro>
        
        <#if (listSize > 0)>
          <@productFeatureMaintNav />
        </#if>
         
        <form method='post' action='<@ofbizUrl>UpdateProductFeatureInCategory</@ofbizUrl>' name="selectAllForm">
        <input type="hidden" name="_useRowSubmit" value="Y" />
        <input type="hidden" name="_checkGlobalScope" value="N" />
        <input type="hidden" name="productFeatureCategoryId" value="${productFeatureCategoryId}" />
        <@table type="data-list" autoAltRows=true cellspacing="0" class="basic-table">
           <@thead>
              <@tr class="header-row">
                <@th>${uiLabelMap.CommonId}</@th>
                <@th>${uiLabelMap.CommonDescription}</@th>
                <@th>${uiLabelMap.ProductFeatureType}</@th>
                <@th>${uiLabelMap.ProductFeatureCategory}</@th>
                <@th>${uiLabelMap.ProductUnitOfMeasureId}</@th>
                <@th>${uiLabelMap.ProductQuantity}</@th>
                <@th>${uiLabelMap.ProductAmount}</@th>
                <@th>${uiLabelMap.ProductIdSeqNum}</@th>
                <@th>${uiLabelMap.ProductIdCode}</@th>
                <@th>${uiLabelMap.ProductAbbrev}</@th>
                <@th align="right">${uiLabelMap.CommonAll}<input type="checkbox" name="selectAll" value="${uiLabelMap.CommonY}" onclick="javascript:toggleAll(this, 'selectAllForm');highlightAllRows(this, 'productFeatureId_tableRow_', 'selectAllForm');" /></@th>
             </@tr>
           </@thead>
        <#if (listSize > 0)>
            <@tbody>
            <#assign rowCount = 0>
            <#list productFeatures as productFeature>
            <#assign curProductFeatureType = productFeature.getRelatedOne("ProductFeatureType", true)>
            <@tr id="productFeatureId_tableRow_${rowCount}" valign="middle">
              <@td><input type="hidden" name="productFeatureId_o_${rowCount}" value="${productFeature.productFeatureId}" />
              <a href="<@ofbizUrl>EditFeature?productFeatureId=${productFeature.productFeatureId}</@ofbizUrl>" class="${styles.button_default!}">${productFeature.productFeatureId}</a></@td>
              <@td><input type="text" size='15' name="description_o_${rowCount}" value="${productFeature.description}" /></@td>
              <@td><select name='productFeatureTypeId_o_${rowCount}' size="1">
                <#if productFeature.productFeatureTypeId?has_content>
                  <option value='${productFeature.productFeatureTypeId}'><#if curProductFeatureType??>${curProductFeatureType.get("description",locale)!}<#else> [${productFeature.productFeatureTypeId}]</#if></option>
                  <option value='${productFeature.productFeatureTypeId}'>---</option>
                </#if>
                <#list productFeatureTypes as productFeatureType>
                  <option value='${productFeatureType.productFeatureTypeId}'>${productFeatureType.get("description",locale)!}</option>
                </#list>
              </select></@td>
              <@td><select name='productFeatureCategoryId_o_${rowCount}' size="1">
                <#if productFeature.productFeatureCategoryId?has_content>
                  <#assign curProdFeatCat = productFeature.getRelatedOne("ProductFeatureCategory", false)>
                  <option value='${productFeature.productFeatureCategoryId}'>${(curProdFeatCat.description)!} [${productFeature.productFeatureCategoryId}]</option>
                  <option value='${productFeature.productFeatureCategoryId}'>---</option>
                </#if>
                <#list productFeatureCategories as productFeatureCategory>
                  <option value='${productFeatureCategory.productFeatureCategoryId}'>${productFeatureCategory.get("description",locale)!} [${productFeatureCategory.productFeatureCategoryId}]</option>
                </#list>
              </select></@td>
              <@td><input type="text" size='10' name="uomId_o_${rowCount}" value="${productFeature.uomId!}" /></@td>
              <@td><input type="text" size='5' name="numberSpecified_o_${rowCount}" value="${productFeature.numberSpecified!}" /></@td>
              <@td><input type="text" size='5' name="defaultAmount_o_${rowCount}" value="${productFeature.defaultAmount!}" /></@td>
              <@td><input type="text" size='5' name="defaultSequenceNum_o_${rowCount}" value="${productFeature.defaultSequenceNum!}" /></@td>
              <@td><input type="text" size='5' name="idCode_o_${rowCount}" value="${productFeature.idCode!}" /></@td>
              <@td><input type="text" size='5' name="abbrev_o_${rowCount}" value="${productFeature.abbrev!}" /></@td>
              <@td align="right"><input type="checkbox" name="_rowSubmit_o_${rowCount}" value="Y" onclick="javascript:checkToggle(this, 'selectAllForm');highlightRow(this,'productFeatureId_tableRow_${rowCount}');" /></@td>
            </@tr>
            <#assign rowCount = rowCount + 1>
            </#list>
            </@tbody>
            <@tfoot>
            <@tr><@td colspan="11" align="center">
            <input type="hidden" name="_rowCount" value="${rowCount}" />
            <input type="submit" value='${uiLabelMap.CommonUpdate}'/>
            </@td></@tr>
            </@tfoot>
        </#if>
        </@table>
        </form>
        
        <#if (listSize > 0)>
          <@productFeatureMaintNav />
        </#if>
</@section>
