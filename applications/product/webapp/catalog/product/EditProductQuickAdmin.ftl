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
<#assign externalKeyParam = "&amp;externalLoginKey=" + requestAttributes.externalLoginKey!>
<#if product?has_content>

<#-- First some general forms and scripts -->
<form name="removeAssocForm" action="<@ofbizUrl>quickAdminUpdateProductAssoc</@ofbizUrl>">
    <input type="hidden" name="productId" value="${product.productId!}"/>
    <input type="hidden" name="PRODUCT_ID" value="${product.productId!}"/>
    <input type="hidden" name="PRODUCT_ID_TO" value=""/>
    <input type="hidden" name="PRODUCT_ASSOC_TYPE_ID" value="PRODUCT_VARIANT"/>
    <input type="hidden" name="FROM_DATE" value=""/>
    <input type="hidden" name="UPDATE_MODE" value="DELETE"/>
    <input type="hidden" name="useValues" value="true"/>
</form>
<form name="removeSelectable" action="<@ofbizUrl>updateProductQuickAdminDelFeatureTypes</@ofbizUrl>">
    <input type="hidden" name="productId" value="${product.productId!}"/>
    <input type="hidden" name="productFeatureTypeId" value=""/>
</form>

<@script>

function removeAssoc(productIdTo, fromDate) {
    if (confirm("Are you sure you want to remove the association of " + productIdTo + "?")) {
        document.removeAssocForm.PRODUCT_ID_TO.value = productIdTo;
        document.removeAssocForm.FROM_DATE.value = fromDate;
        document.removeAssocForm.submit();
    }
}

function removeSelectable(typeString, productFeatureTypeId, productId) {
    if (confirm("Are you sure you want to remove all the selectable features of type " + typeString + "?")) {
        document.removeSelectable.productId.value = productId;
        document.removeSelectable.productFeatureTypeId.value = productFeatureTypeId;
        document.removeSelectable.submit();
    }
}

function doPublish() {
    window.open('/ecommerce/control/product?product_id=${productId!}');
    document.publish.submit();
}

</@script>

<@section title="${uiLabelMap.PageTitleEditProductQuickAdmin}">
        <!-- Name update section -->
        <form action="<@ofbizUrl>updateProductQuickAdminName</@ofbizUrl>" method="post" name="editProduct">
          <@fields type="default-nolabels">
            <input type="hidden" name="productId" value="${productId!}"/>
            <#if (product.isVirtual)! == "Y">
                <input type="hidden" name="isVirtual" value="Y"/>
            </#if>
            <@field type="display">${productId!}</@field>
            <@field type="input" name="productName" size="40" maxlength="40" value="${product.productName!}" />
            <@field type="submit" text="${uiLabelMap.ProductUpdateName}" class="${styles.link_run_sys!} ${styles.action_update!}"/>
          </@fields>
        </form>
</@section>

<#if (product.isVirtual)! == "Y">
<@section title="${uiLabelMap.ProductSelectableFeatures}">
        <!-- ***************************************************** Selectable features section -->
    <@row>
      <@cell>   
        <form action="<@ofbizUrl>EditProductQuickAdmin</@ofbizUrl>" method="post" name="selectableFeatureTypeSelector">
            <input type="hidden" name="productId" value="${product.productId!}"/>
            <@field type="generic" label="${uiLabelMap.CommonType}">
                <select name="productFeatureTypeId" onchange="javascript:document.selectableFeatureTypeSelector.submit();">
                    <option value="~~any~~">${uiLabelMap.ProductAnyFeatureType}</option>
                    <#list featureTypes as featureType>
                        <#if (featureType.productFeatureTypeId)! == (productFeatureTypeId)!>
                            <#assign selected="selected"/>
                        <#else>
                            <#assign selected=""/>
                        </#if>
                        <option ${selected} value="${featureType.productFeatureTypeId!}">${featureType.get("description",locale)!}</option>
                    </#list>
                </select>
            </@field>
        </form>
      </@cell>
    </@row>
    <@row>
      <@cell>
        <form action="<@ofbizUrl>updateProductQuickAdminSelFeat</@ofbizUrl>" method="post" name="selectableFeature">
        <input type="hidden" name="productId" value="${product.productId!}"/>
        <input type="hidden" name="productFeatureTypeId" value="${(productFeatureTypeId)!}"/>
        <@table type="data-list" autoAltRows=true cellspacing="0"> <#-- orig: class="basic-table" -->
          <@thead>
            <@tr class="header-row">
                <@th>${uiLabelMap.ProductProductId}</@th>
                <@th>&nbsp;</@th>
                <@th>&nbsp;</@th>
                <@th>&nbsp;</@th>
                <@th>${uiLabelMap.ProductSRCH}</@th>
                <@th>${uiLabelMap.ProductDL}</@th>
            </@tr>
          </@thead>
        <#assign idx = 0>
        <#list productAssocs as productAssoc>
            <#assign assocProduct = productAssoc.getRelatedOne("AssocProduct", false)/>
            <@tr valign="middle">
                <@td nowrap="nowrap">
                <input type="hidden" name="productId${idx}" value="${assocProduct.productId!}"/>
                <a class="${styles.link_nav_info_id!}" href="<@ofbizUrl>EditProduct?productId=${assocProduct.productId}</@ofbizUrl>">${assocProduct.productId!}</a></@td>
                <@td nowrap="nowrap"><a class="${styles.link_nav_info_name!}" href="<@ofbizUrl>EditProduct?productId=${assocProduct.productId}</@ofbizUrl>">${assocProduct.internalName!}</a></@td>
                <@td colspan="2">
                    <input type="text" name="description${idx}" size="70" maxlength="100" value="${selFeatureDesc[assocProduct.productId]!}"/>
                </@td>
                <#assign checked=""/>
                <#if ((assocProduct.smallImageUrl! != "") && (assocProduct.smallImageUrl! == product.smallImageUrl!) &&
                        (assocProduct.smallImageUrl! != "") && (assocProduct.smallImageUrl! == product.smallImageUrl!)) >
                    <#assign checked = "checked=\"checked\""/>
                </#if>
                <@td><input type="radio" ${checked} name="useImages" value="${assocProduct.productId}"/></@td>
                <#assign fromDate = Static["org.ofbiz.base.util.UtilFormatOut"].encodeQueryValue(productAssoc.getTimestamp("fromDate").toString())/>
                <@td><a class="${styles.link_run_sys!} ${styles.action_remove!}" href="javascript:removeAssoc('${productAssoc.productIdTo}','${fromDate}');">x</a></@td>
            </@tr>
            <#assign idx = idx + 1>
        </#list>
          <@tfoot>
            <@tr>
                <@td colspan="2">&nbsp;</@td>
                <@td>
                    <@table type="data-list" cellspacing="0"> <#-- orig: class="basic-table" -->
                        <#list selectableFeatureTypes as selectableFeatureType>
                        <@tr><@td nowrap="nowrap"><a class="${styles.link_run_sys!} ${styles.action_remove!}" href="javascript:removeSelectable('${(selectableFeatureType.get("description",locale))!}','${selectableFeatureType.productFeatureTypeId}','${product.productId}')">x</a>
                            <a class="${styles.link_nav_info_desc!}" href="<@ofbizUrl>EditProductQuickAdmin?productFeatureTypeId=${(selectableFeatureType.productFeatureTypeId)!}&amp;productId=${product.productId!}</@ofbizUrl>">${(selectableFeatureType.get("description",locale))!}</a></@td></@tr>
                        </#list>
                    </@table>
                </@td>
                <@td align="right">
                    <input name="applyToAll" type="submit" value="${uiLabelMap.ProductAddSelectableFeature}" class="${styles.link_run_sys!} ${styles.action_update!}"/>
                </@td>
                <@td colspan="2"></@td>
            </@tr>
          </@tfoot>
        </@table>
        </form>
      </@cell>
    </@row>
</@section>
</#if>

<#if (product.isVariant)! == "Y">
    <@section title="${uiLabelMap.ProductDistinguishingFeatures}">
        <form action="<@ofbizUrl>updateProductQuickAdminDistFeat</@ofbizUrl>" method="post" name="distFeature">
            <input type="hidden" name="productId" value="${product.productId!}"/>
            <@table type="data-list" autoAltRows=true cellspacing="0"> <#-- orig: class="basic-table" -->
              <@thead>
                <@tr class="header-row">
                    <@th>${uiLabelMap.ProductProductId}</@th>
                </@tr>
              </@thead>
              <@tbody>
                <#assign idx=0/>
                <#list distinguishingFeatures as distinguishingFeature>
                <@tr valign="middle">
                    <@td><a href="<@ofbizUrl>quickAdminRemoveProductFeature?productId=${productId}&amp;productFeatureId=${distinguishingFeature.productFeatureId}</@ofbizUrl>" class="${styles.link_run_sys!} ${styles.action_remove!}">x</a>&nbsp;
                    ${distinguishingFeature.productFeatureId} ${productFeatureTypeLookup.get(distinguishingFeature.productFeatureId).get("description",locale)}: ${distinguishingFeature.get("description",locale)}
                    &nbsp;
                    </@td>
                </@tr>
                </#list>
              </@tbody>
            </@table>
        </form>
    </@section>
</#if>

<!-- ***************************************************** end Selectable features section -->
<@section title="${uiLabelMap.ProductShippingDimensionsAndWeights}">
        <!-- ***************************************************** Shipping dimensions section -->
        <form action="<@ofbizUrl>updateProductQuickAdminShipping</@ofbizUrl>" method="post" name="updateShipping">
            <input type="hidden" name="productId" value="${product.productId!}"/>
            <input type="hidden" name="heightUomId" value="LEN_in"/>
            <input type="hidden" name="widthUomId" value="LEN_in"/>
            <input type="hidden" name="depthUomId" value="LEN_in"/>
            <input type="hidden" name="weightUomId" value="WT_oz"/>
            <@table type="data-list" autoAltRows=true cellspacing="0"> <#-- orig: class="basic-table" -->
              <@thead>
                <@tr class="header-row">
                    <@th>${uiLabelMap.ProductProductHeight}</@th>
                    <@th>${uiLabelMap.ProductProductWidth}</@th>
                    <@th>${uiLabelMap.ProductProductDepth}</@th>
                    <@th>${uiLabelMap.ProductWeight}</@th>
                    <@th>${uiLabelMap.ProductFlOz}</@th>
                    <@th>${uiLabelMap.ProductML}</@th>
                    <@th>${uiLabelMap.ProductNtWt}</@th>
                    <@th>${uiLabelMap.ProductGrams}</@th>
                    <@th>${uiLabelMap.ProductHZ}</@th>
                 <@th>${uiLabelMap.ProductST}</@th>
                    <@th>${uiLabelMap.ProductTD}</@th>
                </@tr>
                </@thead>
        <#if (product.isVirtual)! == "Y">
            <#assign idx=0/>
            <#list assocProducts as assocProduct>
                <@tr valign="middle">
                    <@td><input type="text" name="productHeight${idx}" size="6" maxlength="20" value="${assocProduct.productHeight!}"/></@td>
                    <@td><input type="text" name="productWidth${idx}" size="6" maxlength="20" value="${assocProduct.productWidth!}"/></@td>
                    <@td><input type="text" name="productDepth${idx}" size="6" maxlength="20" value="${assocProduct.productDepth!}"/></@td>
                    <@td><input type="text" name="weight${idx}" size="6" maxlength="20" value="${assocProduct.weight!}"/></@td>
                    <@td><input type="text" name="~floz${idx}" size="6" maxlength="20" value="${featureFloz.get(assocProduct.productId)!}"/></@td>
                    <@td><input type="text" name="~ml${idx}" size="6" maxlength="20" value="${featureMl.get(assocProduct.productId)!}"/></@td>
                    <@td><input type="text" name="~ntwt${idx}" size="6" maxlength="20" value="${featureNtwt.get(assocProduct.productId)!}"/></@td>
                    <@td><input type="text" name="~grams${idx}" size="6" maxlength="20" value="${featureGrams.get(assocProduct.productId)!}"/></@td>
                    <@td><a class="${styles.link_nav_info_id!}" href="<@ofbizUrl>EditProductFeatures?productId=${assocProduct.productId}</@ofbizUrl>">${StringUtil.wrapString(featureHazmat.get(assocProduct.productId)!)}</a></@td>
                    <@td><a class="${styles.link_nav_info_id!}" href="<@ofbizUrl>EditProduct?productId=${assocProduct.productId}</@ofbizUrl>">${StringUtil.wrapString(featureSalesThru.get(assocProduct.productId)!)}</a></@td>
                    <@td><a class="${styles.link_nav_info_id!}" href="<@ofbizUrl>EditProductAssoc?productId=${assocProduct.productId}</@ofbizUrl>">${StringUtil.wrapString(featureThruDate.get(assocProduct.productId)!)}</a></@td>
                </@tr>
                <#assign idx = idx + 1/>
            </#list>
              <@tfoot>
                <@tr>
                    <@td colspan="11" align="right"><input name="applyToAll" type="submit" value="${uiLabelMap.ProductApplyToAll}" class="${styles.link_run_sys!} ${styles.action_update!}"/>
                    &nbsp;<input name="updateShipping" type="submit" value="${uiLabelMap.ProductUpdateShipping}" class="${styles.link_run_sys!} ${styles.action_update!}"/></@td>
                </@tr>
              </@tfoot>
        <#else>
                <@tr>
                    <@td><input type="text" name="productHeight" size="6" maxlength="20" value="${product.productHeight!}" /></@td>
                    <@td><input type="text" name="productWidth" size="6" maxlength="20" value="${product.productWidth!}" /></@td>
                    <@td><input type="text" name="productDepth" size="6" maxlength="20" value="${product.productDepth!}" /></@td>
                    <@td><input type="text" name="weight" size="6" maxlength="20" value="${product.weight!}" /></@td>
                    <@td><input type="text" name="~floz" size="6" maxlength="20" value="${floz!}" /></@td>
                    <@td><input type="text" name="~ml" size="6" maxlength="20" value="${ml!}" /></@td>
                    <@td><input type="text" name="~ntwt" size="6" maxlength="20" value="${ntwt!}" /></@td>
                    <@td><input type="text" name="~grams" size="6" maxlength="20" value="${grams!}" /></@td>
                    <@td><a class="${styles.link_nav_info_value!}" href="<@ofbizUrl>EditProductFeatures?productId=${product.productId}</@ofbizUrl>">${StringUtil.wrapString(hazmat!)}</a></@td>
                    <@td><a class="${styles.link_nav_info_date!}" href="<@ofbizUrl>EditProduct?productId=${product.productId}</@ofbizUrl>">${StringUtil.wrapString(salesthru!)}</a></@td>
                    <@td><a class="${styles.link_nav_info_date!}" href="<@ofbizUrl>EditProductAssoc?productId=${product.productId}</@ofbizUrl>">${StringUtil.wrapString(thrudate!)}</a></@td>
                </@tr>
              <@tfoot>
                <@tr>
                    <@td colspan="10" align="right"><input type="submit" value="${uiLabelMap.ProductUpdateShipping}" class="${styles.link_run_sys!} ${styles.action_update!}" /></@td>
                </@tr>
              </@tfoot>
        </#if>

            </@table>
        </form>
    <!--  **************************************************** end - Shipping dimensions section -->
</@section>

<@section title="${uiLabelMap.ProductStandardFeatures}">
        <!--  **************************************************** Standard Features section -->
    <@row>
      <@cell>
        <#if addedFeatureTypeIds?has_content || standardFeatureAppls?has_content>
        <@table type="generic" class="${styles.table_basic!}" cellspacing="0"> <#-- orig: class="basic-table" -->
        <@tr>
        <@td>
            <#if addedFeatureTypeIds?has_content>
            <form method="post" action="<@ofbizUrl>quickAdminApplyFeatureToProduct</@ofbizUrl>" name="addFeatureById">
            <input type="hidden" name="productId" value="${product.productId!}"/>
            <input type="hidden" name="productFeatureApplTypeId" value="STANDARD_FEATURE"/>
            <input type="hidden" name="fromDate" value="${nowTimestampString}"/>
            <@table type="data-list" autoAltRows=true cellspacing="0"> <#-- orig: class="basic-table" -->
                <#list addedFeatureTypeIds as addedFeatureTypeId>
                    <@tr valign="middle">
                        <@td align="right">${addedFeatureTypes.get(addedFeatureTypeId).description}</@td>
                        <@td>
                            <select name="productFeatureId">
                                <option value="~~any~~">${uiLabelMap.ProductAnyFeatureType}</option>
                            <#list featuresByType.get(addedFeatureTypeId) as feature>
                                <option value="${feature.getString("productFeatureId")}">${feature.description}</option>
                            </#list>
                            </select>
                        </@td>
                    </@tr>
                </#list>
                <@tfoot>
                <@tr><@td colspan="2" align="right"><input type="submit" value="${uiLabelMap.ProductAddFeatures}" class="${styles.link_run_sys!} ${styles.action_add!}"/></@td></@tr>
                </@tfoot>
            </@table>
            </form>
            </#if>
        </@td>
        <@td width="20">&nbsp;</@td>
        <@td valign="top">
            <#if standardFeatureAppls?has_content>
            <@table type="data-list" autoAltRows=true cellspacing="0"> <#-- orig: class="basic-table" -->
                <#list standardFeatureAppls as standardFeatureAppl>
                    <#assign featureId = standardFeatureAppl.productFeatureId/>
                    <@tr valign="middle">
                        <@td colspan="2"><a href="<@ofbizUrl>quickAdminRemoveFeatureFromProduct?productId=${standardFeatureAppl.productId!}&amp;productFeatureId=${featureId!}&amp;fromDate=${(standardFeatureAppl.fromDate)!}</@ofbizUrl>" class="${styles.link_run_sys!} ${styles.action_remove!}">x</a>
                        ${productFeatureTypeLookup.get(featureId).description}: ${standardFeatureLookup.get(featureId).description}
                        </@td>
                    </@tr>
                </#list>
            </@table>
            </#if>
        </@td>
        </@tr>
        </@table>

        </#if>
      </@cell>
    </@row>
    <@row>
      <@cell>
        <form action="<@ofbizUrl>EditProductQuickAdmin</@ofbizUrl>">
            <input type="hidden" name="productFeatureTypeId" value="${(productFeatureTypeId)!}"/>
            <input type="hidden" name="productId" value="${product.productId!}"/>
            <@field type="generic" label="${uiLabelMap.ProductFeatureTypes}">
                <select multiple="multiple" name="addFeatureTypeId">
                    <#list featureTypes as featureType>
                        <option value="${featureType.productFeatureTypeId!}">${featureType.get("description",locale)!}</option>
                    </#list>
                </select>
            </@field>
            <@field type="submit" text="${uiLabelMap.ProductAddFeatureType}" class="${styles.link_run_sys!} ${styles.action_add!}"/>
        </form>
      </@cell>
    </@row>
        <!--  **************************************************** end - Standard Features section -->
</@section>

<@section title="${uiLabelMap.ProductCategories}">
        <!--  **************************************************** Categories section -->
    <@row>
      <@cell>
        <form action="<@ofbizUrl>quickAdminAddCategories</@ofbizUrl>">
          <@fields type="default-nolabels">
            <input type="hidden" name="fromDate" value="${nowTimestampString}"/>
            <input type="hidden" name="productId" value="${product.productId!}"/>
            <@field type="generic">
                <select multiple="multiple" name="categoryId">
                  <#list allCategories as category>
                    <option value="${category.productCategoryId!}">${category.description!} ${category.productCategoryId}</option>
                  </#list>
                </select>
            </@field>
            <@field type="submit" text="${uiLabelMap.ProductUpdateCategories}" class="${styles.link_run_sys!} ${styles.action_update!}"/>
          </@fields>
        </form>
      </@cell>
    </@row>
    <@row>
      <@cell>
          <@table type="data-list" autoAltRows=true cellspacing="0"> <#-- orig: class="basic-table" -->
            <#list productCategoryMembers as prodCatMemb>
                <#assign prodCat = prodCatMemb.getRelatedOne("ProductCategory", false)/>
                <@tr valign="middle">
                    <@td colspan="2">
                      <form name="quickAdminRemoveProductFromCategory_${prodCatMemb_index}" action="<@ofbizUrl>quickAdminRemoveProductFromCategory</@ofbizUrl>" method="post">
                        <input type="hidden" name="productId" value="${prodCatMemb.productId!}" />
                        <input type="hidden" name="productCategoryId" value="${prodCatMemb.productCategoryId}" />
                        <input type="hidden" name="fromDate" value="${(prodCatMemb.fromDate)!}" />
                        <a href="javascript:document.quickAdminRemoveProductFromCategory_${prodCatMemb_index}.submit();" class="${styles.link_run_sys!} ${styles.action_remove!}">x</a>
                        ${prodCat.description!} ${prodCat.productCategoryId}
                      </form>
                    </@td>
                </@tr>
            </#list>
          </@table>
      </@cell>
    </@row>
        <!--  **************************************************** end - Categories section -->
</@section>

<@section title="${uiLabelMap.ProductPublishAndView}">
    <!--  **************************************************** publish section -->
  <#if (showPublish == "true")>
    <@row>
      <@cell>
        <form action="<@ofbizUrl>quickAdminAddCategories</@ofbizUrl>" name="publish">
          <@fields type="default-nolabels">
            <input type="hidden" name="productId" value="${product.productId!}"/>
            <input type="hidden" name="categoryId" value="${allCategoryId!}"/>
            <@field type="generic">
                <@htmlTemplate.renderDateTimeField name="fromDate" event="" action="" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" value="" size="25" maxlength="30" id="fromDate1" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
            </@field>
            <@field type="submit" submitType="input-button" text="${uiLabelMap.ProductPublishAndView}" onClick="doPublish();"/>
          </@fields>
        </form>
      </@cell>
    </@row>
  <#else>
    <@row>
      <@cell>
        <form  action="<@ofbizUrl>quickAdminUnPublish</@ofbizUrl>" name="unpublish">
          <@fields type="default-nolabels">
            <input type="hidden" name="productId" value="${product.productId!}"/>
            <input type="hidden" name="productCategoryId" value="${allCategoryId!}"/>
            <@field type="generic">
                <@htmlTemplate.renderDateTimeField name="thruDate" event="" action="" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" value="" size="25" maxlength="30" id="thruDate1" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
            </@field>
            <@field type="submit" text="${uiLabelMap.ProductRemoveFromSite}" class="${styles.link_run_sys!} ${styles.action_remove!}"/>
          </@fields>
        </form>
      </@cell>
    </@row>
  </#if>
    <!--  **************************************************** end - publish section -->
</@section>

<#else>
  <@commonMsg type="error">${uiLabelMap.ProductProductNotFound} ${productId!}</@commonMsg>
</#if>
