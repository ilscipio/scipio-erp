<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<@section title=uiLabelMap.ProductOverrideSimpleFields>
        <form action="<@ofbizUrl>updateCategoryContent</@ofbizUrl>" method="post" name="categoryForm">
            <input type="hidden" name="productCategoryId" value="${productCategoryId!}" />
                <@field type="select" label=uiLabelMap.ProductProductCategoryType name="productCategoryTypeId" size="1">
                    <option value="">&nbsp;</option>
                    <#list productCategoryTypes as productCategoryTypeData>
                        <option<#if productCategory?has_content><#if productCategory.productCategoryTypeId==productCategoryTypeData.productCategoryTypeId> selected="selected"</#if></#if> value="${productCategoryTypeData.productCategoryTypeId}">${productCategoryTypeData.get("description",locale)}</option>
                    </#list>
                </@field>
                <@field type="input" label=uiLabelMap.ProductName value=((productCategory.categoryName)!) name="categoryName" size="60" maxlength="60"/>
                <@field type="textarea" label=uiLabelMap.ProductCategoryDescription name="description" cols="60" rows="2">${(productCategory.description)!}</@field>
                <@field type="textarea" label=uiLabelMap.ProductLongDescription name="longDescription" cols="60" rows="7">${(productCategory.longDescription)!}</@field>
                <#if productCategory?has_content>
                    <#assign fieldValue = productCategory.detailScreen!>
                <#else>
                    <#assign fieldValue = "">
                </#if>
                <#-- SCIPIO: Now points to shop -->
                <@field type="input" label=uiLabelMap.ProductDetailScreen name="detailScreen" size="60" maxlength="250" value=fieldValue tooltip="${rawLabel('ProductDefaultsTo')} \"categorydetail\", ${rawLabel('ProductDetailScreenMessage')}: \"component://shop/widget/CatalogScreens.xml#categorydetail\"" />
                <@field type="submit" name="Update" text=uiLabelMap.CommonUpdate class="+${styles.link_run_sys!} ${styles.action_update!}" />
        </form>
</@section>