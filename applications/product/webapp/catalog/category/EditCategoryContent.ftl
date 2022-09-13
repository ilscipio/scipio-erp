<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<@section title=uiLabelMap.ProductOverrideSimpleFields>
    <#if productCategory??>
        <@script>
            function insertNowTimestamp(field) {
                eval('document.productForm.' + field + '.value="${nowTimestampString}";');
            };
            function insertImageName(type,nameValue) {
                eval('document.productForm.' + type + 'ImageUrl.value=nameValue;');
            };
        </@script>

        <form action="<@pageUrl>updateCategoryContent</@pageUrl>" method="post" name="categoryForm">
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

                <#assign labelDetail>
                    <#if (productCategory.smallImageUrl)??>
                        <a href="<@contentUrl>${(productCategory.smallImageUrl)!}</@contentUrl>" target="_blank"><img alt="Small Image" src="<@contentUrl>${(productCategory.smallImageUrl)!}</@contentUrl>" class="cssImgSmall"/></a>
                    </#if>
                </#assign>
                <@field type="generic" label=uiLabelMap.ProductSmallImage labelDetail=labelDetail>
                    <@field type="input" name="smallImageUrl" value=(productCategory.smallImageUrl)!'' size="60" maxlength="255"/>
                    <#if productCategoryId?has_content>
                        <div>
                            <span>${uiLabelMap.ProductInsertDefaultImageUrl}: </span>
                            <a href="javascript:insertImageName('small','${imageNameSmall}.jpg');" class="${styles.link_run_local!} ${styles.action_add!}">.jpg</a>
                            <a href="javascript:insertImageName('small','${imageNameSmall}.gif');" class="${styles.link_run_local!} ${styles.action_add!}">.gif</a>
                            <a href="javascript:insertImageName('small','');" class="${styles.link_run_local!} ${styles.action_clear!}">${uiLabelMap.CommonClear}</a>
                        </div>
                    </#if>
                </@field>
                <#assign labelDetail>
                    <#if (productCategory.mediumImageUrl)??>
                        <a href="<@contentUrl>${productCategory.mediumImageUrl}</@contentUrl>" target="_blank"><img alt="Medium Image" src="<@contentUrl>${productCategory.mediumImageUrl}</@contentUrl>" class="cssImgSmall"/></a>
                    </#if>
                </#assign>
                <@field type="generic" label=uiLabelMap.ProductMediumImage labelDetail=labelDetail>
                    <@field type="input" name="mediumImageUrl" value=(productCategory.mediumImageUrl)!'' size="60" maxlength="255"/>
                    <#if productCategoryId?has_content>
                        <div>
                            <span>${uiLabelMap.ProductInsertDefaultImageUrl}: </span>
                            <a href="javascript:insertImageName('medium','${imageNameMedium}.jpg');" class="${styles.link_run_local!} ${styles.action_add!}">.jpg</a>
                            <a href="javascript:insertImageName('medium','${imageNameMedium}.gif');" class="${styles.link_run_local!} ${styles.action_add!}">.gif</a>
                            <a href="javascript:insertImageName('medium','');" class="${styles.link_run_local!} ${styles.action_clear!}">${uiLabelMap.CommonClear}</a>
                        </div>
                    </#if>
                </@field>
                <#assign labelDetail>
                    <#if (productCategory.largeImageUrl)??>
                        <a href="<@contentUrl>${productCategory.largeImageUrl}</@contentUrl>" target="_blank"><img alt="Large Image" src="<@contentUrl>${productCategory.largeImageUrl}</@contentUrl>" class="cssImgSmall"/></a>
                    </#if>
                </#assign>
                <@field type="generic" label=uiLabelMap.ProductLargeImage labelDetail=labelDetail>
                    <@field type="input" name="largeImageUrl" value=(productCategory.largeImageUrl)!'' size="60" maxlength="255"/>
                    <#if productCategoryId?has_content>
                        <div>
                            <span>${uiLabelMap.ProductInsertDefaultImageUrl}: </span>
                            <a href="javascript:insertImageName('large','${imageNameLarge}.jpg');" class="${styles.link_run_local!} ${styles.action_add!}">.jpg</a>
                            <a href="javascript:insertImageName('large','${imageNameLarge}.gif');" class="${styles.link_run_local!} ${styles.action_add!}">.gif</a>
                            <a href="javascript:insertImageName('large','');" class="${styles.link_run_local!} ${styles.action_clear!}">${uiLabelMap.CommonClear}</a>
                        </div>
                    </#if>
                </@field>
                <#assign labelDetail>
                    <#if (productCategory.detailImageUrl)??>
                        <a href="<@contentUrl>${productCategory.detailImageUrl}</@contentUrl>" target="_blank"><img alt="Detail Image" src="<@contentUrl>${productCategory.detailImageUrl}</@contentUrl>" class="cssImgSmall"/></a>
                    </#if>
                </#assign>
                <@field type="generic" label=uiLabelMap.ProductDetailImage labelDetail=labelDetail>
                    <@field type="input" name="detailImageUrl" value=(productCategory.detailImageUrl)!'' size="60" maxlength="255"/>
                    <#if productCategoryId?has_content>
                        <div>
                            <span>${uiLabelMap.ProductInsertDefaultImageUrl}: </span>
                            <a href="javascript:insertImageName('detail','${imageNameDetail}.jpg');" class="${styles.link_run_local!} ${styles.action_add!}">.jpg</a>
                            <a href="javascript:insertImageName('detail','${imageNameDetail}.gif');" class="${styles.link_run_local!} ${styles.action_add!}">.gif</a>
                            <a href="javascript:insertImageName('detail','');" class="${styles.link_run_local!} ${styles.action_clear!}">${uiLabelMap.CommonClear}</a>
                        </div>
                    </#if>
                </@field>
                <#assign labelDetail>
                    <#if (productCategory.originalImageUrl)??>
                        <a href="<@contentUrl>${productCategory.originalImageUrl}</@contentUrl>" target="_blank"><img alt="Original Image" src="<@contentUrl>${productCategory.originalImageUrl}</@contentUrl>" class="cssImgSmall"/></a>
                    </#if>
                </#assign>
                <@field type="generic" label=uiLabelMap.ProductOriginalImage labelDetail=labelDetail>
                    <@field type="input" name="originalImageUrl" value=(productCategory.originalImageUrl)!'' size="60" maxlength="255"/>
                    <#if productCategoryId?has_content>
                        <div>
                            <span>${uiLabelMap.ProductInsertDefaultImageUrl}: </span>
                            <a href="javascript:insertImageName('original','${imageNameOriginal}.jpg');" class="${styles.link_run_local!} ${styles.action_add!}">.jpg</a>
                            <a href="javascript:insertImageName('original','${imageNameOriginal}.gif');" class="${styles.link_run_local!} ${styles.action_add!}">.gif</a>
                            <a href="javascript:insertImageName('original','');" class="${styles.link_run_local!} ${styles.action_clear!}">${uiLabelMap.CommonClear}</a>
                        </div>
                    </#if>
                </@field>

                <@field type="submit" name="Update" text=uiLabelMap.CommonUpdate class="+${styles.link_run_sys!} ${styles.action_update!}" />
        </form>
    </#if>
</@section>