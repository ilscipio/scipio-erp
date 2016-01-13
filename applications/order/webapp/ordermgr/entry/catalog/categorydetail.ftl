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
<@script>
    function callDocumentByPaginate(info) {
        var str = info.split('~');
        var checkUrl = '<@ofbizUrl>categoryAjaxFired</@ofbizUrl>';
        if(checkUrl.search("http"))
            var ajaxUrl = '<@ofbizUrl>categoryAjaxFired</@ofbizUrl>';
        else
            var ajaxUrl = '<@ofbizUrl>categoryAjaxFiredSecure</@ofbizUrl>';
            
        //jQuerry Ajax Request
        jQuery.ajax({
            url: ajaxUrl,
            type: 'POST',
            data: {"category_id" : str[0], "VIEW_SIZE" : str[1], "VIEW_INDEX" : str[2]},
            error: function(msg) {
                alert("An error occurred loading content! : " + msg);
            },
            success: function(msg) {
                jQuery('#div3').html(msg);
            }
        });
     }
</@script>

<#macro paginationControls>
    <#assign viewIndexMax = Static["java.lang.Math"].ceil((listSize)?double / viewSize?double)>
      <#if (viewIndexMax?int > 0)>
        <div class="product-prevnext">
            <select name="pageSelect" onchange="callDocumentByPaginate(this[this.selectedIndex].value);">
                <option value="#">${uiLabelMap.CommonPage} ${viewIndex?int + 1} ${uiLabelMap.CommonOf} ${viewIndexMax}</option>
                <#if (viewIndex?int > 1)>
                    <#list 1..viewIndexMax as curViewNum>
                         <option value="${productCategoryId}~${viewSize}~${curViewNum-1?int}">${uiLabelMap.CommonGotoPage} ${curViewNum}</option>
                    </#list>
                </#if>
            </select>
            <#-- End Page Select Drop-Down -->
            <#if (viewIndex?int > 0)>
                <a href="javascript: void(0);" onclick="callDocumentByPaginate('${productCategoryId}~${viewSize}~${viewIndex?int - 1}');" class="${styles.link_nav!}">${uiLabelMap.CommonPrevious}</a> |
            </#if>
            <#if ((listSize?int - viewSize?int) > 0)>
                <span>${lowIndex} - ${highIndex} ${uiLabelMap.CommonOf} ${listSize}</span>
            </#if>
            <#if (highIndex?int < listSize?int)>
             | <a href="javascript: void(0);" onclick="callDocumentByPaginate('${productCategoryId}~${viewSize}~${viewIndex?int + 1}');" class="${styles.link_nav!}">${uiLabelMap.CommonNext}</a>
            </#if>
        </div>
    </#if>
</#macro>


<#if productCategory??>
    <#assign categoryName = categoryContentWrapper.get("CATEGORY_NAME", "html")!/>
    <#assign categoryDescription = categoryContentWrapper.get("DESCRIPTION", "html")!/>
    <#if categoryName?has_content>
        <@heading>${categoryName}</@heading>
    </#if>
    <#if categoryDescription?has_content>
        <@heading>${categoryDescription}</@heading>
    </#if>
    <#if hasQuantities??>
      <form method="post" action="<@ofbizUrl>addCategoryDefaults<#if requestAttributes._CURRENT_VIEW_??>/${requestAttributes._CURRENT_VIEW_}</#if></@ofbizUrl>" name="thecategoryform">
        <input type='hidden' name='add_category_id' value='${productCategory.productCategoryId}'/>
        <#if requestParameters.product_id??><input type='hidden' name='product_id' value='${requestParameters.product_id}'/></#if>
        <#if requestParameters.category_id??><input type='hidden' name='category_id' value='${requestParameters.category_id}'/></#if>
        <#if requestParameters.VIEW_INDEX??><input type='hidden' name='VIEW_INDEX' value='${requestParameters.VIEW_INDEX}'/></#if>
        <#if requestParameters.SEARCH_STRING??><input type='hidden' name='SEARCH_STRING' value='${requestParameters.SEARCH_STRING}'/></#if>
        <#if requestParameters.SEARCH_CATEGORY_ID??><input type='hidden' name='SEARCH_CATEGORY_ID' value='${requestParameters.SEARCH_CATEGORY_ID}'/></#if>
        <a href="javascript:document.thecategoryform.submit()" class="${styles.link_action_long!}"><span style="white-space: nowrap;">${uiLabelMap.ProductAddProductsUsingDefaultQuantities}</span></a>
      </form>
    </#if>
    <#if searchInCategory?default("Y") == "Y">
        <a href="<@ofbizUrl>advancedsearch?SEARCH_CATEGORY_ID=${productCategory.productCategoryId}</@ofbizUrl>" class="${styles.link_action!}">${uiLabelMap.ProductSearchInCategory}</a>
    </#if>
    <#assign longDescription = categoryContentWrapper.get("LONG_DESCRIPTION", "html")!/>
    <#assign categoryImageUrl = categoryContentWrapper.get("CATEGORY_IMAGE_URL", "url")!/>
    <#if categoryImageUrl?string?has_content || longDescription?has_content>
      <div>
        <#if categoryImageUrl?string?has_content>
          <#assign height=100/>
          <img src='<@ofbizContentUrl>${categoryImageUrl}</@ofbizContentUrl>' vspace='5' hspace='5' align='left' class='cssImgLarge' />
        </#if>
        <#if longDescription?has_content>
          ${longDescription}
        </#if>
      </div>
  </#if>
</#if>

<#if productCategoryLinkScreen?has_content && productCategoryLinks?has_content>
    <div class="productcategorylink-container">
        <#list productCategoryLinks as productCategoryLink>
            ${setRequestAttribute("productCategoryLink",productCategoryLink)}
            ${screens.render(productCategoryLinkScreen)}
        </#list>
    </div>
</#if>

<#if productCategoryMembers?has_content>
    <#-- Pagination -->
    <#if paginateEcommerceStyle??>
        <@paginationControls/>
    <#else>
        <#include "component://common/webcommon/includes/htmlTemplate.ftl"/>
        <#assign commonUrl = "category?category_id="+ (parameters.category_id!) + "&"/>
        <#--assign viewIndex = viewIndex - 1/-->
        <#assign viewIndexFirst = 0/>
        <#assign viewIndexPrevious = viewIndex - 1/>
        <#assign viewIndexNext = viewIndex + 1/>
        <#assign viewIndexLast = Static["org.ofbiz.base.util.UtilMisc"].getViewLastIndex(listSize, viewSize) />
        <#assign messageMap = Static["org.ofbiz.base.util.UtilMisc"].toMap("lowCount", lowIndex, "highCount", highIndex, "total", listSize)/>
        <#assign commonDisplaying = Static["org.ofbiz.base.util.UtilProperties"].getMessage("CommonUiLabels", "CommonDisplaying", messageMap, locale)/>
        <@nextPrev position="top" commonUrl=commonUrl ajaxEnabled=false javaScriptEnabled=false paginateStyle="nav-pager" paginateFirstStyle="nav-first" viewIndex=viewIndex highIndex=highIndex listSize=listSize viewSize=viewSize ajaxFirstUrl="" firstUrl="" paginateFirstLabel="" paginatePreviousStyle="nav-previous" ajaxPreviousUrl="" previousUrl="" paginatePreviousLabel="" pageLabel="" ajaxSelectUrl="" selectUrl="" ajaxSelectSizeUrl="" selectSizeUrl="" commonDisplaying=commonDisplaying paginateNextStyle="nav-next" ajaxNextUrl="" nextUrl="" paginateNextLabel="" paginateLastStyle="nav-last" ajaxLastUrl="" lastUrl="" paginateLastLabel="" paginateViewSizeLabel="" />
    </#if>
      <#assign numCol = numCol?default(1)>
      <#assign numCol = numCol?number>
      <#assign tabCol = 1>
      <div<#if categoryImageUrl?string?has_content> style="position: relative; margin-top: ${height}px;"</#if>
          class="productsummary-container<#if (numCol?int > 1)> matrix</#if>">
      <@table type="data-list" autoAltRows=false open=(numCol?int > 1) close=(numCol?int > 1)> <#-- orig: class="" -->
        <#list productCategoryMembers as productCategoryMember>
          <#if (numCol?int == 1)>
            ${setRequestAttribute("optProductId", productCategoryMember.productId)}
            ${setRequestAttribute("productCategoryMember", productCategoryMember)}
            ${setRequestAttribute("listIndex", productCategoryMember_index)}
            ${screens.render(productsummaryScreen)}
          <#else>
              <@tr open=(tabCol?int = 1) close=(tabCol?int = 1)>
                  <@td>
                      ${setRequestAttribute("optProductId", productCategoryMember.productId)}
                      ${setRequestAttribute("productCategoryMember", productCategoryMember)}
                      ${setRequestAttribute("listIndex", productCategoryMember_index)}
                      ${screens.render(productsummaryScreen)}
                  </@td>
              </@tr>
              <#assign tabCol = tabCol+1><#if (tabCol?int > numCol)><#assign tabCol = 1></#if>
           </#if>
        </#list>
      </@table>
      </div>
    <#if paginateEcommerceStyle??>
        <@paginationControls/>
    <#else>
        <@nextPrev position="bottom" commonUrl=commonUrl ajaxEnabled=false javaScriptEnabled=false paginateStyle="nav-pager" paginateFirstStyle="nav-first" viewIndex=viewIndex highIndex=highIndex listSize=listSize viewSize=viewSize ajaxFirstUrl="" firstUrl="" paginateFirstLabel="" paginatePreviousStyle="nav-previous" ajaxPreviousUrl="" previousUrl="" paginatePreviousLabel="" pageLabel="" ajaxSelectUrl="" selectUrl="" ajaxSelectSizeUrl="" selectSizeUrl="" commonDisplaying=commonDisplaying paginateNextStyle="nav-next" ajaxNextUrl="" nextUrl="" paginateNextLabel="" paginateLastStyle="nav-last" ajaxLastUrl="" lastUrl="" paginateLastLabel="" paginateViewSizeLabel="" />
    </#if>
<#else>
    <hr />
    <@resultMsg>${uiLabelMap.ProductNoProductsInThisCategory}</@resultMsg>
</#if>
