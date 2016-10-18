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
<#include "catalogcommon.ftl">

<#macro paginationControls>
    <#assign viewIndexMax = Static["java.lang.Math"].ceil((listSize - 1)?double / viewSize?double)>
      <#if (viewIndexMax?int > 0)>
        <div class="product-prevnext">
            <#-- Start Page Select Drop-Down -->
            <select name="pageSelect" onchange="window.location=this[this.selectedIndex].value;">
                <option value="#">${uiLabelMap.CommonPage} ${viewIndex?int + 1} ${uiLabelMap.CommonOf} ${viewIndexMax + 1}</option>
                <#list 0..viewIndexMax as curViewNum>
                     <option value="<@ofbizUrl>category/~category_id=${productCategoryId}/~searchCategoryId=${currentSearchCategory.productCategoryId}/~VIEW_SIZE=${viewSize}/~VIEW_INDEX=${curViewNum?int}/~clearSearch=N</@ofbizUrl>">${uiLabelMap.CommonGotoPage} ${curViewNum + 1}</option>
                </#list>
            </select>
            <#-- End Page Select Drop-Down -->
            <#if (0 < viewIndex?int)>
                <a href="<@ofbizUrl>category/~category_id=${productCategoryId}/~searchCategoryId=${currentSearchCategory.productCategoryId}/~VIEW_SIZE=${viewSize}/~VIEW_INDEX=${viewIndex?int - 1}/~clearSearch=N</@ofbizUrl>" class="${styles.link_nav!}">${uiLabelMap.CommonPrevious}</a> |
            </#if>
            <#if ((listSize?int - viewSize?int) > 0)>
                <span>${lowIndex + 1} - ${highIndex} ${uiLabelMap.CommonOf} ${listSize}</span>
            </#if>
            <#if highIndex?int < listSize?int>
             | <a href="<@ofbizUrl>category/~category_id=${productCategoryId}/~searchCategoryId=${currentSearchCategory.productCategoryId}/~VIEW_SIZE=${viewSize}/~VIEW_INDEX=${viewIndex?int + 1}/~clearSearch=N</@ofbizUrl>" class="${styles.link_nav!}">${uiLabelMap.CommonNext}</a>
            </#if>
        </div>
    </#if>
</#macro>


<#if productCategory??>
    <#assign categoryName = categoryContentWrapper.get("CATEGORY_NAME")!/>
    <#assign categoryDescription = categoryContentWrapper.get("DESCRIPTION")!/>
    <#if categoryName?has_content>
        <@heading>${categoryName}</@heading>
    </#if>
    <#if categoryDescription?has_content>
        <p>${categoryDescription}</p>
    </#if>
    <#assign longDescription = htmlContentString(categoryContentWrapper.get("LONG_DESCRIPTION")!)/>
    <#assign categoryImageUrl = categoryContentWrapper.get("CATEGORY_IMAGE_URL", "url")!/>
    <#if categoryImageUrl?string?has_content || longDescription?has_content>
      <div>
        <#if categoryImageUrl?string?has_content>
          <#assign height=100/>
          <img src="<@catalogContentUrl>${categoryImageUrl}</@catalogContentUrl>" vspace="5" hspace="5" border="1" height="${height}" align="left" alt="" />
        </#if>
        <#if longDescription?has_content>
          ${longDescription}
        </#if>
      </div>
  </#if>
</#if>

<#if productIds?has_content>
    <@paginationControls/>
    <#assign numCol = numCol?default(1)>
    <#assign numCol = numCol?number>
    <#assign tabCol = 1>
    <div<#if categoryImageUrl?string?has_content> style="position: relative; margin-top: ${height}px;"</#if> class="productsummary-container<#if (numCol?int > 1)> matrix</#if>">

      <@table type="generic" open=(numCol?int > 1) close=(numCol?int > 1)>

        <#list productIds as productId>
          <#if (numCol?int == 1)>
            <@render resource=productsummaryScreen reqAttribs={"optProductId":productId, "listIndex":productId_index}/>
          <#else>
              <#if (tabCol?int == 1)><@tr open=true close=false /></#if>
                  <@td>
                      <@render resource=productsummaryScreen reqAttribs={"optProductId":productId, "listIndex":productId_index}/>
                  </@td>
              <#if (tabCol?int == numCol)><@tr close=true open=false /></#if>
              <#assign tabCol = tabCol+1><#if (tabCol?int > numCol)><#assign tabCol = 1></#if>
           </#if>
        </#list>
      </@table>
    </div>
    <@paginationControls/>
<#else>
    <hr />
    <@commonMsg type="result-norecord">${uiLabelMap.ProductNoProductsInThisCategory}</@commonMsg>
</#if>
