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

<#if productCategory?has_content>
  <@section>
    <@row>
      <@cell columns=6>
        <@heading>${productCategory.categoryName!}</@heading>
      </@cell>
      <@cell columns=6 class="+${styles.text_right!}">
        <form name="choosequickaddform" method="post" action="<@ofbizUrl>quickadd</@ofbizUrl>">
        <@fields type="default-nolabelarea">
          <@field type="select" name="category_id">
            <option value="${productCategory.productCategoryId}">${productCategory.categoryName!}</option>
            <option value="${productCategory.productCategoryId}">--</option>
            <#list quickAddCats as quickAddCatalogId>
              <#assign loopCategory = delegator.findOne("ProductCategory", {"productCategoryId":quickAddCatalogId}, true)>
              <#if loopCategory?has_content>
                <option value="${quickAddCatalogId}">${loopCategory.categoryName!}</option>
              </#if>
            </#list>
          </@field>
          <@field type="submit" submitType="link" href="javascript:document.choosequickaddform.submit()" class="+${styles.link_run_session!} ${styles.action_select!}" text=uiLabelMap.ProductChooseQuickAddCategory />
        </@fields>
        </form>
      </@cell>
    </@row>
    <#if productCategory.categoryImageUrl?? || productCategory.longDescription??>
      <hr class="sepbar"/>
      <@row>
        <@cell>
            <#if productCategory.categoryImageUrl??>
              <img src="<@ofbizContentUrl>${productCategory.categoryImageUrl}</@ofbizContentUrl>" vspace="5" hspace="5" class="cssImgLarge" alt="" />
            </#if>
            ${productCategory.longDescription!}
        </@cell>
      </@row>
    </#if>
  </@section>
</#if>

<#if productCategoryMembers?? && (0 < productCategoryMembers?size)>
  <@section>
    <@row>
      <@cell>
  <form method="post" action="<@ofbizUrl>addtocartbulk</@ofbizUrl>" name="bulkaddform">
  <@fields type="default-nolabelarea">
    <input type="hidden" name="category_id" value="${categoryId}" />
    <@field type="submit" submitType="link" href="javascript:document.bulkaddform.submit()" class="${styles.link_run_session!} ${styles.action_add!}" text=uiLabelMap.OrderAddAllToCart />
    <@row>
      <@cell class="+quickaddtable">
      <#list productCategoryMembers as productCategoryMember>
        <#assign product = productCategoryMember.getRelatedOne("Product", true)>
          <@section>
            <@render resource=quickaddsummaryScreen reqAttribs={"optProductId":productCategoryMember.productId}/>
          </@section>
      </#list>
      </@cell>
    </@row>
    <@field type="submit" submitType="link" href="javascript:document.bulkaddform.submit()" class="${styles.link_run_session!} ${styles.action_add!}" text=uiLabelMap.OrderAddAllToCart />
  </@fields>
  </form>
      </@cell>
    </@row>
  </@section>
<#else>
  <@section>
    <@commonMsg type="result-norecord">${uiLabelMap.ProductNoProductsInThisCategory}.</@commonMsg>
  </@section>
</#if>

