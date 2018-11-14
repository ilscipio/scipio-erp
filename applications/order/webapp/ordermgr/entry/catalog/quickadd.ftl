<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
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

