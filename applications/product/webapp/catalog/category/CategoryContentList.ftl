<@section>
    <#if productCategoryContentList?has_content>  
         <#list productCategoryContentList as productCategoryContent>
            <form name="removeContentFromCategory_${productCategoryContent_index}" method="post" action="<@ofbizUrl>removeContentFromCategory</@ofbizUrl>">
                  <input name="productCategoryId" type="hidden" value="${parameters.productCategoryId}"/>
                  <input name="contentId" type="hidden" value="${productCategoryContent.contentId}"/>
                  <input name="prodCatContentTypeId" type="hidden" value="${productCategoryContent.prodCatContentTypeId}"/>
                  <input name="fromDate" type="hidden" value="${productCategoryContent.fromDate}"/>
            </form>
        </#list>
        <@table type="data-list" autoAltRows=true responsive=true scrollable=true> <#-- orig: class="basic-table hover-bar" --> <#-- orig: cellspacing="0" -->
            <#-- Header Begins -->
            <@thead>
                <@tr class="header-row-2">
                    <@th>${uiLabelMap.ProductContent}</@th>
                    <@th>${uiLabelMap.ProductType}</@th>
                    <@th>${uiLabelMap.CommonFrom}</@th>
                    <@th>${uiLabelMap.CommonUpdate}</@th>
                    <@th>${uiLabelMap.CommonDelete}</@th>
                </@tr>
            </@thead>
            <#-- Header Ends-->
            <@tbody>
                <#list productCategoryContentList as productCategoryContent>
                    <#assign productCategoryContentType = (productCategoryContent.getRelatedOne("ProductCategoryContentType", true))!>
                    <@tr>
                        <@td>
                              <a href="<@ofbizUrl>EditCategoryContentContent</@ofbizUrl>" class="${styles.link_nav_info_name_long}">
                                  ${productCategoryContent.contentId} 
                              </a>
                        </@td>
                        <@td>${productCategoryContentType.description}</@td>
                        <@td>${productCategoryContent.fromDate?string("yyyy-MM-dd")}</@td>
                        <@td>
                            <form id="UpdateCategoryContentAssoc_${productCategoryContent_index}" name="UpdateCategoryContentAssoc_${productCategoryContent_index}" method="post" action="<@ofbizUrl>EditCategoryContent</@ofbizUrl>">
                                <input name="productCategoryId" type="hidden" value="${parameters.productCategoryId}"/>
                                <input name="contentId" type="hidden" value="${productCategoryContent.contentId}"/>
                                <input name="prodCatContentTypeId" type="hidden" value="${productCategoryContent.prodCatContentTypeId}"/>
                                <input name="fromDate" type="hidden" value="${productCategoryContent.fromDate?string("MM/dd/yyyy HH:mm:ss")}"/>                                    
                                <@field type="submit" submitType="link" href="javascript:document.UpdateCategoryContentAssoc_${productCategoryContent_index}.submit();" name="Update" text=uiLabelMap.CommonUpdate class="${styles.link_run_sys} ${styles.action_update}"/>
                            </form>
                        </@td>
                        <@td>
                            <a href="javascript:document.removeContentFromCategory_${productCategoryContent_index}.submit();" class="${styles.link_run_sys} ${styles.action_remove}">${uiLabelMap.CommonDelete}</a>
                        </@td>
                    </@tr>
                </#list>
            </@tbody>
        </@table>
    <#else>
        <@commonMsg type="result-norecord"/>
    </#if>
</@section>