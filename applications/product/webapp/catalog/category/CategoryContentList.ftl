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
                  <#-- SCIPIO: TODO: REVIEW
                    <@th>${uiLabelMap.CommonEdit}</@th>
                   -->
                    <@th>${uiLabelMap.CommonDelete}</@th>
                </@tr>
            </@thead>
            <#-- Header Ends-->
            <@tbody>
                <#list productCategoryContentList as productCategoryContent>
                    <#assign productCategoryContentType = (productCategoryContent.getRelatedOne("ProductCategoryContentType", true))!>
                    <@tr>
                        <@td>
                            <form id="EditCategoryContentAssoc_${productCategoryContent_index}" name="EditCategoryContentAssoc_${productCategoryContent_index}" method="get" action="<@ofbizUrl>EditCategoryContent</@ofbizUrl>">
                                <input name="productCategoryId" type="hidden" value="${parameters.productCategoryId}"/>
                                <input name="contentId" type="hidden" value="${productCategoryContent.contentId}"/>
                                <input name="prodCatContentTypeId" type="hidden" value="${productCategoryContent.prodCatContentTypeId}"/>
                                <input name="fromDate" type="hidden" value="${productCategoryContent.fromDate}"/>                                    
                                <@field type="submit" submitType="link" href="javascript:document.EditCategoryContentAssoc_${productCategoryContent_index}.submit();" text=productCategoryContent.contentId class="${styles.link_nav_info} ${styles.action_update}"/>
                            </form>
                        </@td>
                        <@td>${productCategoryContentType.description}</@td>
                        <@td>${productCategoryContent.fromDate?string("yyyy-MM-dd")}</@td>
                      <#-- SCIPIO: TODO: REVIEW
                        <@td>
                            <a href="<@ofbizInterWebappUrl extLoginKey=true>/content/control/EditContent?contentId=${productCategoryContent.contentId}</@ofbizInterWebappUrl>" class="${styles.link_nav} ${styles.action_update}">${uiLabelMap.CommonEdit}</a>
                        </@td>
                      -->
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