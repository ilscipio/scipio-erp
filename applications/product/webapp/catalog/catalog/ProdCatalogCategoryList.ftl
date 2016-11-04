<@section>
    <#if prodCatalogCategories?has_content>  
         <#list prodCatalogCategories as prodCatalogCategory>
            <form name="removeProductCategoryFromProdCatalog_${prodCatalogCategory_index}" method="post" action="<@ofbizUrl>removeProductCategoryFromProdCatalog</@ofbizUrl>">
                <input name="prodCatalogId" type="hidden" value="${prodCatalogCategory.prodCatalogId}"/>
                <input name="productCategoryId" type="hidden" value="${prodCatalogCategory.productCategoryId}"/>
                <input name="fromDate" type="hidden" value="${prodCatalogCategory.fromDate}"/>
                <input name="prodCatalogCategoryTypeId" type="hidden" value="${prodCatalogCategory.prodCatalogCategoryTypeId}"/>
                <input name="partyId" type="hidden" value="${userLogin.partyId}"/>
            </form>
        </#list>      
        <form id="EditProdCatalogCategories" name="EditProdCatalogCategories" method="post" action="<@ofbizUrl>updateProductCategoryToProdCatalog</@ofbizUrl>">
            <input name="prodCatalogId" type="hidden" value="${parameters.prodCatalogId}"/>
            <input name="_useRowSubmit" type="hidden" value="Y"/>
            <@table type="data-list" autoAltRows=true responsive=true scrollable=true> <#-- orig: class="basic-table hover-bar" --> <#-- orig: cellspacing="0" -->
                <#-- Header Begins -->
                <@thead>
                    <@tr class="header-row-2">
                        <@th>${uiLabelMap.ProductCategory}</@th>
                        <@th>${uiLabelMap.CommonType}</@th>
                        <@th>${uiLabelMap.CommonFrom}</@th>
                        <@th>${uiLabelMap.CommonThru}</@th>
                        <@th>${uiLabelMap.CommonSequenceNum}</@th>
                        <@th>${uiLabelMap.ProductMakeTop}</@th>
                        <@th>${uiLabelMap.CommonUpdate}</@th>
                        <@th>${uiLabelMap.CommonDelete}</@th>
                    </@tr>
                </@thead>
                <#-- Header Ends-->
                <@tbody>
                    <#list prodCatalogCategories as prodCatalogCategory>
                       
                        <#assign category = (prodCatalogCategory.getRelatedOne("ProductCategory", true))!>
                        <#assign prodCatalogCategoryType = (prodCatalogCategory.getRelatedOne("ProdCatalogCategoryType", true))!>
                        <@tr>
                            <@td>
                                  <input name="prodCatalogId_o_${prodCatalogCategory_index}" type="hidden" value="${parameters.prodCatalogId}"/>
                                  <input name="productCategoryId_o_${prodCatalogCategory_index}" type="hidden" value="${prodCatalogCategory.productCategoryId}"/>
                                  <input name="prodCatalogCategoryTypeId_o_${prodCatalogCategory_index}" type="hidden" value="${prodCatalogCategory.prodCatalogCategoryTypeId}"/>
                                  <input name="fromDate_o_${prodCatalogCategory_index}" type="hidden" value="${prodCatalogCategory.fromDate}"/>
                                  <input id="_rowSubmit_o_${prodCatalogCategory_index}" name="_rowSubmit_o_${prodCatalogCategory_index}" type="hidden" value="N"/>
                                  <a href="EditCategory?productCategoryId=${category.productCategoryId}"><#if category.categoryName?has_content>${category.categoryName} - </#if>${category.productCategoryId}</a>
                            </@td>
                            <@td>${prodCatalogCategoryType.description}</@td>
                            <@td>${prodCatalogCategory.fromDate?string('yyyy-MM-dd')}</@td>
                            <@td>
                                <@field type="datetime" name="thruDate_o_${prodCatalogCategory_index}" value=(prodCatalogCategory.thruDate!) size=13 />
                            </@td>
                            <@td>
                                <@field type="input" name="sequenceNum_o_${prodCatalogCategory_index}" value=(prodCatalogCategory.sequenceNum!) size=10 maxlength=20 />
                            </@td>
                            <@td>
                                <a href="<@ofbizUrl>EditCategory?CATALOG_TOP_CATEGORY=${prodCatalogCategory.productCategoryId}</@ofbizUrl>" class="${styles.link_run_session} ${styles.action_update}">${uiLabelMap.ProductMakeTop}</a>
                            </@td>
                            <@td>
                                <@field type="submit" submitType="link" href="javascript:document.forms['EditProdCatalogCategories'].elements['_rowSubmit_o_${prodCatalogCategory_index}'].value = 'Y';document.EditProdCatalogCategories.submit();" name="Update" text=uiLabelMap.CommonUpdate class="${styles.link_run_sys} ${styles.action_update}"/>                                                                                           
                            </@td>
                            <@td>
                                <a href="javascript:document.removeProductCategoryFromProdCatalog_${prodCatalogCategory_index}.submit();" class="${styles.link_run_sys} ${styles.action_remove}">${uiLabelMap.CommonDelete}</a>
                            </@td>
                        </@tr>
                    </#list>
                </@tbody>
            </@table>
        </form>
    <#else>
        <@commonMsg type="result-norecord"/>
    </#if>
</@section>