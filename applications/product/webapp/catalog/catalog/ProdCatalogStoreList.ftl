<@section>
    <#if productStoreCatalogList?has_content>  
         <#list productStoreCatalogList as prodCatalogStore>
            <form name="removeProdCatalogFromStore_${prodCatalogStore_index}" method="post" action="<@ofbizUrl>deleteProdCatalogStore</@ofbizUrl>">
                <input name="prodCatalogId" type="hidden" value="${prodCatalogStore.prodCatalogId}"/>
                <input name="productStoreId" type="hidden" value="${prodCatalogStore.productStoreId}"/>
                <input name="fromDate" type="hidden" value="${prodCatalogStore.fromDate}"/>                
            </form>
        </#list>      
        <form id="UpdateProdCatalogToStore" name="UpdateProdCatalogToStore" method="post" action="<@ofbizUrl>updateProdCatalogStore</@ofbizUrl>">
            <input name="prodCatalogId" type="hidden" value="${parameters.prodCatalogId}"/>
            <input name="_useRowSubmit" type="hidden" value="Y"/>
            <@table type="data-list" autoAltRows=true responsive=true scrollable=true> <#-- orig: class="basic-table hover-bar" --> <#-- orig: cellspacing="0" -->
                <#-- Header Begins -->
                <@thead>
                    <@tr class="header-row-2">                        
                        <@th>${uiLabelMap.CommStore}</@th>
                        <@th>${uiLabelMap.CommonFrom}</@th>
                        <@th>${uiLabelMap.CommonThru}</@th>
                        <@th>${uiLabelMap.CommonSequenceNum}</@th>
                        <@th>${uiLabelMap.CommonUpdate}</@th>
                        <@th>${uiLabelMap.CommonDelete}</@th>
                    </@tr>
                </@thead>
                <#-- Header Ends-->
                <@tbody>
                    <#list productStoreCatalogList as prodCatalogStore>
                        <#assign productStore = (prodCatalogStore.getRelatedOne("ProductStore", true))!>
                        <@tr>
                            <@td>
                                  <input name="prodCatalogId_o_${prodCatalogStore_index}" type="hidden" value="${parameters.prodCatalogId}"/>
                                  <input name="partyId_o_${prodCatalogStore_index}" type="hidden" value="${prodCatalogStore.partyId}"/>
                                  <input name="productStoreId_o_${prodCatalogStore_index}" type="hidden" value="${prodCatalogStore.productStoreId}"/>
                                  <input name="fromDate_o_${prodCatalogStore_index}" type="hidden" value="${prodCatalogStore.fromDate}"/>
                                  <input id="_rowSubmit_o_${prodCatalogStore_index}" name="_rowSubmit_o_${prodCatalogStore_index}" type="hidden" value="N"/>
                                  <a href="<@ofbizUrl>EditProductStore?productStoreId=${prodCatalogStore.productStoreId}</@ofbizUrl>" class="${styles.link_nav_info_idname}">                                  
                                      <#if productStore.storeName?has_content>${productStore.storeName} - </#if>${productStore.productStoreId}
                                  </a>                      
                            </@td>
                            <@td>${prodCatalogStore.fromDate?string('yyyy-MM-dd')}</@td>
                            <@td>
                                <@field type="datetime" name="thruDate_o_${prodCatalogStore_index}" value=(prodCatalogStore.thruDate!) size=13 />
                            </@td>
                            <@td>
                                <@field type="input" name="sequenceNum_o_${prodCatalogStore_index}" value=(prodCatalogStore.sequenceNum!) size=10 maxlength=20 />
                            </@td>
                            <@td>
                                <@field type="submit" submitType="link" href="javascript:document.forms['UpdateProdCatalogToStore'].elements['_rowSubmit_o_${prodCatalogStore_index}'].value = 'Y';document.UpdateProdCatalogToStore.submit();" name="Update" text=uiLabelMap.CommonUpdate class="${styles.link_run_sys} ${styles.action_update}"/>                                                                                           
                            </@td>
                            <@td>
                                <a href="javascript:document.removeProdCatalogFromStore_${prodCatalogStore_index}.submit();" class="${styles.link_run_sys} ${styles.action_remove}">${uiLabelMap.CommonDelete}</a>
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