<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<#if productPromoId?? && productPromo??>
                <@alert type="info">${uiLabelMap.ProductPromoStoreAddInstructions}</@alert>

                <@section title=uiLabelMap.ProductAddStorePromo>
                    <form method="post" action="<@ofbizUrl>promo_createProductStorePromoAppl</@ofbizUrl>" name="addProductPromoToCatalog">
                      <@fields type="default-compact">
                        <@row>
                            <@cell columns=4>
                                <input type="hidden" name="productPromoId" value="${productPromoId}"/>
                                <input type="hidden" name="tryEntity" value="true"/>
                                <@field type="select" name="productStoreId">
                                  <#list productStores as productStore>
                                    <option value="${(productStore.productStoreId)!}">${(productStore.storeName)!(productStore.productStoreId)!} [${(productStore.productStoreId)!}]</option>
                                  </#list>
                                </@field>
                            </@cell>
                            <@cell columns=4>
                                <@field type="datetime" name="fromDate" value="" size="25" maxlength="30" id="fromDate1" />
                            </@cell>
                            <@cell columns=4>
                                <@field type="submit" text=uiLabelMap.CommonAdd class="${styles.link_run_sys!} ${styles.action_add!}"/>
                            </@cell>
                            </@row>
                      </@fields>
                    </form>
                </@section>
                <@section title=uiLabelMap.PageTitleEditProductPromoStores>
                  <@fields type="default-manual-widgetonly">
                    <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
                         <@thead>
                            <@tr class="header-row">
                                <@th>${uiLabelMap.ProductStoreNameId}</@th>
                                <@th>${uiLabelMap.CommonFromDateTime}</@th>
                                <@th align="center">${uiLabelMap.ProductThruDateTimeSequence}</@th>
                                <@th>&nbsp;</@th>
                            </@tr>
                         </@thead>
                         <@tbody>
                            <#assign line = 0>
                            <#list productStorePromoAppls as productStorePromoAppl>
                            <#assign line = line + 1>
                            <#assign productStore = productStorePromoAppl.getRelatedOne("ProductStore", false)>
                            <@tr valign="middle">
                                <@td><a href="<@ofbizInterWebappUrl>/catalog/control/EditProductStore?productStoreId=${productStorePromoAppl.productStoreId}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_idname!}"><#if productStore??>${(productStore.storeName)!(productStore.productStoreId)!""}<#else>${productStorePromoAppl.productStoreId!""}</#if></a></@td>
                                <#assign hasntStarted = false>
                                <#if (productStorePromoAppl.getTimestamp("fromDate"))?? && nowTimestamp.before(productStorePromoAppl.getTimestamp("fromDate"))> <#assign hasntStarted = true></#if>
                                <#assign cellClass><#if hasntStarted>+${styles.text_color_alert!}</#if></#assign>
                                <@td class=cellClass>${productStorePromoAppl.fromDate!}</@td>
                                <@td align="center">

                                    <#assign hasExpired = false>
                                    <#if (productStorePromoAppl.getTimestamp("thruDate"))?? && nowTimestamp.after(productStorePromoAppl.getTimestamp("thruDate"))> <#assign hasExpired = true></#if>
                                    <form method="post" action="<@ofbizUrl>promo_updateProductStorePromoAppl</@ofbizUrl>" name="lineForm${line}">
                                        <@row>
                                        <@cell columns=8>
                                            <input type="hidden" name="productStoreId" value="${productStorePromoAppl.productStoreId}" />
                                            <input type="hidden" name="productPromoId" value="${productStorePromoAppl.productPromoId}" />
                                            <input type="hidden" name="fromDate" value="${productStorePromoAppl.fromDate}" />
                                            <#if hasExpired><#assign class="alert"></#if>
                                            <@field type="datetime" name="thruDate" class=class!'' value=((productStorePromoAppl.thruDate)!) size="25" maxlength="30" id="thruDate_${productStorePromoAppl_index}" />
                                        </@cell>
                                        <@cell columns=4>
                                            <input type="hidden" size="5" name="sequenceNum" value="${(productStorePromoAppl.sequenceNum)!}" />
                                            <input type="submit" value="${uiLabelMap.CommonUpdate}" class="${styles.link_run_sys!} ${styles.action_update!}" />
                                        </@cell>
                                        </@row>
                                    </form>
                                </@td>
                                <@td align="center">
                                   <form method="post" action="<@ofbizUrl>promo_deleteProductStorePromoAppl</@ofbizUrl>">
                                       <input type="hidden" name="productStoreId" value="${productStorePromoAppl.productStoreId}" />
                                       <input type="hidden" name="productPromoId" value="${productStorePromoAppl.productPromoId}" />
                                       <input type="hidden" name="fromDate" value="${productStorePromoAppl.fromDate}" />
                                       <input type="submit" value="${uiLabelMap.CommonDelete}" class="${styles.link_run_sys!} ${styles.action_remove!}" />
                                   </form>
                                </@td>
                            </@tr>
                            </#list>
                          </@tbody>
                    </@table>
                  </@fields>
                </@section>  

</#if>