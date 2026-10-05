<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
-->
<@section>
    <#if productCategoryContentList?has_content>  
         <#list productCategoryContentList as productCategoryContent>
            <form name="removeContentFromCategory_${productCategoryContent_index}" method="post" action="<@pageUrl>removeContentFromCategory</@pageUrl>">
                  <input name="productCategoryId" type="hidden" value="${parameters.productCategoryId}"/>
                  <input name="contentId" type="hidden" value="${productCategoryContent.contentId}"/>
                  <input name="prodCatContentTypeId" type="hidden" value="${productCategoryContent.prodCatContentTypeId}"/>
                  <input name="fromDate" type="hidden" value="${productCategoryContent.fromDate}"/>
            </form>
        </#list>
        <@paginate mode="content" url=makePageUrl("EditCategoryContent") paramStr="productCategoryId=${productCategoryId!}" viewSize=viewSize!1 viewIndex=viewIndex!0 listSize=listSize!0>
            <@table type="data-list" autoAltRows=true responsive=true scrollable=true>
                <#-- Header Begins -->
                <@thead>
                    <@tr class="header-row-2">
                        <@th>${uiLabelMap.ProductContent}</@th>
                        <@th>${uiLabelMap.ProductType}</@th>
                        <@th>${uiLabelMap.CommonFrom}</@th>
                        <@th>${uiLabelMap.CommonEdit}</@th>
                        <@th>${uiLabelMap.CommonDelete}</@th>
                    </@tr>
                </@thead>
                <#-- Header Ends-->
                <@tbody>
                    <#list productCategoryContentList as productCategoryContent>
                        <#assign productCategoryContentType = (productCategoryContent.getRelatedOne("ProductCategoryContentType", true))!>
                        <@tr>
                            <@td>
                                <form id="EditCategoryContentAssoc_${productCategoryContent_index}" name="EditCategoryContentAssoc_${productCategoryContent_index}" method="get" action="<@pageUrl>EditCategoryContentContent</@pageUrl>">
                                    <input name="productCategoryId" type="hidden" value="${parameters.productCategoryId}"/>
                                    <input name="contentId" type="hidden" value="${productCategoryContent.contentId}"/>
                                    <input name="prodCatContentTypeId" type="hidden" value="${productCategoryContent.prodCatContentTypeId}"/>
                                    <input name="fromDate" type="hidden" value="${productCategoryContent.fromDate}"/>
                                    <a href="javascript:document.EditCategoryContentAssoc_${productCategoryContent_index}.submit();" class="${styles.link_nav_info}">
                                        [${productCategoryContent.contentId}]
                                    </a>
                                </form>


    <#--                            <field name="editProductContentInfo" title="${uiLabelMap.ProductContent}" widget-style="${styles.link_nav_info_desc}">-->
    <#--                                <hyperlink target="EditProductContentContent" description="${description} [${contentId}]" also-hidden="false">-->
    <#--                                    <parameter param-name="productId" />-->
    <#--                                    <parameter param-name="contentId" />-->
    <#--                                    <parameter param-name="productContentTypeId" />-->
    <#--                                    <parameter param-name="fromDate" />-->
    <#--                                </hyperlink>-->
    <#--                            </field>-->

                            </@td>
                            <@td>${productCategoryContentType.description}</@td>
                            <@td>${productCategoryContent.fromDate?string("yyyy-MM-dd")}</@td>
                            <@td>
                                <a href="<@serverUrl extLoginKey=true>/content/control/EditContent?contentId=${productCategoryContent.contentId}</@serverUrl>" class="${styles.link_nav} ${styles.action_update}">${uiLabelMap.CommonEdit}</a>
                            </@td>

                            <@td>
                                <a href="javascript:document.removeContentFromCategory_${productCategoryContent_index}.submit();" class="${styles.link_run_sys} ${styles.action_remove}">${uiLabelMap.CommonDelete}</a>
                            </@td>
                        </@tr>
                    </#list>
                </@tbody>
            </@table>
        </@paginate>
    <#else>
        <@commonMsg type="result-norecord"/>
    </#if>
</@section>