<#-- TODO: License -->

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

<#--macro menuContent menuArgs={}>
  <@menu args=menuArgs>
  <#if activeOnly>
    <@menuitem type="link" href=makeOfbizUrl("EditCategoryProducts?productCategoryId=${productCategoryId!}&activeOnly=false") text=uiLabelMap.ProductActiveAndInactive class="+${styles.action_run_sys!} ${styles.action_show!}" />
  <#else>
    <@menuitem type="link" href=makeOfbizUrl("EditCategoryProducts?productCategoryId=${productCategoryId!}&activeOnly=true") text=uiLabelMap.ProductActiveOnly class="+${styles.action_run_sys!} ${styles.action_show!}" />
  </#if>
  </@menu>
</#macro-->

<#if (listSize > 0)>
<@section title=uiLabelMap.ProductCategoryProductList menuContent=menuContent>
      
        <@paginate mode="content" url=makeOfbizUrl("EditCategoryProducts") paramStr="productCategoryId=${productCategoryId!}&activeOnly=${activeOnly.toString()}" viewSize=viewSize!1 viewIndex=viewIndex!0 listSize=listSize!0>
            <form method="post" action="<@ofbizUrl>updateCategoryProductMember</@ofbizUrl>" name="updateCategoryProductForm">
              <@fields type="default-manual">
                <input type="hidden" name="VIEW_SIZE" value="${viewSize}"/>
                <input type="hidden" name="VIEW_INDEX" value="${viewIndex}"/>
                <input type="hidden" name="activeOnly" value="${activeOnly.toString()}" />
                <input type="hidden" name="productCategoryId" value="${productCategoryId!}" />
                <@table type="data-complex" autoAltRows=true> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
                    <@thead>
                     <@tr class="header-row">
                        <@th>#</@th>
                        <@th>${uiLabelMap.CommonProduct}</@th>
                        <@th>${uiLabelMap.CommonFrom}</@th>
                        <@th>${uiLabelMap.CommonThru}</@th>
                        <@th>${uiLabelMap.CommonSequence}</@th>
                        <@th>${uiLabelMap.CommonQuantity}</@th>
                        <@th>${uiLabelMap.CommonComments}</@th>
                        <@th>${uiLabelMap.CommonUpdate}</@th>
                        <@th>${uiLabelMap.CommonDelete}</@th>
                     </@tr>
                    </@thead>
                  <#assign rowCount = 0>
                  <#list productCategoryMembers as productCategoryMember>
                    <#assign suffix = "_o_" + productCategoryMember_index>
                    <#assign product = productCategoryMember.getRelatedOne("Product", false)>
                    <#assign hasntStarted = false>
                    <#if productCategoryMember.fromDate?? && nowTimestamp.before(productCategoryMember.getTimestamp("fromDate"))><#assign hasntStarted = true></#if>
                    <#assign hasExpired = false>
                    <#if productCategoryMember.thruDate?? && nowTimestamp.after(productCategoryMember.getTimestamp("thruDate"))><#assign hasExpired = true></#if>
                      <@tr valign="middle">
                        <@td>
                          <#if (product.smallImageUrl)??>
                             <a href="<@ofbizUrl>ViewProduct?productId=${(productCategoryMember.productId)!}</@ofbizUrl>"><img alt="Small Image" src="<@ofbizContentUrl>${product.smallImageUrl}</@ofbizContentUrl>" class="cssImgSmall" align="middle" /></a>
                          </#if>
                        </@td>
                        <@td>
                          <a href="<@ofbizUrl>ViewProduct?productId=${(productCategoryMember.productId)!}</@ofbizUrl>" class="${styles.link_nav_info_idname!}"><#if product??>${(product.internalName)!}</#if> [${(productCategoryMember.productId)!}]</a>                          
                        </@td>
                        <#assign cellClass><#if hasntStarted>+${styles.text_color_alert!}</#if></#assign>
                        <@td class=cellClass>${(productCategoryMember.fromDate?date?string.short)!}</@td>
                        <@td>
                            <@field type="datetime" name="thruDate${suffix}" class=class!'' value=((productCategoryMember.thruDate)!) size="25" maxlength="30" id="thruDate${suffix}" />
                        </@td>
                        <@td>
                            <@field type="input" size="5" name="sequenceNum${suffix}" value=((productCategoryMember.sequenceNum)!) />
                        </@td>
                        <@td>
                            <@field type="input" size="5" name="quantity${suffix}" value=((productCategoryMember.quantity)!) />
                        </@td>
                        <@td>
                            <input type="hidden" name="productId${suffix}" value="${(productCategoryMember.productId)!}" />
                            <input type="hidden" name="productCategoryId${suffix}" value="${(productCategoryMember.productCategoryId)!}" />
                            <input type="hidden" name="fromDate${suffix}" value="${(productCategoryMember.fromDate)!}" />
                            <#if hasExpired><#assign class="alert"></#if>
                            <@field type="textarea" name="comments${suffix}" rows="2" cols="40">${(productCategoryMember.comments)!}</@field>
                        </@td>                        
                        <@td>
                            <@field type="submit" text=uiLabelMap.CommonUpdate class="+${styles.link_run_sys!} ${styles.action_update!}" />
                            <input type="hidden" value="${productCategoryMembers.size()}" name="_rowCount" />
                        </@td>
                        <@td align="center">
                            <a href="javascript:document.deleteProductFromCategory_o_${rowCount}.submit()" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonDelete}</a>
                        </@td>
                      </@tr>
                      <#assign rowCount = rowCount + 1>
                  </#list>
                </@table>
              </@fields>
            </form>
            <#assign rowCount = 0>
            <#list productCategoryMembers as productCategoryMember>
                <form name="deleteProductFromCategory_o_${rowCount}" method="post" action="<@ofbizUrl>removeCategoryProductMember</@ofbizUrl>">
                  <input type="hidden" name="VIEW_SIZE" value="${viewSize}"/>
                  <input type="hidden" name="VIEW_INDEX" value="${viewIndex}"/>
                  <input type="hidden" name="productId" value="${(productCategoryMember.productId)!}" />
                  <input type="hidden" name="productCategoryId" value="${(productCategoryMember.productCategoryId)!}"/>
                  <input type="hidden" name="fromDate" value="${(productCategoryMember.fromDate)!}"/>
                  <input type="hidden" name="activeOnly" value="${activeOnly.toString()}"/>
                </form>
                <#assign rowCount = rowCount + 1>
            </#list>   
        </@paginate>
    </@section>
</#if>
