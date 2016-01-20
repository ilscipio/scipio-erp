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

<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
  <#if activeOnly>
    <@menuitem type="link" href=makeOfbizUrl("EditCategoryProducts?productCategoryId=${productCategoryId!}&amp;activeOnly=false") text="${uiLabelMap.ProductActiveAndInactive}" />
  <#else>
    <@menuitem type="link" href=makeOfbizUrl("EditCategoryProducts?productCategoryId=${productCategoryId!}&amp;activeOnly=true") text="${uiLabelMap.ProductActiveOnly}" />
  </#if>
  </@menu>
</#macro>

<@section title="${uiLabelMap.PageTitleEditCategoryProducts}" menuContent=menuContent>
      <#macro categoryProductsNav>
        <@menu type="button">
          <@menuitem type="link" href=makeOfbizUrl("EditCategoryProducts?productCategoryId=${productCategoryId!}&amp;VIEW_SIZE=${viewSize}&amp;VIEW_INDEX=${viewIndex-1}&amp;activeOnly=${activeOnly.toString()}") text="${uiLabelMap.CommonPrevious}" disabled=(viewIndex <= 1) />
          <@menuitem type="text" text="${lowIndex} - ${highIndex} ${uiLabelMap.CommonOf} ${listSize}" />
          <@menuitem type="link" href=makeOfbizUrl("EditCategoryProducts?productCategoryId=${productCategoryId!}&amp;VIEW_SIZE=${viewSize}&amp;VIEW_INDEX=${viewIndex+1}&amp;activeOnly=${activeOnly.toString()}") text="${uiLabelMap.CommonNext}" disabled=(listSize <= highIndex) />
        </@menu>
      </#macro>
      
      <#if (listSize > 0)>
        <@categoryProductsNav />
      </#if>

        <#if (listSize > 0)>
           <form method="post" action="<@ofbizUrl>updateCategoryProductMember</@ofbizUrl>" name="updateCategoryProductForm">
              <input type="hidden" name="VIEW_SIZE" value="${viewSize}"/>
              <input type="hidden" name="VIEW_INDEX" value="${viewIndex}"/>
              <input type="hidden" name="activeOnly" value="${activeOnly.toString()}" />
              <input type="hidden" name="productCategoryId" value="${productCategoryId!}" />
              <@table type="data-complex" autoAltRows=true cellspacing="0"> <#-- orig: class="basic-table" -->
                <@thead>
                 <@tr class="header-row">
                    <@th>${uiLabelMap.CommonProduct}</@th>
                    <@th>${uiLabelMap.CommonFrom}</@th>
                    <@th align="center">${uiLabelMap.ProductThruDateTimeSequenceQuantity} ${uiLabelMap.CommonComments}</@th>
                    <@th>&nbsp;</@th>
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
                      <a href="<@ofbizUrl>EditProduct?productId=${(productCategoryMember.productId)!}</@ofbizUrl>" class="${styles.link_nav_info_idname!}"><#if product??>${(product.internalName)!}</#if> [${(productCategoryMember.productId)!}]</a>
                      <#if (product.smallImageUrl)??>
                         <a href="<@ofbizUrl>EditProduct?productId=${(productCategoryMember.productId)!}</@ofbizUrl>"><img alt="Small Image" src="<@ofbizContentUrl>${product.smallImageUrl}</@ofbizContentUrl>" class="cssImgSmall" align="middle" /></a>
                      </#if>
                    </@td>
                    <#assign colorStyle><#if hasntStarted> style="color: red;"</#if></#assign>
                    <@td style=colorStyle>${(productCategoryMember.fromDate?date?string.short)!}</@td>
                    <@td align="center">
                        <input type="hidden" name="productId${suffix}" value="${(productCategoryMember.productId)!}" />
                        <input type="hidden" name="productCategoryId${suffix}" value="${(productCategoryMember.productCategoryId)!}" />
                        <input type="hidden" name="fromDate${suffix}" value="${(productCategoryMember.fromDate)!}" />
                        <#if hasExpired><#assign class="alert"></#if>
                        <@htmlTemplate.renderDateTimeField name="thruDate${suffix}" event="" action="" className="${class!''}"  title="Format: yyyy-MM-dd HH:mm:ss.SSS" value="${(productCategoryMember.thruDate)!}" size="25" maxlength="30" id="thruDate${suffix}" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
                        <input type="text" size="5" name="sequenceNum${suffix}" value="${(productCategoryMember.sequenceNum)!}" />
                        <input type="text" size="5" name="quantity${suffix}" value="${(productCategoryMember.quantity)!}" />
                        <br />
                        <textarea name="comments${suffix}" rows="2" cols="40">${(productCategoryMember.comments)!}</textarea>
                    </@td>
                    <@td align="center">
                      <a href="javascript:document.deleteProductFromCategory_o_${rowCount}.submit()" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonDelete}</a>
                    </@td>
                  </@tr>
                  <@tr valign="middle" groupLast=true>
                      <@td colspan="4" align="center">
                          <input type="submit" value="${uiLabelMap.CommonUpdate}" class="${styles.link_run_sys!} ${styles.action_update!}" />
                          <input type="hidden" value="${productCategoryMembers.size()}" name="_rowCount" />
                      </@td>
                  </@tr>
                  <#assign rowCount = rowCount + 1>
              </#list>
              </@table>
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
      <#else>
        <@resultMsg>${uiLabelMap.CommonNoRecordFound}.</@resultMsg>     
      </#if>
    
      <#if (listSize > 0)>
        <@categoryProductsNav />
      </#if>
</@section>
