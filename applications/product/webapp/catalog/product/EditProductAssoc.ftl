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
<@section title="${uiLabelMap.PageTitleEditProductAssociations}">
        <form action="<@ofbizUrl>UpdateProductAssoc</@ofbizUrl>" method="post" style="margin: 0;" name="editProductAssocForm">
        <input type="hidden" name="productId" value="${productId!}" />

        <#if !(productAssoc??)>
            <#if productId?? && productIdTo?? && productAssocTypeId?? && fromDate??>
                <div><b><#assign uiLabelWithVar=uiLabelMap.ProductAssociationNotFound?interpret><@uiLabelWithVar/></b></div>
                <input type="hidden" name="UPDATE_MODE" value="CREATE" />
            <#else>
                <input type="hidden" name="UPDATE_MODE" value="CREATE" />
            </#if>
        <#else>
            <#assign isCreate = false>
            <#assign curProductAssocType = productAssoc.getRelatedOne("ProductAssocType", true)>
            <input type="hidden" name="UPDATE_MODE" value="UPDATE" />
            <input type="hidden" name="PRODUCT_ID" value="${productId!}" />
            <input type="hidden" name="PRODUCT_ID_TO" value="${productIdTo!}" />
            <input type="hidden" name="PRODUCT_ASSOC_TYPE_ID" value="${productAssocTypeId!}" />
            <input type="hidden" name="FROM_DATE" value="${fromDate!}" />
        </#if>


        <#if !(productAssoc??)>
            <#if productId?? && productIdTo?? && productAssocTypeId?? && fromDate??>
              <@field type="generic" label="${uiLabelMap.ProductProductId}">
                  <input type="text" name="PRODUCT_ID" size="20" maxlength="40" value="${productId!}" />
              </@field>
              <@field type="generic" label="${uiLabelMap.ProductProductIdTo}">
                  <input type="text" name="PRODUCT_ID_TO" size="20" maxlength="40" value="${productIdTo!}" />
              </@field>
              <@field type="generic" label="${uiLabelMap.ProductAssociationTypeId}">
                    <select name="PRODUCT_ASSOC_TYPE_ID" size="1">
                    <#if productAssocTypeId?has_content>
                        <#assign curAssocType = delegator.findOne("ProductAssocType", Static["org.ofbiz.base.util.UtilMisc"].toMap("productAssocTypeId", productAssocTypeId), false)>
                        <#if curAssocType??>
                            <option selected="selected" value="${(curAssocType.productAssocTypeId)!}">${(curAssocType.get("description",locale))!}</option>
                            <option value="${(curAssocType.productAssocTypeId)!}"></option>
                        </#if>
                    </#if>
                    <#list assocTypes as assocType>
                        <option value="${(assocType.productAssocTypeId)!}">${(assocType.get("description",locale))!}</option>
                    </#list>
                    </select>
              </@field>
              <@field type="generic" label="${uiLabelMap.CommonFromDate}">
                  <@htmlTemplate.renderDateTimeField name="FROM_DATE" event="" action="" value="${fromDate!}" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" size="25" maxlength="30" id="fromDate1" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
                        ${uiLabelMap.CommonSetNowEmpty}
              </@field>
            <#else>
              <@field type="generic" label="${uiLabelMap.ProductProductId}">
                  <input type="text" name="PRODUCT_ID" size="20" maxlength="40" value="${productId!}" />
              </@field>
              <@field type="generic" label="${uiLabelMap.ProductProductIdTo}">
                  <@htmlTemplate.lookupField formName="editProductAssocForm" name="PRODUCT_ID_TO" id="PRODUCT_ID_TO" fieldFormName="LookupProduct"/>
              </@field>
              <@field type="generic" label="${uiLabelMap.ProductAssociationTypeId}">
                  <select name="PRODUCT_ASSOC_TYPE_ID" size="1">
                    <!-- <option value="">&nbsp;</option> -->
                    <#list assocTypes as assocType>
                        <option value="${(assocType.productAssocTypeId)!}">${(assocType.get("description",locale))!}</option>
                    </#list>
                    </select>
              </@field>
              <@field type="generic" label="${uiLabelMap.CommonFromDate}">
                  ${fromDate!}
                    <@htmlTemplate.renderDateTimeField name="FROM_DATE" event="" action="" value="${fromDate!}" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" size="25" maxlength="30" id="fromDate2" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
                    ${uiLabelMap.CommonSetNowEmpty}
              </@field>
            </#if>
        <#else>
            <@field type="generic" label="${uiLabelMap.ProductProductId}">
                <b>${productId!}</b> ${uiLabelMap.ProductRecreateAssociation}
            </@field>
            <@field type="generic" label="${uiLabelMap.ProductProductIdTo}">
                <b>${productIdTo!}</b> ${uiLabelMap.ProductRecreateAssociation}
            </@field>
            <@field type="generic" label="${uiLabelMap.ProductAssociationType}">
                <b><#if curProductAssocType??>${(curProductAssocType.get("description",locale))!}<#else> ${productAssocTypeId!}</#if></b> ${uiLabelMap.ProductRecreateAssociation}
            </@field>
            <@field type="generic" label="${uiLabelMap.CommonFromDate}">
                <b>${fromDate!}</b> ${uiLabelMap.ProductRecreateAssociation}
            </@field>
        </#if>
        
        <@field type="generic" label="${uiLabelMap.CommonThruDate}">
          <#if useValues> 
            <#assign value = productAssoc.thruDate!>
          <#else> 
            <#assign value = (request.getParameter("THRU_DATE"))!>
          </#if>            
            <@htmlTemplate.renderDateTimeField name="THRU_DATE" event="" action="" value="${value}" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" size="25" maxlength="30" id="thruDate1" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
        </@field>
        <@field type="generic" label="${uiLabelMap.ProductSequenceNum}">
            <input type="text" name="SEQUENCE_NUM" <#if useValues>value="${(productAssoc.sequenceNum)!}"<#else>value="${(request.getParameter("SEQUENCE_NUM"))!}"</#if> size="5" maxlength="10" />
        </@field>
        <@field type="generic" label="${uiLabelMap.ProductReason}">
            <input type="text" name="REASON" <#if useValues>value="${(productAssoc.reason)!}"<#else>value="${(request.getParameter("REASON"))!}"</#if> size="60" maxlength="255" />
        </@field>
        <@field type="generic" label="${uiLabelMap.ProductInstruction}">
            <input type="text" name="INSTRUCTION" <#if useValues>value="${(productAssoc.instruction)!}"<#else>value="${(request.getParameter("INSTRUCTION"))!}"</#if> size="60" maxlength="255" />
        </@field>

        <@field type="generic" label="${uiLabelMap.ProductQuantity}">
            <input type="text" name="QUANTITY" <#if useValues>value="${(productAssoc.quantity)!}"<#else>value="${(request.getParameter("QUANTITY"))!}"</#if> size="10" maxlength="15" />
        </@field>

        <@field type="submitarea">
            <input type="submit" <#if isCreate>value="${uiLabelMap.CommonCreate}"<#else>value="${uiLabelMap.CommonUpdate}"</#if> />
        </@field>
 
        </form>
</@section>
<#if productId?? && product??>
    <@section title="${uiLabelMap.ProductAssociationsFromProduct}">
        <@table type="data-list" autoAltRows=true cellspacing="0" class="basic-table">
         <@thead>
            <@tr class="header-row">
                <@th>${uiLabelMap.ProductProductId}</@th>
                <@th>${uiLabelMap.ProductName}</@th>
                <@th>${uiLabelMap.CommonFromDateTime}</@th>
                <@th>${uiLabelMap.CommonThruDateTime}</@th>
                <@th>${uiLabelMap.ProductSeqNum}</@th>
                <@th>${uiLabelMap.CommonQuantity}</@th>
                <@th>${uiLabelMap.ProductAssociationType}</@th>
                <@th>&nbsp;</@th>
                <@th>&nbsp;</@th>
            </@tr>
          </@thead>
          <@tbody>
            <#list assocFromProducts as assocFromProduct>
            <#assign listToProduct = assocFromProduct.getRelatedOne("AssocProduct", true)>
            <#assign curProductAssocType = assocFromProduct.getRelatedOne("ProductAssocType", true)>
            <@tr valign="middle">
                <@td><a href="<@ofbizUrl>EditProduct?productId=${(assocFromProduct.productIdTo)!}</@ofbizUrl>" class="${styles.button_default!}">${(assocFromProduct.productIdTo)!}</a></@td>
                <@td><#if listToProduct??><a href="<@ofbizUrl>EditProduct?productId=${(assocFromProduct.productIdTo)!}</@ofbizUrl>" class="${styles.button_default!}">${(listToProduct.internalName)!}</a></#if>&nbsp;</@td>
                <#assign colorStyle><#if (assocFromProduct.getTimestamp("fromDate"))?? && nowDate.before(assocFromProduct.getTimestamp("fromDate"))> style="color: red;"</#if></#assign>
                <@td style=colorStyle>
                ${(assocFromProduct.fromDate)!}&nbsp;</@td>
                <#assign colorStyle><#if (assocFromProduct.getTimestamp("thruDate"))?? && nowDate.after(assocFromProduct.getTimestamp("thruDate"))> style="color: red;"</#if></#assign>
                <@td style=colorStyle>
                ${(assocFromProduct.thruDate)!}&nbsp;</@td>
                <@td>&nbsp;${(assocFromProduct.sequenceNum)!}</@td>
                <@td>&nbsp;${(assocFromProduct.quantity)!}</@td>
                <@td><#if curProductAssocType??> ${(curProductAssocType.get("description",locale))!}<#else>${(assocFromProduct.productAssocTypeId)!}</#if></@td>
                <@td>
                <a href="<@ofbizUrl>UpdateProductAssoc?UPDATE_MODE=DELETE&amp;productId=${productId}&amp;PRODUCT_ID=${productId}&amp;PRODUCT_ID_TO=${(assocFromProduct.productIdTo)!}&amp;PRODUCT_ASSOC_TYPE_ID=${(assocFromProduct.productAssocTypeId)!}&amp;FROM_DATE=${(assocFromProduct.fromDate)!}&amp;useValues=true</@ofbizUrl>" class="${styles.button_default!}">
                ${uiLabelMap.CommonDelete}</a>
                </@td>
                <@td>
                <a href="<@ofbizUrl>EditProductAssoc?productId=${productId}&amp;PRODUCT_ID=${productId}&amp;PRODUCT_ID_TO=${(assocFromProduct.productIdTo)!}&amp;PRODUCT_ASSOC_TYPE_ID=${(assocFromProduct.productAssocTypeId)!}&amp;FROM_DATE=${(assocFromProduct.fromDate)!}&amp;useValues=true</@ofbizUrl>" class="${styles.button_default!}">
                ${uiLabelMap.CommonEdit}</a>
                </@td>
            </@tr>
            </#list>
          </@tbody>
        </@table>
    </@section>
    <@section title="${uiLabelMap.ProductAssociationsToProduct}">
        <@table type="data-list" autoAltRows=true cellspacing="0" class="basic-table">
          <@thead>
            <@tr class="header-row">
            <@th>${uiLabelMap.ProductProductId}</@th>
            <@th>${uiLabelMap.ProductName}</@th>
            <@th>${uiLabelMap.CommonFromDateTime}</@th>
            <@th>${uiLabelMap.CommonThruDateTime}</@th>
            <@th>${uiLabelMap.ProductAssociationType}</@th>
            <@th>&nbsp;</@th>
            </@tr>
          </@thead>
          <@tbody>  
            <#list assocToProducts as assocToProduct>
            <#assign listToProduct = assocToProduct.getRelatedOne("MainProduct", true)>
            <#assign curProductAssocType = assocToProduct.getRelatedOne("ProductAssocType", true)>
            <@tr valign="middle">
                <@td><a href="<@ofbizUrl>EditProduct?productId=${(assocToProduct.productId)!}</@ofbizUrl>" class="${styles.button_default!}">${(assocToProduct.productId)!}</a></@td>
                <@td><#if listToProduct??><a href="<@ofbizUrl>EditProduct?productId=${(assocToProduct.productId)!}</@ofbizUrl>" class="${styles.button_default!}">${(listToProduct.internalName)!}</a></#if></@td>
                <@td>${(assocToProduct.getTimestamp("fromDate"))!}&nbsp;</@td>
                <@td>${(assocToProduct.getTimestamp("thruDate"))!}&nbsp;</@td>
                <@td><#if curProductAssocType??> ${(curProductAssocType.get("description",locale))!}<#else> ${(assocToProduct.productAssocTypeId)!}</#if></@td>
                <@td>
                <a href="<@ofbizUrl>UpdateProductAssoc?UPDATE_MODE=DELETE&amp;productId=${(assocToProduct.productIdTo)!}&amp;PRODUCT_ID=${(assocToProduct.productId)!}&amp;PRODUCT_ID_TO=${(assocToProduct.productIdTo)!}&amp;PRODUCT_ASSOC_TYPE_ID=${(assocToProduct.productAssocTypeId)!}&amp;FROM_DATE=${(assocToProduct.fromDate)!}&amp;useValues=true</@ofbizUrl>" class="${styles.button_default!}">
                ${uiLabelMap.CommonDelete}</a>
                </@td>
            </@tr>
            </#list>
          </@tbody>
        </@table>
    </@section>
</#if>
<br />
<span class="tooltip">${uiLabelMap.CommonNote} : ${uiLabelMap.ProductHighlightedExplanation}</span>
