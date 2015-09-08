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

<script language="JavaScript" type="text/javascript">
<!-- //
function lookupBom() {
    document.searchbom.productId.value=document.editProductAssocForm.productId.value;
    document.searchbom.productAssocTypeId.value=document.editProductAssocForm.productAssocTypeId.options[document.editProductAssocForm.productAssocTypeId.selectedIndex].value;
    document.searchbom.submit();
}
// -->
</script>

<#assign sectionTitle>${uiLabelMap.PageTitleEditProductBom}<#if product??> ${(product.internalName)!}</#if>&nbsp;[${uiLabelMap.CommonId}&nbsp;${productId!}]</#assign>
<#assign menuHtml>
  <@menu type="section" inlineItems=true>
  <#if product?has_content>
    <@menuitem type="link" ofbizHref="BomSimulation?productId=${productId}&amp;bomType=${productAssocTypeId}" text="${uiLabelMap.ManufacturingBomSimulation}" />
  </#if>
  </@menu>
</#assign>
<@section title=sectionTitle menuHtml=menuHtml>
    
  <form name="searchform" action="<@ofbizUrl>UpdateProductBom</@ofbizUrl>#topform" method="post">
    <input type="hidden" name="UPDATE_MODE" value=""/>

    <@table type="fields" class="basic-table" cellspacing="0">
        <@tr>
            <@td align="right">${uiLabelMap.ManufacturingBomType}</@td>
            <@td>&nbsp; <a name="topform"/></@td>
            <@td>
                <select name="productAssocTypeId" size="1">
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
            </@td>
            <@td align="right">${uiLabelMap.ProductProductId}</@td>
            <@td>&nbsp;</@td>
            <@td>
                <@htmlTemplate.lookupField value="${productId!}" formName="searchform" name="productId" id="productId" fieldFormName="LookupProduct"/>
                <span><a href="javascript:document.searchform.submit();" class="${styles.button_default!}">${uiLabelMap.ManufacturingShowBOMAssocs}</a></span>
            </@td>
        </@tr>
        <@tr>
            <@td colspan='3'>
                &nbsp;
            </@td>
            <@td align="right">${uiLabelMap.ManufacturingCopyToProductId}</@td>
            <@td>&nbsp;</@td>
            <@td>
                <@htmlTemplate.lookupField formName="searchform" name="copyToProductId" id="copyToProductId" fieldFormName="LookupProduct"/>
                <span><a href="javascript:document.searchform.UPDATE_MODE.value='COPY';document.searchform.submit();" class="${styles.button_default!}">${uiLabelMap.ManufacturingCopyBOMAssocs}</a></span>
            </@td>
        </@tr>
    </@table>
  </form>
    <hr />
    <form action="<@ofbizUrl>UpdateProductBom</@ofbizUrl>" method="post" name="editProductAssocForm">
    <#if !(productAssoc??)>
        <input type="hidden" name="UPDATE_MODE" value="CREATE"/>
    <#else>
        <#assign curProductAssocType = productAssoc.getRelatedOne("ProductAssocType", true)>
        <input type="hidden" name="UPDATE_MODE" value="UPDATE"/>
        <input type="hidden" name="productId" value="${productId!}"/>
        <input type="hidden" name="productIdTo" value="${productIdTo!}"/>
        <input type="hidden" name="productAssocTypeId" value="${productAssocTypeId!}"/>
        <input type="hidden" name="fromDate" value="${fromDate!}"/>
    </#if>
    
    <@table type="fields" class="basic-table" cellspacing="0">
    
    <#if !(productAssoc??)>
          <@tr>
            <@td align="right">${uiLabelMap.ManufacturingBomType}</@td>
            <@td>&nbsp;</@td>
            <@td>
                <select name="productAssocTypeId" size="1">
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
            </@td>
          </@tr>
          <@tr>
            <@td align="right">${uiLabelMap.ProductProductId}</@td>
            <@td>&nbsp;</@td>
            <@td>
                <@htmlTemplate.lookupField value="${productId!}" formName="editProductAssocForm" name="productId" id="productId2" fieldFormName="LookupProduct"/>
            </@td>
          </@tr>
          <@tr>
            <@td align="right">${uiLabelMap.ManufacturingProductIdTo}</@td>
            <@td>&nbsp;</@td>
            <@td>
                <@htmlTemplate.lookupField value="${productIdTo!}" formName="editProductAssocForm" name="productIdTo" id="productIdTo" fieldFormName="LookupProduct"/>
            </@td>
          </@tr>
          <@tr>
            <@td align="right">${uiLabelMap.CommonFromDate}</@td>
            <@td>&nbsp;</@td>
            <@td>
                <@htmlTemplate.renderDateTimeField name="fromDate" event="" action="" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" value="" size="25" maxlength="50" id="fromDate_1" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
                <span class="tooltip">(${uiLabelMap.ManufacturingWillBeSetToNow})</span>
            </@td>
          </@tr>
    <#else>
          <@tr>
            <@td align="right">${uiLabelMap.ProductProductId}</@td>
            <@td>&nbsp;</@td>
            <@td>${productId!}</@td>
          </@tr>
          <@tr>
            <@td align="right">${uiLabelMap.ManufacturingProductIdTo}</@td>
            <@td>&nbsp;</@td>
            <@td>${productIdTo!}</@td>
          </@tr>
          <@tr>
            <@td align="right">${uiLabelMap.ManufacturingBomType}</@td>
            <@td>&nbsp;</@td>
            <@td><#if curProductAssocType??>${(curProductAssocType.get("description",locale))!}<#else> ${productAssocTypeId!}</#if></@td>
          </@tr>
          <@tr>
            <@td align="right">${uiLabelMap.CommonFromDate}</@td>
            <@td>&nbsp;</@td>
            <@td>${fromDate!}</@td>
          </@tr>
    </#if>
    
    <@tr>
        <@td width="26%" align="right">${uiLabelMap.CommonThruDate}</@td>
        <@td>&nbsp;</@td>
        <@td width="74%">
            <#if useValues> 
              <#assign value= productAssoc.thruDate!>
            <#else>
              <#assign value= request.getParameter("thruDate")!>
            </#if>
            <@htmlTemplate.renderDateTimeField value="${value!''}" name="thruDate" className="" event="" action=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" size="30" maxlength="30" id="fromDate_2" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
        </@td>
    </@tr>
    <@tr>
        <@td width="26%" align="right">${uiLabelMap.CommonSequenceNum}</@td>
        <@td>&nbsp;</@td>
        <@td width="74%"><input type="text" name="sequenceNum" <#if useValues>value="${(productAssoc.sequenceNum)!}"<#else>value="${(request.getParameter("sequenceNum"))!}"</#if> size="5" maxlength="10"/></@td>
    </@tr>
    <@tr>
        <@td width="26%" align="right">${uiLabelMap.ManufacturingReason}</@td>
        <@td>&nbsp;</@td>
        <@td width="74%"><input type="text" name="reason" <#if useValues>value="${(productAssoc.reason)!}"<#else>value="${(request.getParameter("reason"))!}"</#if> size="60" maxlength="255"/></@td>
    </@tr>
    <@tr>
        <@td width="26%" align="right">${uiLabelMap.ManufacturingInstruction}</@td>
        <@td>&nbsp;</@td>
        <@td width="74%"><input type="text" name="instruction" <#if useValues>value="${(productAssoc.instruction)!}"<#else>value="${(request.getParameter("instruction"))!}"</#if> size="60" maxlength="255"/></@td>
    </@tr>
    <@tr>
        <@td width="26%" align="right">${uiLabelMap.ManufacturingQuantity}</@td>
        <@td>&nbsp;</@td>
        <@td width="74%"><input type="text" name="quantity" <#if useValues>value="${(productAssoc.quantity)!}"<#else>value="${(request.getParameter("quantity"))!}"</#if> size="10" maxlength="15"/></@td>
    </@tr>
    <@tr>
        <@td width="26%" align="right">${uiLabelMap.ManufacturingScrapFactor}</@td>
        <@td>&nbsp;</@td>
        <@td width="74%"><input type="text" name="scrapFactor" <#if useValues>value="${(productAssoc.scrapFactor)!}"<#else>value="${(request.getParameter("scrapFactor"))!}"</#if> size="10" maxlength="15"/></@td>
    </@tr>
    <@tr>
        <@td width="26%" align="right">${uiLabelMap.ManufacturingFormula}</@td>
        <@td>&nbsp;</@td>
        <@td width="74%">
            <select name="estimateCalcMethod">
            <option value="">&nbsp;</option>
            <#assign selectedFormula = "">
            <#if useValues>
                <#assign selectedFormula = (productAssoc.estimateCalcMethod)!>
            <#else>
                <#assign selectedFormula = (request.getParameter("estimateCalcMethod"))!>
            </#if>
            <#list formulae as formula>
                <option value="${formula.customMethodId}" <#if selectedFormula = formula.customMethodId>selected="selected"</#if>>${formula.get("description",locale)!}</option>
            </#list>
            </select>
        </@td>
    </@tr>
    <@tr>
        <@td width="26%" align="right">${uiLabelMap.ManufacturingRoutingTask}</@td>
        <@td>&nbsp;</@td>
        <@td width="74%">
          <#if useValues>
            <#assign value = productAssoc.routingWorkEffortId!>
          <#else>
            <#assign value = request.getParameter("routingWorkEffortId")!>
          </#if>
          <#if value?has_content>
            <@htmlTemplate.lookupField value="${value}" formName="editProductAssocForm" name="routingWorkEffortId" id="routingWorkEffortId" fieldFormName="LookupRoutingTask"/>
          <#else>
            <@htmlTemplate.lookupField formName="editProductAssocForm" name="routingWorkEffortId" id="routingWorkEffortId" fieldFormName="LookupRoutingTask"/>
          </#if>
        </@td>
    </@tr>
    <@tr>
        <@td colspan="2">&nbsp;</@td>
        <@td><input type="submit" <#if !(productAssoc??)>value="${uiLabelMap.CommonAdd}"<#else>value="${uiLabelMap.CommonEdit}"</#if>/></@td>
    </@tr>
    </@table>
    </form>
</@section>

<#if productId?? && product??>
  <@section title="${uiLabelMap.ManufacturingProductComponents}">
    <a name="components"></a>
    <@table type="data-list" autoAltRows=true class="basic-table" cellspacing="0">
     <@thead>
      <@tr class="header-row">
        <@th>${uiLabelMap.ProductProductId}</@th>
        <@th>${uiLabelMap.ProductProductName}</@th>
        <@th>${uiLabelMap.CommonFromDate}</@th>
        <@th>${uiLabelMap.CommonThruDate}</@th>
        <@th>${uiLabelMap.CommonSequenceNum}</@th>
        <@th>${uiLabelMap.CommonQuantity}</@th>
        <@th>${uiLabelMap.ManufacturingScrapFactor}</@th>
        <@th>${uiLabelMap.ManufacturingFormula}</@th>
        <@th>${uiLabelMap.ManufacturingRoutingTask}</@th>
        <@th>&nbsp;</@th>
        <@th>&nbsp;</@th>
      </@tr>
    </@thead>
    <#list assocFromProducts! as assocFromProduct>
    <#assign listToProduct = assocFromProduct.getRelatedOne("AssocProduct", true)>
    <#assign curProductAssocType = assocFromProduct.getRelatedOne("ProductAssocType", true)>
      <@tr>
        <@td><a href="<@ofbizUrl>EditProductBom?productId=${(assocFromProduct.productIdTo)!}&amp;productAssocTypeId=${(assocFromProduct.productAssocTypeId)!}#components</@ofbizUrl>" class="${styles.button_default!}">${(assocFromProduct.productIdTo)!}</a></@td>
        <@td><#if listToProduct??><a href="<@ofbizUrl>EditProductBom?productId=${(assocFromProduct.productIdTo)!}&amp;productAssocTypeId=${(assocFromProduct.productAssocTypeId)!}#components</@ofbizUrl>" class="${styles.button_default!}">${(listToProduct.internalName)!}</a></#if>&nbsp;</@td>
        <#assign class><#if (assocFromProduct.getTimestamp("fromDate"))?? && nowDate.before(assocFromProduct.getTimestamp("fromDate"))>alert</#if></#assign>
        <@td class=class>
        ${(assocFromProduct.fromDate)!}&nbsp;</@td>
        <#assign class><#if (assocFromProduct.getTimestamp("thruDate"))?? && nowDate.after(assocFromProduct.getTimestamp("thruDate"))>alert</#if></#assign>
        <@td class=class>
        ${(assocFromProduct.thruDate)!}&nbsp;</@td>
        <@td>&nbsp;${(assocFromProduct.sequenceNum)!}</@td>
        <@td>&nbsp;${(assocFromProduct.quantity)!}</@td>
        <@td>&nbsp;${(assocFromProduct.scrapFactor)!}</@td>
        <@td>&nbsp;${(assocFromProduct.estimateCalcMethod)!}</@td>
        <@td>&nbsp;${(assocFromProduct.routingWorkEffortId)!}</@td>
        <@td>
        <a href="<@ofbizUrl>UpdateProductBom?UPDATE_MODE=DELETE&amp;productId=${productId}&amp;productIdTo=${(assocFromProduct.productIdTo)!}&amp;productAssocTypeId=${(assocFromProduct.productAssocTypeId)!}&amp;fromDate=${(assocFromProduct.fromDate)!}&amp;useValues=true</@ofbizUrl>" class="${styles.button_default!}">${uiLabelMap.CommonDelete}</a>
        </@td>
        <@td>
        <a href="<@ofbizUrl>EditProductBom?productId=${productId}&amp;productIdTo=${(assocFromProduct.productIdTo)!}&amp;productAssocTypeId=${(assocFromProduct.productAssocTypeId)!}&amp;fromDate=${(assocFromProduct.fromDate)!}&amp;useValues=true</@ofbizUrl>" class="${styles.button_default!}">${uiLabelMap.CommonEdit}</a>
        </@td>
      </@tr>
    </#list>
    </@table>
  </@section>
  <@section title="${uiLabelMap.ManufacturingProductComponentOf}">
      <@table type="data-list" autoAltRows=true class="basic-table" cellspacing="0">
       <@thead>
        <@tr class="header-row">
            <@th>${uiLabelMap.ProductProductId}</@th>
            <@th>${uiLabelMap.ProductProductName}</@th>
            <@th>${uiLabelMap.CommonFromDate}</@th>
            <@th>${uiLabelMap.CommonThruDate}</@th>
            <@th>${uiLabelMap.CommonQuantity}</@th>
            <@th>&nbsp;</@th>
        </@tr>
        </@thead>
        <#list assocToProducts! as assocToProduct>
        <#assign listToProduct = assocToProduct.getRelatedOne("MainProduct", true)>
        <#assign curProductAssocType = assocToProduct.getRelatedOne("ProductAssocType", true)>
        <@tr>
            <@td><a href="<@ofbizUrl>EditProductBom?productId=${(assocToProduct.productId)!}&amp;productAssocTypeId=${(assocToProduct.productAssocTypeId)!}#components</@ofbizUrl>" class="${styles.button_default!}">${(assocToProduct.productId)!}</a></@td>
<#--                <@td><#if listToProduct??><a href="<@ofbizUrl>EditProduct?productId=${(assocToProduct.productId)!}</@ofbizUrl>" class="${styles.button_default!}">${(listToProduct.internalName)!}</a></#if></@td> -->
            <@td><#if listToProduct??><a href="<@ofbizUrl>EditProductBom?productId=${(assocToProduct.productId)!}&amp;productAssocTypeId=${(assocToProduct.productAssocTypeId)!}#components</@ofbizUrl>" class="${styles.button_default!}">${(listToProduct.internalName)!}</a></#if></@td>
            <@td>${(assocToProduct.getTimestamp("fromDate"))!}&nbsp;</@td>
            <@td>${(assocToProduct.getTimestamp("thruDate"))!}&nbsp;</@td>
            <@td>${(assocToProduct.quantity)!}&nbsp;</@td>
            <@td>
                <a href="<@ofbizUrl>UpdateProductBom?UPDATE_MODE=DELETE&amp;productId=${(assocToProduct.productId)!}&amp;productIdTo=${(assocToProduct.productIdTo)!}&amp;productAssocTypeId=${(assocToProduct.productAssocTypeId)!}&amp;fromDate=${Static["org.ofbiz.base.util.UtilFormatOut"].encodeQueryValue(assocToProduct.getTimestamp("fromDate").toString())}&amp;useValues=true</@ofbizUrl>" class="${styles.button_default!}">
                ${uiLabelMap.CommonDelete}</a>
            </@td>
        </@tr>
        </#list>
      </@table>
      
      <p>${uiLabelMap.CommonNote}: <b class="alert">${uiLabelMap.CommonRed}</b> ${uiLabelMap.ManufacturingNote1} <b style="color: red;">${uiLabelMap.CommonRed}</b>${uiLabelMap.ManufacturingNote2} <b style="color: red;">${uiLabelMap.CommonRed}</b>${uiLabelMap.ManufacturingNote3}<p>
  </@section>
</#if>
