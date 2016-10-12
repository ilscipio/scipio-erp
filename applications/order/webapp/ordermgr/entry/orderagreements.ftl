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


<form method="post" name="agreementForm" action="<@ofbizUrl>setOrderCurrencyAgreementShipDates</@ofbizUrl>">

  <#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
      <@menuitem type="link" href="javascript:document.agreementForm.submit()" text=uiLabelMap.CommonContinue class="+${styles.action_run_session!} ${styles.action_continue!}" />
    </@menu>
  </#macro>
  <@section title=uiLabelMap.OrderOrderEntryCurrencyAgreementShipDates menuContent=menuContent>
    <#if agreements??>
      <input type="hidden" name="hasAgreements" value="Y"/>
    <#else>
      <input type="hidden" name="hasAgreements" value="N"/>
    </#if>
    
    <@table type="fields"> <#-- orig: class="basic-table" -->
      <#if agreements??>     
      <@field type="select" label=uiLabelMap.OrderSelectAgreement name="agreementId">
            <option value="">${uiLabelMap.CommonNone}</option>
            <#list agreements as agreement>
            <option value="${agreement.agreementId}">${agreement.agreementId} - ${agreement.description!}</option>
            </#list>
      </@field>
      </#if>
      <#if agreementRoles??>
        <@field type="select" label=uiLabelMap.OrderSelectAgreementRoles name="agreementId">
              <option value="">${uiLabelMap.CommonNone}</option>
              <#list agreementRoles as agreementRole>
                  <option value="${agreementRole.agreementId!}">${agreementRole.agreementId!} - ${agreementRole.roleTypeId!}</option>
              </#list>
        </@field>
      </#if>

      <#if "PURCHASE_ORDER" == cart.getOrderType()>
        <@field type="input" label=uiLabelMap.OrderOrderId size="15" maxlength="100" name="orderId" value=""/>
      </#if>

      <@field type="input" label=uiLabelMap.OrderOrderName size="60" maxlength="100" name="orderName"/>

    <#if cart.getOrderType() != "PURCHASE_ORDER">
      <@field type="input" label=uiLabelMap.OrderPONumber name="correspondingPoId" size="15" />
    </#if>
      
      <#if agreements??>
        <#assign fieldLabel = uiLabelMap.OrderSelectCurrencyOr>
      <#else>
        <#assign fieldLabel = uiLabelMap.OrderSelectCurrency>
      </#if>
      <@field type="select" label=fieldLabel name="currencyUomId">
          <option value=""></option>
          <#list currencies as currency>
          <option value="${currency.uomId}" <#if currencyUomId?default('') == currency.uomId>selected="selected"</#if> >${currency.uomId}</option>
          </#list>
      </@field>

    <#if catalogCol?has_content>
      <@field type="select" name="CURRENT_CATALOG_ID" label=uiLabelMap.ProductChooseCatalog>
        <#list catalogCol! as catalogId>
          <#assign thisCatalogName = Static["org.ofbiz.product.catalog.CatalogWorker"].getCatalogName(request, catalogId)>
          <option value="${catalogId}" <#if (currentCatalogId!'') == catalogId>selected="selected"</#if> >${thisCatalogName}</option>
        </#list>
      </@field>
    <#else>
      <input type="hidden" name="CURRENT_CATALOG_ID" value=""/> 
    </#if>

      <@field type="lookup" label=uiLabelMap.WorkEffortWorkEffortId formName="agreementForm" name="workEffortId" id="workEffortId" fieldFormName="LookupWorkEffort"/>

      <@field type="datetime" label=uiLabelMap.OrderShipAfterDateDefault name="shipAfterDate" value="" size="25" maxlength="30" id="shipAfterDate1"/>

      <@field type="datetime" label=uiLabelMap.OrderShipBeforeDateDefault name="shipBeforeDate" value="" size="25" maxlength="30" id="shipBeforeDate1"/>

      <#if cart.getOrderType() == "PURCHASE_ORDER">
        <@field type="datetime" label=uiLabelMap.FormFieldTitle_cancelBackOrderDate name="cancelBackOrderDate" value="" size="25" maxlength="30" id="cancelBackOrderDate1"/>
      </#if>

    </@table>
  </@section>
</form>
