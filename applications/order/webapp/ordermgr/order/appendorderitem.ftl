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

<@script>
  function quicklookup(element) {
    window.location='<@ofbizUrl>LookupBulkAddSupplierProductsInApprovedOrder</@ofbizUrl>?orderId='+element.value;
  }
</@script>

<#if orderHeader?has_content>
  <@section title=uiLabelMap.OrderAddToOrder>
    <@row>
      <@cell columns=6>
        <form method="post" action="<@ofbizUrl>appendItemToOrder</@ofbizUrl>" name="appendItemForm">
          <input type="hidden" size="25" name="orderId" value="${orderId!}"/>
          <#if !catalogCol?has_content>
              <input type="hidden" name="prodCatalogId" value=""/>
          </#if>
          <#if catalogCol?has_content && catalogCol?size == 1>
              <input type="hidden" name="prodCatalogId" value="${catalogCol.first}"/>
          </#if>
          <#if shipGroups?size == 1>
              <input type="hidden" name="shipGroupSeqId" value="${shipGroups.first.shipGroupSeqId}"/>
          </#if>
             
          <#if catalogCol?has_content && (catalogCol?size > 1)>
            <@field type="select" name="prodCatalogId" label=uiLabelMap.ProductChooseCatalog>
                <#list catalogCol as catalogId>
                  <#assign thisCatalogName = Static["org.ofbiz.product.catalog.CatalogWorker"].getCatalogName(request, catalogId)>
                  <option value="${catalogId}">${thisCatalogName}</option>
                </#list>
            </@field>
          </#if>
            <@field type="generic" label=uiLabelMap.ProductProductId>
                <#-- FIXME Problem here: the input field is shared -->
                  <@field type="lookup" formName="appendItemForm" name="productId" id="productId" fieldFormName="LookupProduct"/>
                  <#if "PURCHASE_ORDER" == orderHeader.orderTypeId>
                      <a href="javascript:quicklookup(document.appendItemForm.orderId)" class="${styles.link_nav!} ${styles.action_find!}">${uiLabelMap.OrderQuickLookup}</a>
                  </#if>
            </@field>
            <@field type="generic" label=uiLabelMap.OrderPrice>
                <@field type="input" size="6" name="basePrice" value=(requestParameters.price!)/>
                <@field type="checkbox" name="overridePrice" value="Y" label=uiLabelMap.OrderOverridePrice/>
            </@field>
            <@field type="input" label=uiLabelMap.OrderQuantity size="6" name="quantity" value=requestParameters.quantity!"1"/>
          <#if (shipGroups?size > 1)>
            <@field type="select" label=uiLabelMap.OrderShipGroup name="shipGroupSeqId">
              <#list shipGroups as shipGroup>
                 <option value="${shipGroup.shipGroupSeqId}">${shipGroup.shipGroupSeqId}</option>
              </#list>
            </@field>
          </#if>
            <@field type="datetime" label=uiLabelMap.OrderDesiredDeliveryDate name="itemDesiredDeliveryDate" size="25" maxlength="30" id="itemDesiredDeliveryDate1" />
            <@field type="select" label=uiLabelMap.OrderReturnReason name="reasonEnumId">
                <option value="">&nbsp;</option>
                <#list orderItemChangeReasons as reason>
                <option value="${reason.enumId}">${reason.get("description",locale)?default(reason.enumId)}</option>
                </#list>
            </@field>
            <#if orderHeader.orderTypeId == "PURCHASE_ORDER" && purchaseOrderItemTypeList?has_content>
              <@field type="select" label=uiLabelMap.OrderOrderItemType name="orderItemTypeId">
                  <option value="">&nbsp;</option>
                  <#list purchaseOrderItemTypeList as orderItemType>
                    <option value="${orderItemType.orderItemTypeId}">${orderItemType.description}</option>
                  </#list>
              </@field>
            </#if>
            <@field type="input" label=uiLabelMap.CommonComment size="25" name="changeComments"/>
            <@field type="submit" text=uiLabelMap.OrderAddToOrder class="${styles.link_run_sys!} ${styles.action_add!}"/>
        </form>
      </@cell>
    </@row>
  </@section>
</#if>