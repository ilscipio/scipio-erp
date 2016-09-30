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
    function showQohAtp() {
        document.qohAtpForm.productId.value = document.quickaddform.add_product_id.value;
        document.qohAtpForm.submit();
    }
    function quicklookupGiftCertificate() {
        window.location='AddGiftCertificate';
    }
</@script>

<#if shoppingCart.getOrderType() == "PURCHASE_ORDER">
  <#assign target="productAvailabalityByFacility">
<#else>
  <#assign target="getProductInventoryAvailable">
</#if>

<@menu type="button">
    <@menuitem type="link" href="javascript:quicklookupGiftCertificate()" text=uiLabelMap.OrderAddGiftCertificate class="+${styles.action_nav!} ${styles.action_add!}" />
  <#if "PURCHASE_ORDER" == shoppingCart.getOrderType()>
    <@menuitem type="link" href="javascript:showQohAtp()" text=uiLabelMap.ProductAtpQoh class="+${styles.action_nav!} ${styles.action_view!}" />
  </#if>
</@menu>

<@section title="${rawString(uiLabelMap.CommonCreate)} ${rawString(uiLabelMap.OrderOrder)}">
      <#if shoppingCart.getOrderType() == "SALES_ORDER">
          <#if quantityOnHandTotal?? && availableToPromiseTotal?? && (productId)??>
            <@row>
              <@cell>
                ${uiLabelMap.ProductQuantityOnHand}: ${quantityOnHandTotal}
              </@cell>
              <@cell>
                ${uiLabelMap.ProductAvailableToPromise}: ${availableToPromiseTotal}
              </@cell>
            </@row>
          </#if>
      <#else>

        <#if parameters.availabalityList?has_content>
        <@row>
          <@cell>
          <@table type="data-list" autoAltRows=false> <#-- orig: class="" -->
            <@thead>
              <@tr>
                  <@th>${uiLabelMap.Facility}</@th>
                  <@th>${uiLabelMap.ProductQuantityOnHand}</@th>
                  <@th>${uiLabelMap.ProductAvailableToPromise}</@th>
              </@tr>
            </@thead>
            <@tbody>
            <#list parameters.availabalityList as availabality>
               <@tr>
                 <@td>${availabality.facilityId}</@td>
                 <@td>${availabality.quantityOnHandTotal}</@td>
                 <@td>${availabality.availableToPromiseTotal}</@td>
               </@tr>
            </#list>
            </@tbody>
          </@table>
          </@cell>
        </@row>
        </#if>
        
      </#if>
      
      <@row>
        <@cell>
          <#-- Scipio: NOTE (2016-02-03): default-alt1 automatically expresses alternate checkbox style. see styles hash. -->
          <@fields type="default-alt1">
            <form name="qohAtpForm" method="post" action="<@ofbizUrl>${target}</@ofbizUrl>">
                <input type="hidden" name="facilityId" value="${facilityId!}"/>
                <input type="hidden" name="productId"/>
                <input type="hidden" id="ownerPartyId" name="ownerPartyId" value="${shoppingCart.getBillToCustomerPartyId()!}" />
            </form>
              
            <form method="post" action="<@ofbizUrl>additem</@ofbizUrl>" name="quickaddform">
                <#if orderType=="PURCHASE_ORDER">
                  <#if supplierPartyId?has_content><#-- SCIPIO: use supplier not partyId: partId?has_content -->                                               
                    <#assign fieldFormName="LookupSupplierProduct?partyId=${supplierPartyId}">
                  <#else>
                    <#assign fieldFormName="LookupSupplierProduct">
                  </#if>
                <#else>
                  <#assign fieldFormName="LookupProduct">
                </#if>
                      
                <@field type="lookup" formName="quickaddform" name="add_product_id" id="add_product_id" fieldFormName=fieldFormName label=uiLabelMap.ProductProductId/>
                <@field type="input" size="6" name="quantity" value="1" label=uiLabelMap.OrderQuantity/>

                <#if useAsDefaultDesiredDeliveryDate??> 
                  <#assign value = defaultDesiredDeliveryDate>
                </#if>
                <@field type="datetime" dateType="datetime" label=uiLabelMap.OrderDesiredDeliveryDate name="itemDesiredDeliveryDate" value=(value!) size="25" maxlength="30" id="additem_itemDesiredDeliveryDate" />
                <@field type="checkbox" name="useAsDefaultDesiredDeliveryDate" value="true" checked=(useAsDefaultDesiredDeliveryDate??) label=uiLabelMap.OrderUseDefaultDesiredDeliveryDate />
                <@field type="datetime" dateType="datetime" label=uiLabelMap.OrderShipAfterDate name="shipAfterDate" value=(shoppingCart.getDefaultShipAfterDate()!) size="25" maxlength="30" id="additem_shipAfterDate" />
                <@field type="datetime" dateType="datetime" label=uiLabelMap.OrderShipBeforeDate name="shipBeforeDate" value=(shoppingCart.getDefaultShipBeforeDate()!) size="25" maxlength="30" id="additem_shipBeforeDate"/>
               
              <#if shoppingCart.getOrderType() == "PURCHASE_ORDER">
                <@field type="select" label=uiLabelMap.OrderOrderItemType name="add_item_type">
                    <option value="">&nbsp;</option>
                    <#list purchaseOrderItemTypeList as orderItemType>
                    <option value="${orderItemType.orderItemTypeId}">${orderItemType.description}</option>
                    </#list>
                </@field>
              </#if>
                <@field type="checkbox" name="useAsDefaultComment" value="true" checked=useAsDefaultComment?? label=uiLabelMap.OrderUseDefaultComment />
                <@field type="input" size="25" name="itemComment" value=(defaultComment!) label=uiLabelMap.CommonComment/>
                <@field type="submit" class="+${styles.link_run_session!} ${styles.action_add!}" text=uiLabelMap.OrderAddToOrder/>
            </form>

        <#if shoppingCart.getOrderType() == "PURCHASE_ORDER">
            <form method="post" action="<@ofbizUrl>additem</@ofbizUrl>" name="bulkworkaddform">
                <@field type="select" label=uiLabelMap.OrderOrderItemType name="add_item_type">
                  <option value="BULK_ORDER_ITEM">${uiLabelMap.ProductBulkItem}</option><option value="WORK_ORDER_ITEM">${uiLabelMap.ProductWorkItem}</option>
                </@field>
                
                <@field type="lookup" formName="bulkworkaddform" value=(requestParameters.add_category_id!) name="add_category_id" id="add_category_id" fieldFormName="LookupProductCategory" label=uiLabelMap.ProductProductCategory/>
                
                <@field type="input" size="25" name="add_item_description" value="" label=uiLabelMap.CommonDescription/>
                <@field type="input" size="3" name="quantity" value=requestParameters.quantity?default('1') label=uiLabelMap.OrderQuantity/>
                <@field type="input" size="6" name="price" value=(requestParameters.price!) label=uiLabelMap.OrderPrice/>
                    
                <@field type="submit" class="+${styles.link_run_session!} ${styles.action_add!}" text=uiLabelMap.OrderAddToOrder/>
            </form>
        </#if>

        </@fields>
      </@cell>
   </@row>
</@section>

<@script>
  document.quickaddform.add_product_id.focus();
</@script>

<!-- Internal cart info: productStoreId=${shoppingCart.getProductStoreId()!} locale=${shoppingCart.getLocale()!} currencyUom=${shoppingCart.getCurrency()!} userLoginId=${(shoppingCart.getUserLogin().getString("userLoginId"))!} autoUserLogin=${(shoppingCart.getAutoUserLogin().getString("userLoginId"))!} -->
