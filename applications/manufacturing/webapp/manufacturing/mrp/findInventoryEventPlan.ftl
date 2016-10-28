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
function lookupInventory() {
    document.lookupinventory.submit();
}
</@script>
<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
      <#if requestParameters.hideFields?default("N") == "Y">
        <@menuitem type="link" href=makeOfbizUrl("FindInventoryEventPlan?hideFields=N${paramList}") text=uiLabelMap.CommonShowLookupFields class="+${styles.action_run_sys!} ${styles.action_show!}" />
      <#else>
        <#if inventoryList??>
            <@menuitem type="link" href=makeOfbizUrl("FindInventoryEventPlan?hideFields=Y${paramList}") text=uiLabelMap.CommonHideFields class="+${styles.action_run_sys!} ${styles.action_hide!}" />
        </#if>
      </#if>
  </@menu>
</#macro>
<@section title=uiLabelMap.PageTitleFindInventoryEventPlan menuContent=menuContent>
    <form method="post" name="lookupinventory" action="<@ofbizUrl>FindInventoryEventPlan</@ofbizUrl>">
    <input type="hidden" name="lookupFlag" value="Y"/>
    <input type="hidden" name="hideFields" value="Y"/>
      <#if requestParameters.hideFields?default("N") != "Y">
          <@field type="lookup" label=uiLabelMap.ManufacturingProductId value='${requestParameters.productId!}' formName="lookupinventory" name="productId" id="productId" fieldFormName="LookupProduct"/>
          <@field type="datetime" label=uiLabelMap.CommonFromDate name="eventDate" value=(requestParameters.eventDate!) size="25" maxlength="30" id="fromDate_2"/>
          <@field type="submit" submitType="link" href="javascript:lookupInventory();" class="+${styles.link_run_sys!} ${styles.action_find!}" text=uiLabelMap.CommonFind />
      </#if>
    </form>
</@section>

<#if requestParameters.hideFields?default("N") != "Y">
<@script>
document.lookupinventory.productId.focus();
</@script>
</#if>
<#if requestParameters.lookupFlag?default("N") == "Y">
    <@section>
      <#if inventoryList?has_content>
        <p class="${styles.float_left!}">${uiLabelMap.CommonElementsFound}</p>

    <#-- SCIPIO: FIXME?: I'm not sure this search works properly... -->
    <#assign paramStr = addParamsToStr(rawString(paramList!""), {"hideFields": requestParameters.hideFields!"N"}, "&amp;", false)>
    <@paginate mode="content" url=makeOfbizUrl("FindInventoryEventPlan") paramStr=paramStr viewSize=viewSize!1 viewIndex=viewIndex!0 listSize=listSize!0>
      <@table type="data-complex" autoAltRows=false> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
       <@thead>
        <@tr class="header-row">
          <@th>${uiLabelMap.CommonType}</@th>
          <@th align="center">&nbsp;</@th>
          <@th>${uiLabelMap.CommonDescription}</@th>
          <@th>${uiLabelMap.CommonDate}</@th>
          <@th align="center">&nbsp;</@th>
          <@th align="right">${uiLabelMap.CommonQuantity}</@th>
          <@th align="right">${uiLabelMap.ManufacturingTotalQuantity}</@th>
        </@tr>
        </@thead>
        <@tr type="util">
          <@td colspan="7"><hr /></@td>
        </@tr>
        <#assign count = lowIndex>
        <#assign productTmp = "">
        <#list inventoryList[lowIndex..highIndex-1] as inven>
            <#assign product = inven.getRelatedOne("Product", false)>
            <#if facilityId?has_content>
            </#if>
            <#if ! product.equals( productTmp )>
                <#assign quantityAvailableAtDate = 0>
                <#assign errorEvents = delegator.findByAnd("MrpEvent", {"mrpEventTypeId":"ERROR", "productId":inven.productId}, null, false)>
                <#assign qohEvents = delegator.findByAnd("MrpEvent", {"mrpEventTypeId":"INITIAL_QOH", "productId":inven.productId}, null, false)>
                <#assign additionalErrorMessage = "">
                <#assign initialQohEvent = "">
                <#assign productFacility = "">
                <#if qohEvents?has_content>
                    <#assign initialQohEvent = Static["org.ofbiz.entity.util.EntityUtil"].getFirst(qohEvents)>
                </#if>
                <#if initialQohEvent?has_content>
                    <#if initialQohEvent.quantity?has_content>
                        <#assign quantityAvailableAtDate = initialQohEvent.quantity>
                    </#if>
                    <#if initialQohEvent.facilityId?has_content>
                        <#assign productFacility = delegator.findOne("ProductFacility", {"facilityId":initialQohEvent.facilityId, "productId":inven.productId}, false)!>
                    </#if>
                <#else>
                    <#assign additionalErrorMessage = "No QOH information found, assuming 0.">
                </#if>
                <@tr bgcolor="lightblue">
                  <@th>
                      <b>[${inven.productId}]</b>&nbsp;&nbsp;${product.internalName!}
                  </@th>
                  <@td>
                    <#if productFacility?has_content>
                      <div>
                      <b>${uiLabelMap.ProductFacility}:</b>&nbsp;${productFacility.facilityId!}
                      </div>
                      <div>
                      <b>${uiLabelMap.ProductMinimumStock}:</b>&nbsp;${productFacility.minimumStock!}
                      </div>
                      <div>
                      <b>${uiLabelMap.ProductReorderQuantity}:</b>&nbsp;${productFacility.reorderQuantity!}
                      </div>
                      <div>
                      <b>${uiLabelMap.ProductDaysToShip}:</b>&nbsp;${productFacility.daysToShip!}
                      </div>
                      </#if>
                  </@td>
                  <@td colspan="5" align="right">
                    <big><b>${quantityAvailableAtDate}</b></big>
                  </@td>
                </@tr>
                <#if additionalErrorMessage?has_content>
                <@tr type="meta">
                    <@td colspan="7"><font color="red">${additionalErrorMessage}</font></@td>
                </@tr>
                </#if>
                <#list errorEvents as errorEvent>
                <@tr type="meta">
                    <@td colspan="7"><font color="red">${errorEvent.eventName!}</font></@td>
                </@tr>
                </#list>
            </#if>
            <#assign quantityAvailableAtDate = quantityAvailableAtDate?default(0) + inven.getBigDecimal("quantity")>
            <#assign productTmp = product>
            <#assign MrpEventType = inven.getRelatedOne("MrpEventType", false)>
            <@tr alt=true>
              <@td>${MrpEventType.get("description",locale)}</@td>
              <@td>&nbsp;</@td>
              <@td>${inven.eventName!}</@td>
              <@td><font<#if inven.isLate?default("N") == "Y"> color="red"</#if>>${inven.getString("eventDate")}</font></@td>
              <@td>&nbsp;</@td>
              <@td align="right">${inven.getString("quantity")}</@td>
              <@td align="right">${quantityAvailableAtDate!}</@td>
            </@tr>
            <#assign count=count+1>
           </#list>
       </@table>
      </@paginate>

      <#else>
       <@commonMsg type="result-norecord">${uiLabelMap.CommonNoElementFound}</@commonMsg>
      </#if>
    </@section>
</#if>
