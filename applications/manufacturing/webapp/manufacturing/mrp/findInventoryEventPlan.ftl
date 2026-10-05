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
<#--
Changes to this file: Copyright (C) Ilscipio GmbH. The changes are licensed
under the GNU Affero General Public License, version 3, or a commercial
license from Ilscipio GmbH (file LICENSE). The original code stays under
the Apache License, version 2.0, as stated above.
-->

<@script>
function lookupInventory() {
    document.lookupinventory.submit();
}
</@script>
<#-- SCIPIO: turns the raw MrpEvent.eventName into a link to the source record, per event type -->
<#macro mrpEventDescLink evt>
  <#-- SCIPIO: use getString() (raw), not dot-access, to avoid the auto-sanitized (HTML-entity-encoded) value -->
  <#assign evtTypeId = evt.getString("mrpEventTypeId")!"">
  <#assign evtName = evt.getString("eventName")!"">
  <#if !evtName?has_content>
  <#-- nothing to link -->
  <#elseif evtTypeId == "SALES_ORDER_SHIP" || evtTypeId == "PUR_ORDER_RECP">
    <#assign dashIdx = evtName?index_of("-")>
    <#assign orderId = (dashIdx > -1)?then(evtName?substring(0, dashIdx), evtName)>
    <a href="<@serverUrl>/ordermgr/control/orderview?orderId=${orderId}${raw(externalKeyParam!"")}</@serverUrl>" class="${styles.link_nav_info_id!}">${evtName}</a>
  <#elseif evtTypeId == "PROD_REQ_RECP">
    <a href="<@serverUrl>/ordermgr/control/EditRequirement?requirementId=${evtName}${raw(externalKeyParam!"")}</@serverUrl>" class="${styles.link_nav_info_id!}">${evtName}</a>
  <#elseif evtTypeId == "MANUF_ORDER_REQ" || evtTypeId == "MANUF_ORDER_RECP">
    <#assign dashIdx = evtName?index_of("-")>
    <#assign prodRunId = (dashIdx > -1)?then(evtName?substring(0, dashIdx), evtName)>
    <a href="<@pageUrl>ShowProductionRun?productionRunId=${prodRunId}</@pageUrl>" class="${styles.link_nav_info_id!}">${evtName}</a>
  <#elseif evtTypeId == "PROP_MANUF_O_RECP" || evtTypeId == "PROP_PUR_O_RECP">
    <#-- SCIPIO: eventName format is "*requirementId (date)*"; the leading/trailing "*" marker may be stored HTML-entity-encoded -->
    <#assign reqId = evtName?replace("&#x2a;", "", "i")?replace("&#42;", "")?replace("*", "")?trim>
    <#assign spaceIdx = reqId?index_of(" ")>
    <#assign reqId = (spaceIdx > -1)?then(reqId?substring(0, spaceIdx), reqId)>
    <a href="<@serverUrl>/ordermgr/control/EditRequirement?requirementId=${reqId}${raw(externalKeyParam!"")}</@serverUrl>" class="${styles.link_nav_info_id!}">${evtName}</a>
  <#else>
    ${evtName}
  </#if>
</#macro>
<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
      <a href="<@pageUrl>MrpRuns</@pageUrl>" class="${styles.link_nav!} ${styles.action_view!}">${uiLabelMap.PageTitleMrpRuns}</a>
      <#if requestParameters.hideFields?default("N") == "Y">
        <@menuitem type="link" href=makePageUrl("FindInventoryEventPlan?hideFields=N${paramList}") text=uiLabelMap.CommonShowLookupFields class="+${styles.action_run_sys!} ${styles.action_show!}" />
      <#else>
        <#if inventoryList??>
            <@menuitem type="link" href=makePageUrl("FindInventoryEventPlan?hideFields=Y${paramList}") text=uiLabelMap.CommonHideFields class="+${styles.action_run_sys!} ${styles.action_hide!}" />
        </#if>
      </#if>
  </@menu>
</#macro>
<@section title=uiLabelMap.PageTitleFindInventoryEventPlan menuContent=menuContent>
    <form method="post" name="lookupinventory" action="<@pageUrl>FindInventoryEventPlan</@pageUrl>">
    <input type="hidden" name="lookupFlag" value="Y"/>
    <input type="hidden" name="hideFields" value="Y"/>
      <#if requestParameters.hideFields?default("N") != "Y">
          <@field type="lookup" label=uiLabelMap.ManufacturingProductId value='${requestParameters.productId!}' formName="lookupinventory" name="productId" id="productId" fieldFormName="LookupProduct"/>
          <#assign selectedMrpId = requestParameters.mrpId!(defaultMrpId!"")>
          <@field type="select" label=uiLabelMap.ManufacturingMrpRuns name="mrpId">
            <option value=""></option>
            <#list mrpRunList![] as mrpRunOpt>
              <option value="${mrpRunOpt.mrpId}"<#if selectedMrpId == mrpRunOpt.mrpId> selected="selected"</#if>>${mrpRunOpt.mrpId}<#if mrpRunOpt.mrpName?has_content> - ${mrpRunOpt.mrpName}</#if></option>
            </#list>
          </@field>
          <@field type="select" label=uiLabelMap.ProductFacility name="facilityId" allowEmpty=true>
            <option value=""></option>
            <#list facilityList![] as facilityOpt>
              <option value="${facilityOpt.facilityId}"<#if (requestParameters.facilityId!"") == facilityOpt.facilityId> selected="selected"</#if>>${facilityOpt.facilityName!} [${facilityOpt.facilityId}]</option>
            </#list>
          </@field>
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
<#if showResults!false>
    <@section>
      <#if inventoryList?has_content>
        <p class="${styles.float_left!}">${uiLabelMap.CommonElementsFound}</p>

    <#assign paramStr = addParamsToStr(raw(paramList!""), {"hideFields": requestParameters.hideFields!"N"}, "&amp;", false)>
    <@paginate mode="content" url=makePageUrl("FindInventoryEventPlan") paramStr=paramStr viewSize=viewSize!1 viewIndex=viewIndex!0 listSize=listSize!0>
      <@table type="data-complex" autoAltRows=false>
       <@thead>
        <@tr class="header-row">
          <@th>${uiLabelMap.CommonType}</@th>
          <@th align="center">&nbsp;</@th>
          <@th>${uiLabelMap.CommonDescription}</@th>
          <@th>${uiLabelMap.CommonDate}</@th>
          <@th align="center">${uiLabelMap.ManufacturingIsLate}</@th>
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
                <@tr class="${styles.color_info!}">
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
                    <@td colspan="7"><span class="${styles.text_color_alert!}">${additionalErrorMessage}</span></@td>
                </@tr>
                </#if>
                <#list errorEvents as errorEvent>
                <@tr type="meta">
                    <@td colspan="7"><span class="${styles.text_color_alert!}">${errorEvent.eventName!}</span></@td>
                </@tr>
                </#list>
            </#if>
            <#assign quantityAvailableAtDate = quantityAvailableAtDate?default(0) + inven.getBigDecimal("quantity")>
            <#assign productTmp = product>
            <#assign MrpEventType = inven.getRelatedOne("MrpEventType", false)>
            <@tr alt=true>
              <@td>${MrpEventType.get("description",locale)}</@td>
              <@td>&nbsp;</@td>
              <@td><@mrpEventDescLink evt=inven/></@td>
              <@td><span<#if inven.isLate?default("N") == "Y"> class="${styles.text_color_alert!}"</#if>>${inven.getString("eventDate")}</span></@td>
              <@td align="center"><#if inven.isLate?default("N") == "Y"><span class="${styles.text_color_alert!}">${uiLabelMap.ManufacturingIsLate}</span></#if></@td>
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
