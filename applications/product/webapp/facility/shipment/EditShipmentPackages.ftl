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

<#if shipment??>
<@section title="${uiLabelMap.PageTitleEditShipmentPackages}">
        <@table type="data-complex" autoAltRows=false cellspacing="0"> <#-- orig: class="basic-table" -->
          <@thead>
            <@tr class="header-row">
                <@th>${uiLabelMap.ProductPackage}</@th>
                <@th>${uiLabelMap.CommonCreated}</@th>
                <@th>&nbsp;</@th>
                <@th>&nbsp;</@th>
                <@th>&nbsp;</@th>
                <@th>&nbsp;</@th>
            </@tr>
          </@thead>
        <#assign alt_row = false>
        <#list shipmentPackageDatas as shipmentPackageData>
            <#assign shipmentPackage = shipmentPackageData.shipmentPackage>
            <#assign shipmentPackageContents = shipmentPackageData.shipmentPackageContents!>
            <#assign shipmentPackageRouteSegs = shipmentPackageData.shipmentPackageRouteSegs!>
            <#assign weightUom = shipmentPackageData.weightUom!>
            <form method="post" action="<@ofbizUrl>updateShipmentPackage</@ofbizUrl>" name="updateShipmentPackageForm${shipmentPackageData_index}">
            <input type="hidden" name="shipmentId" value="${shipmentId}"/>
            <input type="hidden" name="shipmentPackageSeqId" value="${shipmentPackage.shipmentPackageSeqId}"/>
            <@tr valign="middle" alt=alt_row>
                <@td>${shipmentPackage.shipmentPackageSeqId}</@td>
                <@td>${(shipmentPackage.dateCreated.toString())!}</@td>
                <@td>
                    <span>${uiLabelMap.ProductWeight}</span>
                    <input type="text" size="5" name="weight" value="${shipmentPackage.weight!}"/>
                    <span>${uiLabelMap.ProductWeightUnit}</span>
                    <select name="weightUomId">
                        <#if weightUom?has_content>
                            <option value="${weightUom.uomId}">${weightUom.get("description",locale)}</option>
                            <option value="${weightUom.uomId}">---</option>
                        <#else>
                            <option value="">&nbsp;</option>
                        </#if>
                        <#list weightUoms as weightUomOption>
                            <option value="${weightUomOption.uomId}">${weightUomOption.get("description",locale)} [${weightUomOption.abbreviation}]</option>
                        </#list>
                    </select>
                </@td>
                <@td>
                    <span>${uiLabelMap.ProductShipmentBoxType}</span>
                    <select name="shipmentBoxTypeId">
                        <option value="">&nbsp;</option>
                        <#list boxTypes as boxType>
                            <option value="${boxType.shipmentBoxTypeId}" <#if shipmentPackage.shipmentBoxTypeId?? && shipmentPackage.shipmentBoxTypeId == boxType.shipmentBoxTypeId>selected="selected"</#if>>${boxType.get("description",locale)}</option>
                        </#list>
                    </select>
                    <span>${uiLabelMap.ProductShipmentInsuredValuePackage}</span>
                    <input type="text" size="5" name="insuredValue" value="${shipmentPackage.insuredValue!}"/>
                </@td>
                <@td><a href="javascript:document.updateShipmentPackageForm${shipmentPackageData_index}.submit();" class="${styles.link_run_sys!} ${styles.action_update!}">${uiLabelMap.CommonUpdate}</a></@td>
                <@td><a href="javascript:document.deleteShipmentPackage_${shipmentPackageData_index}.submit();" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonDelete}</a></@td>
            </@tr>
            </form>
            <form name="deleteShipmentPackage_${shipmentPackageData_index}" method="post" action="<@ofbizUrl>deleteShipmentPackage</@ofbizUrl>">
                <input type="hidden" name="shipmentId" value="${shipmentId}"/>
                <input type="hidden" name="shipmentPackageSeqId" value="${shipmentPackage.shipmentPackageSeqId}"/>
            </form>
        <#list shipmentPackageContents as shipmentPackageContent>
            <@tr valign="middle" alt=alt_row>
                <@td>&nbsp;</@td>
                <@td><span>${uiLabelMap.ProductItem}</span> ${shipmentPackageContent.shipmentItemSeqId}</@td>
                <@td colspan="2">
                        <span>${uiLabelMap.ProductQuantity}</span>
                        ${shipmentPackageContent.quantity!}
                        <a href="javascript:document.deleteShipmentPackageContent${shipmentPackageData_index}${shipmentPackageContent_index}.submit();" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonDelete}</a>
                </@td>
                <@td>&nbsp;</@td>
                <@td>&nbsp;</@td>
            </@tr>
            <form name="deleteShipmentPackageContent${shipmentPackageData_index}${shipmentPackageContent_index}" method="post" action="<@ofbizUrl>deleteShipmentPackageContent</@ofbizUrl>">
                <input type="hidden" name="shipmentId" value="${shipmentId}"/>
                <input type="hidden" name="shipmentPackageSeqId" value="${shipmentPackageContent.shipmentPackageSeqId}"/>
                <input type="hidden" name="shipmentItemSeqId" value="${shipmentPackageContent.shipmentItemSeqId}"/>
            </form>
        </#list>
            <@tr valign="middle" alt=alt_row>
                <form name="createShipmentPackageContentForm${shipmentPackageData_index}" method="post" action="<@ofbizUrl>createShipmentPackageContent</@ofbizUrl>">
                <input type="hidden" name="shipmentId" value="${shipmentId}"/>
                <input type="hidden" name="shipmentPackageSeqId" value="${shipmentPackage.shipmentPackageSeqId}"/>
                <@td>&nbsp;</@td>
                <@td>
                        <span>${uiLabelMap.ProductAddFromItem}</span>
                        <select name="shipmentItemSeqId">
                            <#list shipmentItems as shipmentItem>
                                <option>${shipmentItem.shipmentItemSeqId}</option>
                            </#list>
                        </select>
                </@td>
                <@td colspan="2">
                        <span>${uiLabelMap.ProductQuantity}</span>
                        <input type="text" name="quantity" size="5" value="0"/>
                        <a href="javascript:document.createShipmentPackageContentForm${shipmentPackageData_index}.submit()" class="${styles.link_run_sys!} ${styles.action_add!}">${uiLabelMap.CommonAdd}</a>
                </@td>
                <@td>&nbsp;</@td>
                <@td>&nbsp;</@td>
                </form>
            </@tr>
        <#list shipmentPackageRouteSegs as shipmentPackageRouteSeg>
            <form action="<@ofbizUrl>updateShipmentPackageRouteSeg</@ofbizUrl>" method="post" name="updateShipmentPackageRouteSegForm${shipmentPackageData_index}${shipmentPackageRouteSeg_index}">
            <input type="hidden" name="shipmentId" value="${shipmentId}"/>
            <input type="hidden" name="shipmentRouteSegmentId" value="${shipmentPackageRouteSeg.shipmentRouteSegmentId}"/>
            <input type="hidden" name="shipmentPackageSeqId" value="${shipmentPackageRouteSeg.shipmentPackageSeqId}"/>
            <@tr valign="middle" alt=alt_row>
                <@td>&nbsp;</@td>
                <@td><span>${uiLabelMap.ProductRouteSegment}</span> ${shipmentPackageRouteSeg.shipmentRouteSegmentId}</@td>
                <@td><span>${uiLabelMap.ProductTrack}</span> <input type="text" size="22" name="trackingCode" value="${shipmentPackageRouteSeg.trackingCode!}"/></@td>
                <@td>
                        <span>${uiLabelMap.ProductBox}</span>
                        <input type="text" size="5" name="boxNumber" value="${shipmentPackageRouteSeg.boxNumber!}"/>
                        <a href="javascript:document.updateShipmentPackageRouteSegForm${shipmentPackageData_index}${shipmentPackageRouteSeg_index}.submit();" class="${styles.link_run_sys!} ${styles.action_update!}">${uiLabelMap.CommonUpdate}</a>
                        <a href="javascript:document.deleteShipmentPackageRouteSeg${shipmentPackageData_index}${shipmentPackageRouteSeg_index}.submit();" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonDelete}</a>
                </@td>
                <@td>&nbsp;</@td>
            </@tr>
            </form>
            <form name="deleteShipmentPackageRouteSeg${shipmentPackageData_index}${shipmentPackageRouteSeg_index}" method="post" action="<@ofbizUrl>deleteShipmentPackageRouteSeg</@ofbizUrl>">
                <input type="hidden" name="shipmentId" value="${shipmentId}"/>
                <input type="hidden" name="shipmentPackageSeqId" value="${shipmentPackageRouteSeg.shipmentPackageSeqId}"/>
                <input type="hidden" name="shipmentRouteSegmentId" value="${shipmentPackageRouteSeg.shipmentRouteSegmentId}"/>
            </form>
        </#list>
            <#--
            <@tr valign="middle" alt=alt_row>
                <form action="<@ofbizUrl>createShipmentPackageRouteSeg</@ofbizUrl>" name="createShipmentPackageRouteSegForm${shipmentPackageData_index}">
                <input type="hidden" name="shipmentId" value="${shipmentId}"/>
                <input type="hidden" name="shipmentPackageSeqId" value="${shipmentPackage.shipmentPackageSeqId}"/>
                <@td>&nbsp;</@td>
                <@td><span>${uiLabelMap.ProductAddRouteSegmentInfo}</span>
                    <select name="shipmentRouteSegmentId">
                        <#list shipmentRouteSegments as shipmentRouteSegment>
                            <option>${shipmentRouteSegment.shipmentRouteSegmentId}</option>
                        </#list>
                    </select>
                </@td>
                <@td><span>Track ${uiLabelMap.CommonNbr}</span><input type="text" size="22" name="trackingCode"/></@td>
                <@td><span>Box ${uiLabelMap.CommonNbr}</span><input type="text" size="5" name="boxNumber"/></@td>
                <@td><a href="javascript:document.createShipmentPackageRouteSegForm${shipmentPackageData_index}.submit()" class="${styles.link_run_sys!} ${styles.action_add!}">${uiLabelMap.CommonAdd}</a></@td>
                <@td>&nbsp;</@td>
                </form>
            </@tr>
            -->
            <#-- toggle the row color -->
            <#assign alt_row = !alt_row>
        </#list>
        <#--
        <form action="<@ofbizUrl>createShipmentPackage</@ofbizUrl>" name="createShipmentPackageForm">
            <input type="hidden" name="shipmentId" value="${shipmentId}"/>
            <@tr>
                <@td>${uiLabelMap.ProductNewPackage}</@td>
                <@td>&nbsp;</@td>
                <@td><span>${uiLabelMap.ProductWeight}</span> <input type="text" size="5" name="weight"/></@td>
                <@td><span>${uiLabelMap.ProductWeightUnit}</span>
                    <select name="weightUomId">
                        <#list weightUoms as weightUomOption>
                            <option value="${weightUomOption.uomId}">${weightUomOption.get("description",locale)} [${weightUomOption.abbreviation}]</option>
                        </#list>
                    </select>
                </@td>
                <@td><a href="javascript:document.createShipmentPackageForm.submit();" class="${styles.link_run_sys!} ${styles.action_add!}">${uiLabelMap.CommonCreate}</a></@td>
                <@td>&nbsp;</@td>
            </@tr>
        </form>
        -->
        </@table>
</@section>
<#else>
  <@section>
    <@errorMsg>${uiLabelMap.ProductShipmentNotFoundId} : [${shipmentId!}]</@errorMsg>
  </@section>
</#if>