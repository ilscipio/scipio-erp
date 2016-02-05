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
<#if shipmentPackageDatas?has_content>
<@section>
      <@table type="data-complex" class="+${styles.table_spacing_tiny_hint!}" autoAltRows=true> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" --> <#-- orig: cellpadding="2" -->
       <@thead>
        <@tr class="header-row">
          <@th>${uiLabelMap.ProductPackage}</@th>
          <@th>${uiLabelMap.CommonCreated}</@th>
          <@th>&nbsp;</@th>
          <@th>&nbsp;</@th>
        </@tr>
        </@thead>
        <#list shipmentPackageDatas as shipmentPackageData>
          <#assign shipmentPackage = shipmentPackageData.shipmentPackage>
          <#assign shipmentPackageContents = shipmentPackageData.shipmentPackageContents!>
          <#assign shipmentPackageRouteSegs = shipmentPackageData.shipmentPackageRouteSegs!>
          <#assign weightUom = shipmentPackageData.weightUom!>
          <@tr valign="middle">
            <@td>${shipmentPackage.shipmentPackageSeqId}</@td>
            <@td>${(shipmentPackage.dateCreated.toString())!}</@td>
            <@td><span>${uiLabelMap.ProductWeight}</span> ${shipmentPackage.weight!}</@td>
            <@td><span>${uiLabelMap.ProductWeightUnit}</span> <#if weightUom?has_content>${weightUom.get("description",locale)}<#else>${shipmentPackage.weightUomId!}</#if></@td>
          </@tr>
          <#list shipmentPackageContents as shipmentPackageContent>
            <@tr valign="middle" groupLast=true>
              <@td>&nbsp;</@td>
              <@td><span>${uiLabelMap.ProductItem}</span> ${shipmentPackageContent.shipmentItemSeqId}</@td>
              <@td><span>${uiLabelMap.ProductQuantity}</span> ${shipmentPackageContent.quantity!}</@td>
              <@td>&nbsp;</@td>
            </@tr>
          </#list>
          <#list shipmentPackageRouteSegs as shipmentPackageRouteSeg>
            <@tr valign="middle" groupLast=true>
              <@td>&nbsp;</@td>
              <@td><span>${uiLabelMap.ProductRouteSegment}</span> ${shipmentPackageRouteSeg.shipmentRouteSegmentId}</@td>
              <@td><span>${uiLabelMap.ProductTracking}</span> ${shipmentPackageRouteSeg.trackingCode!}</@td>
              <@td><span>${uiLabelMap.ProductBox}</span> ${shipmentPackageRouteSeg.boxNumber!}</@td>
            </@tr>
          </#list>
        </#list>
      </@table>
</@section>
</#if>