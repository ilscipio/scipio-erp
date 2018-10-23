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
    function quicklookup(func, locationelement, facilityelement, productelement) {
        
        var productId = productelement.value;
        if (productId.length == 0) {
          alert("${rawLabel('ProductFieldEmpty')?js_string}");
          return;
        }
        var facilityId = facilityelement.value;
        var request = "LookupProductInventoryLocation?productId=" + productId + "&facilityId=" + facilityId;
        window[func](locationelement, request);
    }
</@script>

<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
    <@menuitem type="link" href=makeOfbizUrl("PickMoveStockSimple?facilityId=${facilityId!}") text=uiLabelMap.CommonPrint class="+${styles.action_run_sys!} ${styles.action_export!}" />
  </@menu>
</#macro>
<@section title=uiLabelMap.ProductStockMovesNeeded menuContent=menuContent>
        <#if moveByOisgirInfoList?has_content || moveByPflInfoList?has_content>
          <form method="post" action="<@ofbizUrl>processPhysicalStockMove</@ofbizUrl>" name="selectAllForm">
              <#-- general request fields -->
              <input type="hidden" name="facilityId" value="${facilityId!}" />
              <input type="hidden" name="_useRowSubmit" value="Y" />
              <#assign rowCount = 0>
              <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table hover-bar" --> <#-- orig: cellspacing="0" -->
               <@thead>
                <@tr class="header-row">
                    <@th>${uiLabelMap.ProductProductId}</@th>
                    <@th>${uiLabelMap.ProductProduct}</@th>
                    <@th>${uiLabelMap.ProductFromLocation}</@th>
                    <@th>${uiLabelMap.ProductQoh}</@th>
                    <@th>${uiLabelMap.ProductAtp}</@th>
                    <@th>${uiLabelMap.ProductToLocation}</@th>
                    <@th>${uiLabelMap.ProductQoh}</@th>
                    <@th>${uiLabelMap.ProductAtp}</@th>
                    <@th>${uiLabelMap.ProductMinimumStock}</@th>
                    <@th>${uiLabelMap.ProductMoveQuantity}</@th>
                    <@th>${uiLabelMap.CommonConfirm}</@th>
                    <@th align="right">
                        ${uiLabelMap.ProductSelectAll}&nbsp;
                        <input type="checkbox" name="selectAll" value="Y" onclick="javascript:toggleAll(this, 'selectAllForm');highlightAllRows(this, 'moveInfoId_tableRow_', 'selectAllForm');" />
                    </@th>
                </@tr>
                </@thead>
                    <#list moveByOisgirInfoList! as moveByOisgirInfo>
                        <#assign product = moveByOisgirInfo.product>
                        <#assign facilityLocationFrom = moveByOisgirInfo.facilityLocationFrom>
                        <#assign facilityLocationTypeEnumFrom = (facilityLocationFrom.getRelatedOne("TypeEnumeration", true))!>
                        <#assign facilityLocationTo = moveByOisgirInfo.facilityLocationTo>
                        <#assign targetProductFacilityLocation = moveByOisgirInfo.targetProductFacilityLocation>
                        <#assign facilityLocationTypeEnumTo = (facilityLocationTo.getRelatedOne("TypeEnumeration", true))!>
                        <#assign totalQuantity = moveByOisgirInfo.totalQuantity>
                        <@tr id="moveInfoId_tableRow_${rowCount}" valign="middle">
                            <@td>${product.productId}</@td>
                            <@td>${product.internalName!}</@td>
                            <@td>${facilityLocationFrom.areaId!}:${facilityLocationFrom.aisleId!}:${facilityLocationFrom.sectionId!}:${facilityLocationFrom.levelId!}:${facilityLocationFrom.positionId!}<#if facilityLocationTypeEnumFrom?has_content>(${facilityLocationTypeEnumFrom.description})</#if>[${facilityLocationFrom.locationSeqId}]</@td>
                            <@td>${moveByOisgirInfo.quantityOnHandTotalFrom!}</@td>
                            <@td>${moveByOisgirInfo.availableToPromiseTotalFrom!}</@td>
                            <@td>${facilityLocationTo.areaId!}:${facilityLocationTo.aisleId!}:${facilityLocationTo.sectionId!}:${facilityLocationTo.levelId!}:${facilityLocationTo.positionId!}<#if facilityLocationTypeEnumTo?has_content>(${facilityLocationTypeEnumTo.description})</#if>[${facilityLocationTo.locationSeqId}]</@td>
                            <@td>${moveByOisgirInfo.quantityOnHandTotalTo!}</@td>
                            <@td>${moveByOisgirInfo.availableToPromiseTotalTo!}</@td>
                            <@td>${targetProductFacilityLocation.minimumStock!}</@td>
                            <@td>${targetProductFacilityLocation.moveQuantity!}</@td>
                            <@td align="right">
                                <input type="hidden" name="productId_o_${rowCount}" value="${product.productId!}" />
                                <input type="hidden" name="facilityId_o_${rowCount}" value="${facilityId!}" />
                                <input type="hidden" name="locationSeqId_o_${rowCount}" value="${facilityLocationFrom.locationSeqId!}" />
                                <input type="hidden" name="targetLocationSeqId_o_${rowCount}" value="${facilityLocationTo.locationSeqId!}" />
                                <input type="text" name="quantityMoved_o_${rowCount}" size="6" value="${totalQuantity?string.number}" />
                            </@td>
                            <@td align="right">
                                <input type="checkbox" name="_rowSubmit_o_${rowCount}" value="Y" onclick="javascript:checkToggle(this, 'selectAllForm');highlightRow(this,'moveInfoId_tableRow_${rowCount}');" />
                            </@td>
                        </@tr>
                        <#assign rowCount = rowCount + 1>
                    </#list>
                    <#list moveByPflInfoList! as moveByPflInfo>
                        <#assign product = moveByPflInfo.product>
                        <#assign facilityLocationFrom = moveByPflInfo.facilityLocationFrom>
                        <#assign facilityLocationTypeEnumFrom = (facilityLocationFrom.getRelatedOne("TypeEnumeration", true))!>
                        <#assign facilityLocationTo = moveByPflInfo.facilityLocationTo>
                        <#assign targetProductFacilityLocation = moveByPflInfo.targetProductFacilityLocation>
                        <#assign facilityLocationTypeEnumTo = (facilityLocationTo.getRelatedOne("TypeEnumeration", true))!>
                        <#assign totalQuantity = moveByPflInfo.totalQuantity>
                        <@tr id="moveInfoId_tableRow_${rowCount}" valign="middle">
                            <@td>${product.productId}</@td>
                            <@td>${product.internalName!}</@td>
                            <@td>${facilityLocationFrom.areaId!}:${facilityLocationFrom.aisleId!}:${facilityLocationFrom.sectionId!}:${facilityLocationFrom.levelId!}:${facilityLocationFrom.positionId!}<#if facilityLocationTypeEnumFrom?has_content>(${facilityLocationTypeEnumFrom.description})</#if>[${facilityLocationFrom.locationSeqId}]</@td>
                            <@td>${moveByPflInfo.quantityOnHandTotalFrom!}</@td>
                            <@td>${moveByPflInfo.availableToPromiseTotalFrom!}</@td>
                            <@td>${facilityLocationTo.areaId!}:${facilityLocationTo.aisleId!}:${facilityLocationTo.sectionId!}:${facilityLocationTo.levelId!}:${facilityLocationTo.positionId!}<#if facilityLocationTypeEnumTo?has_content>(${facilityLocationTypeEnumTo.description})</#if>[${facilityLocationTo.locationSeqId}]</@td>
                            <@td>${moveByPflInfo.quantityOnHandTotalTo!}</@td>
                            <@td>${moveByPflInfo.availableToPromiseTotalTo!}</@td>
                            <@td>${targetProductFacilityLocation.minimumStock!}</@td>
                            <@td>${targetProductFacilityLocation.moveQuantity!}</@td>
                            <@td align="right">
                                <input type="hidden" name="productId_o_${rowCount}" value="${product.productId!}" />
                                <input type="hidden" name="facilityId_o_${rowCount}" value="${facilityId!}" />
                                <input type="hidden" name="locationSeqId_o_${rowCount}" value="${facilityLocationFrom.locationSeqId!}" />
                                <input type="hidden" name="targetLocationSeqId_o_${rowCount}" value="${facilityLocationTo.locationSeqId!}" />
                                <input type="text" name="quantityMoved_o_${rowCount}" size="6" value="${totalQuantity?string.number}" />
                            </@td>
                            <@td align="right">
                                <input type="checkbox" name="_rowSubmit_o_${rowCount}" value="Y" onclick="javascript:checkToggle(this, 'selectAllForm');highlightRow(this,'moveInfoId_tableRow_${rowCount}');" />
                            </@td>
                        </@tr>
                        <#assign rowCount = rowCount + 1>
                    </#list>
                    <@tfoot>
                    <@tr>
                        <@td colspan="13" align="right">
                            <a href="javascript:document.selectAllForm.submit();" class="${styles.link_run_sys!} ${styles.action_update!}">${uiLabelMap.ProductConfirmSelectedMoves}</a>
                        </@td>
                    </@tr>
                    </@tfoot>
            </@table>
            
            <input type="hidden" name="_rowCount" value="${rowCount}" />
        </form>
      <#else>
        <@commonMsg type="result-norecord">${uiLabelMap.ProductNoStockMovesNeeded}.</@commonMsg>
      </#if>
      
      <#assign messageCount = 0>
      <#list pflWarningMessageList! as pflWarningMessage>
        <#assign messageCount = messageCount + 1>
        <@alert type="warning">${messageCount}:${pflWarningMessage}.</@alert>
      </#list>
</@section>      
      
<@section title=uiLabelMap.ProductQuickStockMove>
        <form method="post" action="<@ofbizUrl>processQuickStockMove</@ofbizUrl>" name="quickStockMove">
            <input type="hidden" name="facilityId" value="${facilityId!}" />
                <@field type="lookup" label=uiLabelMap.ProductProduct formName="quickStockMove" name="productId" id="productId" fieldFormName="LookupProduct"/>
                <@field type="lookup" label=uiLabelMap.ProductFromLocation formName="quickStockMove" name="locationSeqId" id="locationSeqId" fieldFormName="LookupFacilityLocation?facilityId=${facilityId}&amp;locationTypeEnumId=FLT_PICKLOC"/>
                 <@field type="lookup" label=uiLabelMap.ProductToLocation formName="quickStockMove" name="targetLocationSeqId" id="targetLocationSeqId" fieldFormName="LookupFacilityLocation?facilityId=${facilityId}&amp;locationTypeEnumId=FLT_PICKLOC"/>
                <@field type="input" label=uiLabelMap.ProductMoveQuantity name="quantityMoved" size="6" />
                 <@field type="submit" submitType="link" href="javascript:document.quickStockMove.submit();" class="+${styles.link_run_sys!} ${styles.action_update!}" text=uiLabelMap.ProductQuickStockMove />
        </form>
</@section>
