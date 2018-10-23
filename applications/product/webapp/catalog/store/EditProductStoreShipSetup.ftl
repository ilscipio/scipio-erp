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
<@section title=uiLabelMap.ProductCarrierShipmentMethod>
    <form name="addscarr" method="post" action="<@ofbizUrl>prepareCreateShipMeth</@ofbizUrl>">
      <@fields type="default-nolabelarea">
        <input type="hidden" name="newShipMethod" value="Y"/>
        <input type="hidden" name="productStoreId" value="${productStoreId!}"/>
        <@field type="select" name="carrierShipmentString">
              <option>${uiLabelMap.ProductSelectOne}</option>
              <#list carrierShipmentMethods as shipmentMethod>
                <option value="${shipmentMethod.partyId}|${shipmentMethod.roleTypeId}|${shipmentMethod.shipmentMethodTypeId}">${shipmentMethod.shipmentMethodTypeId} (${shipmentMethod.partyId}/${shipmentMethod.roleTypeId})</option>
              </#list>
        </@field>
        <@field type="submit" class="+${styles.link_run_sys!} ${styles.action_add!}" text=uiLabelMap.CommonAdd/>
      </@fields>
    </form>
</@section>
