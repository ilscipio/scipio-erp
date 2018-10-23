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
   
<@section title=uiLabelMap.ProductReceiveReturn>
    <form name="receiveInventoryReturn" method="post" action="<@ofbizUrl>ReceiveInventory</@ofbizUrl>">
        <input type="hidden" name="facilityId" value="${requestParameters.facilityId!}" />
        <input type="hidden" name="initialSelected" value="Y" />
        <@field type="input" label=uiLabelMap.ProductReturnNumber name="returnId" size="20" maxlength="20" value=(requestParameters.returnId!) />
        <@field type="submit" submitType="link" href="javascript:document.receiveInventoryReturn.submit();" class="+${styles.link_run_sys!} ${styles.action_receive!}" text=uiLabelMap.ProductReceiveProduct />
    </form>
</@section>