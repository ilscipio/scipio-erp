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
<@section title="${uiLabelMap.ProductInventorySummary}">
        <@table type="data-list" autoAltRows=true cellspacing="0"> <#-- orig: class="basic-table" -->
          <@thead>
            <@tr class="header-row">
                <@th>${uiLabelMap.ProductProductId}</@th>
                    <#list featureTypeIds as featureTypeId>
                        <#assign featureType = delegator.findOne("ProductFeatureType", Static["org.ofbiz.base.util.UtilMisc"].toMap("productFeatureTypeId", featureTypeId), false)>
                        <@th>${featureType.description}&nbsp;</@th>
                    </#list>
                <@th>${uiLabelMap.ProductQoh}</@th>
                <@th>${uiLabelMap.ProductAtp}</@th>
            </@tr>
          </@thead>
          <@tbody>
            <#list variantInventorySummaries as variantSummary>
              <@tr valign="middle">
                <@td><a href="<@ofbizUrl>EditProductInventoryItems?productId=${variantSummary.productId}</@ofbizUrl>" class="${styles.button_default!}">${variantSummary.productId}</a></@td>
                    <#list featureTypeIds as featureTypeId>
                        <@td>${(variantSummary[featureTypeId].description)?default(featureTypeId)}</@td>
                    </#list>
                <@td>${variantSummary.quantityOnHandTotal}</@td>
                <@td>${variantSummary.availableToPromiseTotal}</@td>
              </@tr>
            </#list>
          </@tbody>
        </@table>
</@section>
