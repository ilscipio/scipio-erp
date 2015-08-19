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
<div class="screenlet">
    <div class="screenlet-title-bar">
        <h3>${uiLabelMap.ProductInventorySummary}</h3>
    </div>
    <div class="screenlet-body">
        <table cellspacing="0" class="basic-table">
          <thead>
            <tr class="header-row">
                <th>${uiLabelMap.ProductProductId}</th>
                    <#list featureTypeIds as featureTypeId>
                        <#assign featureType = delegator.findOne("ProductFeatureType", Static["org.ofbiz.base.util.UtilMisc"].toMap("productFeatureTypeId", featureTypeId), false)>
                        <th>${featureType.description}&nbsp;</th>
                    </#list>
                <th>${uiLabelMap.ProductQoh}</th>
                <th>${uiLabelMap.ProductAtp}</th>
            </tr>
            </thead>
            <#assign rowClass = "2">
            <#list variantInventorySummaries as variantSummary>
            <tr valign="middle"<@dataRowClassStr alt=(rowClass == "1") />>
                <td><a href="<@ofbizUrl>EditProductInventoryItems?productId=${variantSummary.productId}</@ofbizUrl>" class="${styles.button_default!}">${variantSummary.productId}</a></td>
                    <#list featureTypeIds as featureTypeId>
                        <td>${(variantSummary[featureTypeId].description)?default(featureTypeId)}</td>
                    </#list>
                <td>${variantSummary.quantityOnHandTotal}</td>
                <td>${variantSummary.availableToPromiseTotal}</td>
            </tr>
            <#-- toggle the row color -->
            <#if rowClass == "2">
                <#assign rowClass = "1">
            <#else>
                <#assign rowClass = "2">
            </#if>
            </#list>
        </table>
    </div>
</div>
