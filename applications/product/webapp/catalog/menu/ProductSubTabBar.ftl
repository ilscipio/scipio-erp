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
<@row>
    <@cell>
    <ul class="${styles.button_group!}">
        <li><a href="<@ofbizUrl>EditProduct</@ofbizUrl>" class="tiny button success">${uiLabelMap.ProductNewProduct}</a></li>
        <#if product?has_content>
            <li><a href="<@ofbizUrl>CreateVirtualWithVariantsForm</@ofbizUrl>?product_id=${productId!}" class="tiny button success">${uiLabelMap.ProductNewVirtualProduct}</a></li>
            <li><a href="/ecommerce/control/product?product_id=${productId!}" class="tiny button success">${uiLabelMap.ProductProductPage}</a></li>
            <li><a href="<@ofbizUrl>ProductBarCode.pdf</@ofbizUrl>?productId=${productId!}" class="tiny button" target="_blank">${uiLabelMap.ProductBarcode}</a></li>
        </#if>
        <li><a href="<@ofbizUrl>EditProductTag</@ofbizUrl>" class="tiny button">${uiLabelMap.ProductTags}</a></li>
        <#--<#if tabButtonItem?has_content && tabButtonItem="EditProduct">
            <li><a href="javascript:expandAll(true);" class="tiny button">${uiLabelMap.CommonExpandAll}</a></li>
            <li><a href="javascript:expandAll(false);" class="tiny button">${uiLabelMap.CommonCollapseAll}</a></li>
        </#if>-->
    </ul>
    </@cell>
</@row>