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
    <@menu type="subtab">
        <@menuitem type="link" ofbizHref="EditProduct" text="${uiLabelMap.ProductNewProduct}" contentClass="+success" />
        <#if product?has_content>
            <li><a href="<@ofbizUrl>CreateVirtualWithVariantsForm</@ofbizUrl>?product_id=${productId!}" class="${styles.menu_subtab_itemlink!} success">${uiLabelMap.ProductNewVirtualProduct}</a></li>
            <li><a href="/ecommerce/control/product?product_id=${productId!}" class="${styles.menu_subtab_itemlink!} success">${uiLabelMap.ProductProductPage}</a></li>
            <li><a href="<@ofbizUrl>ProductBarCode.pdf</@ofbizUrl>?productId=${productId!}" class="${styles.menu_subtab_itemlink!}" target="_blank">${uiLabelMap.ProductBarcode}</a></li>
        </#if>
        <@menuitem type="link" ofbizHref="EditProductTag" text="${uiLabelMap.ProductTags}" />
        <#--<#if tabButtonItem?has_content && tabButtonItem="EditProduct">
            <@menuitem type="link" href="javascript:expandAll(true);" text="${uiLabelMap.CommonExpandAll}" />
            <@menuitem type="link" href="javascript:expandAll(false);" text="${uiLabelMap.CommonCollapseAll}" />
        </#if>-->
    </@menu>
    </@cell>
</@row>