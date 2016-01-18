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
        <@menuitem type="link" href=makeOfbizUrl("EditProduct") text="${uiLabelMap.ProductNewProduct}" contentClass="+${styles.action_nav!} ${styles.action_add!}" />
        <#if product?has_content>
          <@menuitem type="link" href=makeOfbizUrl("CreateVirtualWithVariantsForm?product_id=${productId!}") text="${uiLabelMap.ProductNewVirtualProduct}" contentClass="+${styles.action_nav!} ${styles.action_add!}" />
          <@menuitem type="link" href="/ecommerce/control/product?product_id=${productId!}" text="${uiLabelMap.ProductProductPage}" contentClass="+${styles.action_nav!} ${styles.action_add!}" />
          <@menuitem type="link" href=makeOfbizUrl("ProductBarCode.pdf?productId=${productId!}") text="${uiLabelMap.ProductBarcode}" target="_blank" />
        </#if>
        <@menuitem type="link" href=makeOfbizUrl("EditProductTag") text="${uiLabelMap.ProductTags}" />
        <#--<#if tabButtonItem?has_content && tabButtonItem="EditProduct">
            <@menuitem type="link" href="javascript:expandAll(true);" text="${uiLabelMap.CommonExpandAll}" />
            <@menuitem type="link" href="javascript:expandAll(false);" text="${uiLabelMap.CommonCollapseAll}" />
        </#if>-->
    </@menu>
    </@cell>
</@row>