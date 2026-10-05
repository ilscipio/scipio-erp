<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
-->
<#include "component://product/webapp/catalog/common/common.ftl">
<@row>
    <@cell>
    <@menu type="subtab">
        <#if parameters._CURRENT_VIEW_=="EditProduct">        
            <#if product?has_content>
              <#if (isCreateProduct!false) != true>
                <@menuitem type="link" href=makePageUrl("EditProduct") text=uiLabelMap.ProductNewProduct class="+${styles.action_nav!} ${styles.action_add!}" />  
              </#if>                      
                <@menuitem type="link" href=makePageUrl("CreateVirtualWithVariantsForm?productId=${productId!}") text=uiLabelMap.ProductNewVirtualProduct class="+${styles.action_nav!} ${styles.action_add!}" />
                <@menuitem type="link" href=makePageUrl("ViewProduct?productId=${productId!}") text=uiLabelMap.ProductProductOverview class="+${styles.action_nav!} ${styles.action_view!}" />                        
            </#if>
        <#else>
          <#if (isCreateProduct!false) != true>
            <@menuitem type="link" href=makePageUrl("EditProduct") text=uiLabelMap.ProductNewProduct class="+${styles.action_nav!} ${styles.action_add!}" />
          </#if>
            <#if product?has_content>
                <@menuitem type="link" href=makePageUrl("CreateVirtualWithVariantsForm?productId=${productId!}") text=uiLabelMap.ProductNewVirtualProduct class="+${styles.action_nav!} ${styles.action_add!}" />
                <@menuitem type="link" href=makePageUrl("EditProduct?productId=${productId!}") text=uiLabelMap.ProductEditProduct class="+${styles.action_nav!} ${styles.action_update!}" />                       
            </#if>                        
        </#if>        
        <#if product?has_content>
          <@cataloglib.productShopPageUrlMenuItem productId=productId!/>
          <@menuitem type="link" href=makePageUrl("ProductBarCode.pdf?productId=${productId!}") text=uiLabelMap.ProductBarcode target="_blank" class="+${styles.action_nav!} ${styles.action_export!}" />
        </#if>
        <#--
        <@menuitem type="link" href=makePageUrl("EditProductTag") text=uiLabelMap.ProductTags class="+${styles.action_nav!} ${styles.action_update!}" />
        <#if activeSubMenuItem?has_content && activeSubMenuItem="EditProduct">
            <@menuitem type="link" href="javascript:expandAll(true);" text=uiLabelMap.CommonExpandAll class="+${styles.action_run_local!} ${styles.action_show!}" />
            <@menuitem type="link" href="javascript:expandAll(false);" text=uiLabelMap.CommonCollapseAll class="+${styles.action_run_local!} ${styles.action_hide!}" />
        </#if>-->
    </@menu>
    </@cell>
</@row>