<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#include "../common/common.ftl">
<@row>
    <@cell>
    <@menu type="subtab">
        <#if parameters._CURRENT_VIEW_=="EditProduct">        
            <#if product?has_content>
              <#if (isCreateProduct!false) != true>
                <@menuitem type="link" href=makeOfbizUrl("EditProduct") text=uiLabelMap.ProductNewProduct class="+${styles.action_nav!} ${styles.action_add!}" />  
              </#if>                      
                <@menuitem type="link" href=makeOfbizUrl("CreateVirtualWithVariantsForm?productId=${productId!}") text=uiLabelMap.ProductNewVirtualProduct class="+${styles.action_nav!} ${styles.action_add!}" />
                <@menuitem type="link" href=makeOfbizUrl("ViewProduct?productId=${productId!}") text=uiLabelMap.ProductProductOverview class="+${styles.action_nav!} ${styles.action_view!}" />                        
            </#if>
        <#else>
          <#if (isCreateProduct!false) != true>
            <@menuitem type="link" href=makeOfbizUrl("EditProduct") text=uiLabelMap.ProductNewProduct class="+${styles.action_nav!} ${styles.action_add!}" />
          </#if>
            <#if product?has_content>
                <@menuitem type="link" href=makeOfbizUrl("CreateVirtualWithVariantsForm?productId=${productId!}") text=uiLabelMap.ProductNewVirtualProduct class="+${styles.action_nav!} ${styles.action_add!}" />
                <@menuitem type="link" href=makeOfbizUrl("EditProduct?productId=${productId!}") text=uiLabelMap.ProductEditProduct class="+${styles.action_nav!} ${styles.action_update!}" />                       
            </#if>                        
        </#if>        
        <#if product?has_content>
          <@productShopPageUrlMenuItem productId=productId!/>
          <@menuitem type="link" href=makeOfbizUrl("ProductBarCode.pdf?productId=${productId!}") text=uiLabelMap.ProductBarcode target="_blank" class="+${styles.action_nav!} ${styles.action_export!}" />
        </#if>
        <#--
        <@menuitem type="link" href=makeOfbizUrl("EditProductTag") text=uiLabelMap.ProductTags class="+${styles.action_nav!} ${styles.action_update!}" />
        <#if activeSubMenuItem?has_content && activeSubMenuItem="EditProduct">
            <@menuitem type="link" href="javascript:expandAll(true);" text=uiLabelMap.CommonExpandAll class="+${styles.action_run_local!} ${styles.action_show!}" />
            <@menuitem type="link" href="javascript:expandAll(false);" text=uiLabelMap.CommonCollapseAll class="+${styles.action_run_local!} ${styles.action_hide!}" />
        </#if>-->
    </@menu>
    </@cell>
</@row>