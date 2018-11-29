<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#include "../ordercommon.ftl">

<#-- SCIPIO: Must use context or accessor
<#assign shoppingCart = sessionAttributes.shoppingCart!>-->
<#assign shoppingCart = getShoppingCart()!>
<#if shoppingCart?has_content>
    <#assign shoppingCartSize = shoppingCart.size()>
<#else>
    <#assign shoppingCartSize = 0>
</#if>
<@section id="minicart" title=uiLabelMap.OrderCartSummary>
        <#if (shoppingCartSize > 0)>
          <#macro cartLinks>
            <@menu type="button">
              <@menuitem type="link" href=makeOfbizUrl("view/showcart") text=uiLabelMap.OrderViewCart class="+${styles.action_nav!} ${styles.action_view!}" />
              <@menuitem type="link" href=makeOfbizUrl("checkoutoptions") text=uiLabelMap.OrderCheckout class="+${styles.action_nav!} ${styles.action_begin!}"/>
              <@menuitem type="link" href=makeOfbizUrl("quickcheckout") text=uiLabelMap.OrderCheckoutQuick class="+${styles.action_nav!} ${styles.action_begin!}"/>
              <@menuitem type="link" href=makeOfbizUrl("onePageCheckout") text=uiLabelMap.EcommerceOnePageCheckout class="+${styles.action_nav!} ${styles.action_begin!}"/>
              <@menuitem type="link" href=makeOfbizUrl("googleCheckout") text=uiLabelMap.EcommerceCartToGoogleCheckout class="+${styles.action_nav!} ${styles.action_begin!}"/>
            </@menu>
          </#macro>
        
          <#if hidetoplinks?default("N") != "Y">
            <@cartLinks />
          </#if>
          
          <@table type="data-complex">
            <@thead>
              <@tr>
                <@th>${uiLabelMap.OrderQty}</@th>
                <@th>${uiLabelMap.OrderItem}</@th>
                <@th>${uiLabelMap.CommonSubtotal}</@th>
              </@tr>
            </@thead>
            <@tfoot>
              <@tr>
                <@td colspan="3">
                  ${uiLabelMap.OrderTotal}: <@ofbizCurrency amount=shoppingCart.getDisplayGrandTotal() isoCode=shoppingCart.getCurrency()/>
                </@td>
              </@tr>
            </@tfoot>
            <@tbody>
            <#list shoppingCart.items() as cartLine>
              <@tr>
                <@td>${cartLine.getQuantity()?string.number}</@td>
                <@td>
                  <#if cartLine.getProductId()??>
                      <#if cartLine.getParentProductId()??>
                          <a href="<@ofbizCatalogAltUrl productId=cartLine.getParentProductId()/>" class="${styles.link_nav_info_name!}">${cartLine.getName()}</a>
                      <#else>
                          <a href="<@ofbizCatalogAltUrl productId=cartLine.getProductId()/>" class="${styles.link_nav_info_name!}">${cartLine.getName()}</a>
                      </#if>
                  <#else>
                    <strong>${cartLine.getItemTypeDescription()!}</strong>
                  </#if>
                </@td>
                <@td><@ofbizCurrency amount=cartLine.getDisplayItemSubTotal() isoCode=shoppingCart.getCurrency()/></@td>
              </@tr>
              <#if cartLine.getReservStart()??>
                <@tr><@td>&nbsp;</@td><@td colspan="2">(${cartLine.getReservStart()?string("yyyy-MM-dd")}, ${cartLine.getReservLength()} <#if cartLine.getReservLength() == 1>${uiLabelMap.CommonDay}<#else>${uiLabelMap.CommonDays}</#if>)</@td></@tr>
              </#if>
            </#list>
            </@tbody>
          </@table>
          
          <#if hidebottomlinks?default("N") != "Y">
            <@cartLinks />
          </#if>
        <#else>
          <@commonMsg type="result-norecord">${uiLabelMap.OrderShoppingCartEmpty}</@commonMsg>
        </#if>
</@section>
