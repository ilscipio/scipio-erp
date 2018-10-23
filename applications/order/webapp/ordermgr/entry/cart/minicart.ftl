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

<#assign shoppingCart = sessionAttributes.shoppingCart!>
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
          
          <@table type="data-complex"> <#-- orig: class="" -->
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
