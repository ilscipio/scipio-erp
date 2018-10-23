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

<#-- Screenlet to add cart to shopping list. The shopping lists are presented in a dropdown box. -->

<#if (shoppingLists??) && (shoppingCartSize > 0)>
    <@section title=uiLabelMap.OrderAddOrderToShoppingList>
        <form method="post" name="addBulkToShoppingList" action="<@ofbizUrl>addBulkToShoppingList</@ofbizUrl>">
          <#assign index = 0/>
          <#list shoppingCart.items() as cartLine>
            <#if (cartLine.getProductId()??) && !cartLine.getIsPromo()>
              <input type="hidden" name="selectedItem" value="${index}"/>
            </#if>
            <#assign index = index + 1/>
          </#list>
          
          <@field type="select" label=uiLabelMap.PageTitleShoppingList name="shoppingListId" currentValue="">
          <#list shoppingLists as shoppingList>
            <option value="${shoppingList.shoppingListId}">${shoppingList.getString("listName")}</option>
          </#list>
            <option value="">---</option>
            <option value="">${uiLabelMap.OrderNewShoppingList}</option>
          </@field>
          
          <@field type="submit" class="+${styles.link_run_sys!} ${styles.action_add!}" text=uiLabelMap.OrderAddToShoppingList/>
        </form>
    </@section>
</#if>
