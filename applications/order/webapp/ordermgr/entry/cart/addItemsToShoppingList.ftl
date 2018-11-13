<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
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
