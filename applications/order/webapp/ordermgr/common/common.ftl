
<#-- SCIPIO: common order-wide helper definitions and macros -->

<#include "component://product/webapp/catalog/common/common.ftl">

<#-- 2018-11-29: Returns shopping cart IF exists (for ordermgr, may only exist for orderentry)
    Templates that are not sure if cart is in context or not MUST use this; do NOT access sessionAttributes.shoppingCart anymore! -->
<#function getShoppingCart>
    <#return shoppingCart!cart!Static["org.ofbiz.order.shoppingcart.ShoppingCartEvents"].getCartObjectIfExists(request!)!>
</#function>
