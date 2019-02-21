<#-- SCIPIO: Common Order templates utilities and definitions include
    NOTE: For reuse from other applications, please import *lib.ftl instead. -->
<#import "component://order/webapp/ordermgr/common/orderlib.ftl" as orderlib>
<#-- FIXME?: For now, dump the macros into main namespace as well, for getShoppingCart and field stuff... -->
<#include "component://order/webapp/ordermgr/common/orderlib.ftl">
<#-- 2019-02-21: basically needed for getting macros/functions required to build payment methods -->
<#include "component://shop/webapp/shop/order/ordercommon.ftl">

<#-- FIXME: getShoppingCart really belongs in orderlib, but namespace issues for now -->
<#-- 2018-11-29: Returns shopping cart IF exists (for ordermgr, may only exist for orderentry)
    Templates that are not sure if cart is in context or not MUST use this; do NOT access sessionAttributes.shoppingCart anymore! -->
<#function getShoppingCart>
    <#return shoppingCart!cart!Static["org.ofbiz.order.shoppingcart.ShoppingCartEvents"].getCartObjectIfExists(request!)!>
</#function>