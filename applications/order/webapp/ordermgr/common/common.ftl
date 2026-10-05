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
<#-- SCIPIO: Common Order templates utilities and definitions include
    NOTE: For reuse from other applications, please import *lib.ftl instead. -->
<#import "component://order/webapp/ordermgr/common/orderlib.ftl" as orderlib>
<#-- FIXME?: For now, dump the macros into main namespace as well, for getShoppingCart and field stuff... -->
<#include "component://order/webapp/ordermgr/common/orderlib.ftl">

<#-- FIXME: getShoppingCart really belongs in orderlib, but namespace issues for now -->
<#-- 2018-11-29: Returns shopping cart IF exists (for ordermgr, may only exist for orderentry)
    Templates that are not sure if cart is in context or not MUST use this; do NOT access sessionAttributes.shoppingCart anymore! -->
<#function getShoppingCart>
    <#return shoppingCart!cart!Static["org.ofbiz.order.shoppingcart.ShoppingCartEvents"].getCartObjectIfExists(request!)!>
</#function>