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
<#-- SCIPIO: 4.0.0: Aurora Shop - opens the content area; footer.ftl closes it.
The page title is the h1; the CSS hides it on the home page and on a page that renders its own h1.
On the pages of the customer account a row of links leads to the other account pages. -->
<#assign asPageTitle = "">
<#if title?has_content><#assign asPageTitle = title><#elseif titleProperty?has_content><#assign asPageTitle = uiLabelMap[titleProperty]!""></#if>
<#assign asView = raw(requestAttributes._CURRENT_VIEW_!parameters._CURRENT_VIEW_!"")>
<#assign asAcctGroup = {
    "viewprofile": "profile", "editperson": "profile", "editcontactmech": "profile", "editcreditcard": "profile",
    "editeftaccount": "profile", "editgiftcard": "profile", "changepassword": "profile", "manageAddress": "profile", "editProfile": "profile",
    "orderhistory": "orders", "orderstatus": "orders", "orderdownloads": "orders", "requestReturn": "orders",
    "editShoppingList": "lists", "showShoppingList": "lists",
    "messagelist": "messages", "messagedetail": "messages", "newmessage": "messages"
}[asView]!"">
<#assign asSignedIn = (userLogin?? && (userLogin.userLoginId!"anonymous") != "anonymous")>
<main class="as-main scp-content<#if asAcctGroup?has_content && asSignedIn> as-account-page</#if>" id="as-content" tabindex="-1">
<#if asPageTitle?has_content><h1 class="as-page-title">${asPageTitle}</h1></#if>
<#if asAcctGroup?has_content && asSignedIn>
  <nav class="as-acct-nav" aria-label="${uiLabelMap.CommonProfile!"Account"}">
    <#list [["profile", "viewprofile", uiLabelMap.CommonProfile!"Profile"], ["orders", "orderhistory", uiLabelMap.EcommerceOrderHistory!"Orders"],
            ["lists", "editShoppingList", uiLabelMap.EcommerceShoppingLists!"Shopping lists"], ["messages", "messagelist", uiLabelMap.CommonMessages!"Messages"]] as item>
      <a href="<@ofbizUrl>${item[1]}</@ofbizUrl>"<#if item[0] == asAcctGroup> aria-current="page"</#if>>${item[2]}</a>
    </#list>
    <a class="as-acct-nav-out" href="<@ofbizUrl>logout</@ofbizUrl>">${uiLabelMap.CommonLogout!"Sign out"}</a>
  </nav>
</#if>
