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
<@section>
    <#-- A store without catalogs renders nothing; lay out only the stores that render a tree,
         so the grid has no empty cell. -->
    <#assign storeTrees = []>
    <#list productStores as productStore>
        <#assign storeTree><@render resource="component://product/widget/catalog/CommonScreens.xml#ScipioViewCatalogTree"  ctxVars={"productStoreId":productStore.productStoreId!} asString=true /></#assign>
        <#if storeTree?contains("<section")><#assign storeTrees = storeTrees + [storeTree]></#if>
    </#list>
    <#list storeTrees as storeTree>
        <#if storeTree_index %2 == 0><@row open=true close=false/></#if>
        <@cell columns=6>${storeTree}</@cell>
        <#if storeTree_index %2 == 1 || !storeTree_has_next><@row open=false close=true/></#if>
    </#list>
</@section>
