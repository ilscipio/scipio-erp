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
<#include "component://setup/webapp/setup/common/common.ftl">

<#-- SCIPIO: DEPRECATED in favor of catalog tree
 -->
<#if productStoreCatalogList?has_content>

<@table type="data-list">
  <@thead>
    <@th width="30%">${uiLabelMap.ProductCatalog}</@th>
    <@th width="20%">${uiLabelMap.CommonFrom}</@th>
    <@th width="20%">${uiLabelMap.CommonThru}</@th>
    <@th width="10%">${uiLabelMap.ProductSequenceNum}</@th>
    <@th>${uiLabelMap.CommonActions}</@th>
  </@thead>
  <@tbody>
    <#list productStoreCatalogList as pscat>
      <@tr>
        <#assign cat = pscat.getRelatedOne("ProdCatalog", false)>
        <@td><@setupExtAppLink uri="/catalog/control/EditProdCatalog?prodCatalogId=${raw(cat.prodCatalogId)}"><#t/>
                <#if cat.catalogName?has_content>${cat.catalogName} [${cat.prodCatalogId}]<#else>${cat.prodCatalogId}</#if><#t/>
             </@setupExtAppLink>
        </@td>
        <@td>${pscat.fromDate!}</@td>
        <@td>${pscat.thruDate!}</@td>
        <@td>${pscat.sequenceNum!}<#if pscat?is_first> (${uiLabelMap.CommonDefault})</#if></@td>
        <@td><#-- class="+${styles.text_right!}" -->
            <a href="javascript:jQuery('#setupCatalog-editCatalog-${escapeVal(pscat.prodCatalogId, 'js-html')}').submit();void(0);"<#rt/>
              <#lt/> class="${styles.link_nav} ${styles.action_update}">${uiLabelMap.CommonEdit}</a>
            <a href="javascript:jQuery('#setupCatalog-deleteCatalog-${escapeVal(pscat.prodCatalogId, 'js-html')}').submit();void(0);"<#rt/>
              <#lt/> class="${styles.link_nav} ${styles.action_remove}">${uiLabelMap.CommonDelete}</a>
        </@td>
      </@tr>
    </#list>
  </@tbody>
</@table>

<#list productStoreCatalogList as pscat>
  <@form method="get" action=makePageUrl("setupCatalog") id=("setupCatalog-editCatalog-"+raw(pscat.prodCatalogId))>
    <@defaultWizardFormFields exclude=["prodCatalogId"]/>
    <@field type="hidden" name="setupContinue" value="N"/>
    <@field type="hidden" name="prodCatalogId" value=pscat.prodCatalogId/>
  </@form>
  
  <@form method="get" action=makePageUrl("setupDeleteCatalog") id=("setupCatalog-deleteCatalog-"+raw(pscat.prodCatalogId))>
    <@defaultWizardFormFields exclude=["prodCatalogId", "productStoreId"]/>
    <@field type="hidden" name="setupContinue" value="N"/>
    <@field type="hidden" name="isDeleteCatalog" value="Y"/>
    
    <@field type="hidden" name="prodCatalogId" value=pscat.prodCatalogId/>
    <@field type="hidden" name="productStoreId" value=pscat.productStoreId/>
    <@field type="hidden" name="fromDate" value=pscat.fromDate/>
  </@form>
</#list>

</#if>
