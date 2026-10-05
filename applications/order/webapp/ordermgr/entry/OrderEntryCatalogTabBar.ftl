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
<#--
Changes to this file: Copyright (C) Ilscipio GmbH. The changes are licensed
under the GNU Affero General Public License, version 3, or a commercial
license from Ilscipio GmbH (file LICENSE). The original code stays under
the Apache License, version 2.0, as stated above.
-->

<#include "component://order/webapp/ordermgr/common/common.ftl">
<#import "component://product/webapp/catalog/common/cataloglib.ftl" as cataloglib>

  <#if orderHeader?has_content>
    <@section title=uiLabelMap.PageTitleLookupBulkAddProduct/>
  <#else>
    <#macro menuContent menuArgs={}>
      <@menu args=menuArgs>
        <@menuitem type="link" href=makePageUrl("orderentry") text=uiLabelMap.OrderOrderItems class="+${styles.action_nav!}" />
       
        <#if (showProductLinks!false) && product?has_content>
          <#-- SCIPIO: Copied (moved) product edit link from editProduct.ftl -->
          <#if security.hasEntityPermission("CATALOG", "_UPDATE", request)><#-- SCIPIO: changed to _UPDATE from _CREATE -->
            <@menuitem type="link" href=makeServerUrl("/catalog/control/ViewProduct?productId=${raw(product.productId)}${raw(externalKeyParam!)}")
              target="catalog" text=uiLabelMap.ProductEditProduct class="+${styles.action_nav!} ${styles.action_update!}" />
          </#if>
          <@cataloglib.productShopPageUrlMenuItem productId=product.productId/><#-- SCIPIO: Live product link -->
        </#if>

        <#if (showCategoryLinks!false) && productCategory?has_content><#-- SCIPIO: category links -->
          <#if security.hasEntityPermission("CATALOG", "_UPDATE", request)><#-- SCIPIO: changed to _UPDATE from _CREATE -->
            <@menuitem type="link" href=makeServerUrl("/catalog/control/EditCategory?productCategoryId=${raw(productCategory.productCategoryId)}${raw(externalKeyParam!)}")
              target="catalog" text=uiLabelMap.ProductEditCategory class="+${styles.action_nav!} ${styles.action_update!}" />
          </#if>
          <@cataloglib.categoryShopPageUrlMenuItem categoryId=productCategory.productCategoryId/><#-- SCIPIO: Live product link -->
        </#if>
      </@menu>
    </#macro>
    <@section menuContent=menuContent 
        title=(rawLabel('CommonCreate')+" "+rawLabel((shoppingCart.getOrderType() == "PURCHASE_ORDER")?then('OrderPurchaseOrder', 'OrderSalesOrder')))/>
  </#if>