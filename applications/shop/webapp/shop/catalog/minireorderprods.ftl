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
<#include "component://shop/webapp/shop/catalog/catalogcommon.ftl">

<#if reorderProducts?has_content>
<@section title="${rawLabel('ProductQuickReorder')}..." id="minireorderprods">
        <#list reorderProducts as miniProduct>
          <div>
              <@render resource="component://shop/widget/CatalogScreens.xml#miniproductsummary" reqAttribs={"miniProdQuantity":reorderQuantities.get(miniProduct.productId), "miniProdFormName":"theminireorderprod" + miniProduct_index + "form", "optProductId":miniProduct.productId}/>
          </div>
          <#if miniProduct_has_next>
              
          </#if>
        </#list>
</@section>
</#if>
