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

<#if (shoppingCart.getOrderType() == "SALES_ORDER")>
    <#assign associatedProducts = Static["org.ofbiz.order.shoppingcart.product.ProductDisplayWorker"].getRandomCartProductAssoc(request, true)!>
</#if>

<#if associatedProducts?has_content>
  <@section title=uiLabelMap.OrderHelpAlsoInterestedIn>
      <@table type="data-complex" class="+${styles.table_spacing_tiny_hint!}" autoAltRows=false> <#-- orig: cellspacing="0" --> <#-- orig: cellpadding="1" --> <#-- orig: border="0" -->
        <#-- random complementary products -->
        <#list associatedProducts as assocProduct>
          <@tr>
            <@td>
              <@render resource=productsummaryScreen reqAttribs={"optProduct":assocProduct, "listIndex":assocProduct_index}/>
            </@td>
          </@tr>
          <#if assocProduct_has_next>
            <@tr type="util"><@td><hr/></@td></@tr>
          </#if>
        </#list>
      </@table>
  </@section>
</#if>
