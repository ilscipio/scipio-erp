<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#if (shoppingCart.getOrderType() == "SALES_ORDER")>
    <#assign associatedProducts = Static["org.ofbiz.order.shoppingcart.product.ProductDisplayWorker"].getRandomCartProductAssoc(request, true)!>
</#if>

<#if associatedProducts?has_content>
  <@section title=uiLabelMap.OrderHelpAlsoInterestedIn>
      <@table type="data-complex" class="+${styles.table_spacing_tiny_hint!}" autoAltRows=false>
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
