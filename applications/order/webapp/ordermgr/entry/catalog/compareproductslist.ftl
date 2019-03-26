<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<@section title=uiLabelMap.ProductCompareProducts>
  <#assign productCompareList = Static["org.ofbiz.product.product.ProductEvents"].getProductCompareList(request)/>
  <#if productCompareList?has_content>
    <@table type="data-list" autoAltRows=false>
    <#list productCompareList as product>
      <@tr>
        <@td>
          ${Static["org.ofbiz.product.product.ProductContentWrapper"].getProductContentAsText(product, "PRODUCT_NAME", request, "raw")}
        </@td>
        <@td>
          <form method="post" action="<@pageUrl>removeFromCompare</@pageUrl>" name="removeFromCompare${product_index}form">
            <input type="hidden" name="productId" value="${product.productId}"/>
          </form>
          <a href="javascript:document.removeFromCompare${product_index}form.submit()" class="${styles.link_run_session!} ${styles.action_remove!}">${uiLabelMap.CommonRemove}</a>
        </@td>
      </@tr>
    </#list>
  </@table>
  <@menu type="button">
    <@menuitem type="link" href=makePageUrl("clearCompareList") text=uiLabelMap.CommonClearAll class="+${styles.action_run_session!} ${styles.action_clear!}" />
    <@menuitem type="link" href="javascript:popUp('${escapeVal(makePageUrl('compareProducts'), 'js')}', 'compareProducts', '650', '750')" text=uiLabelMap.ProductCompareProducts class="+${styles.action_nav!} ${styles.action_view!}" />
  </@menu>
<#else>
  <@commonMsg type="result-norecord">${uiLabelMap.ProductNoProductsToCompare}</@commonMsg>
</#if>
</@section>
