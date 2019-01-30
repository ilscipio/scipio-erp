<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#if allProductPromos?has_content>
    <@section title=uiLabelMap.OrderManualPromotions>
        <form method="post" action="<@pageUrl>doManualPromotions</@pageUrl>" name="domanualpromotions">
          <!-- to enter more than two manual promotions, just add a new select box with name="productPromoId_n" -->
          <select name="productPromoId_1">
            <option value=""></option>
            <#list allProductPromos as productPromo>
              <option value="${productPromo.productPromoId}">${productPromo.promoName!}</option>
            </#list>
          </select>
          <select name="productPromoId_2">
            <option value=""></option>
            <#list allProductPromos as productPromo>
              <option value="${productPromo.productPromoId}">${productPromo.promoName!}</option>
            </#list>
          </select>
          <input type="submit" class="${styles.link_run_sys!} ${styles.action_update!}" value="${uiLabelMap.OrderDoPromotions}" />
        </form>
    </@section>
</#if>
