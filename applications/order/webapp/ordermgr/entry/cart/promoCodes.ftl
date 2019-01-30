<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#if shoppingCart.getOrderType() == "SALES_ORDER">
    <@section title=uiLabelMap.OrderPromotionCouponCodes_spaced>
        <form method="post" action="<@pageUrl>addpromocode<#if requestAttributes._CURRENT_VIEW_?has_content>/${requestAttributes._CURRENT_VIEW_}</#if></@pageUrl>" name="addpromocodeform">
          <@field type="input" size="15" name="productPromoCodeId" value="" />
          <@field type="submit" class="+${styles.link_run_sys!} ${styles.action_add!}" text=uiLabelMap.OrderAddCode />
          <#assign productPromoCodeIds = (shoppingCart.getProductPromoCodesEntered())!>
          <#if productPromoCodeIds?has_content>
            <@row>
                <@cell>
            ${uiLabelMap.OrderEnteredPromoCodes}:
            <#list productPromoCodeIds as productPromoCodeId>
                  ${productPromoCodeId}<br/>
            </#list>
                </@cell>
            </@row>
          </#if>
        </form>
    </@section>
</#if>
