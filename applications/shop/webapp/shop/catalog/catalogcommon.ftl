
<#-- Cato: common shop catalog definitions and macros -->

<#include "../common/common.ftl">

<#-- migrated from productdetail.ftl -->
<#macro associatedProducts assocProducts beforeName showName afterName formNamePrefix targetRequestName>
      <#assign pageProduct = product />
      <#assign targetRequest = "product" />
      <#if targetRequestName?has_content>
        <#assign targetRequest = targetRequestName />
      </#if>
      <#if assocProducts?has_content>
        <#assign assocTitle>${beforeName!}<#if showName == "Y">${productContentWrapper.get("PRODUCT_NAME", "html")!}</#if>${afterName!}</#assign>
        <@section title=assocTitle>
            <@grid columns=5>
                <#list assocProducts as productAssoc>
                    <li>
                        <#if productAssoc.productId == product.productId>
                            <#assign assocProductId = productAssoc.productIdTo />
                        <#else>
                            <#assign assocProductId = productAssoc.productId />
                        </#if>
                        <#--
                          <a href="<@ofbizUrl>${targetRequest}/<#if categoryId??>~category_id=${categoryId}/</#if>~product_id=${assocProductId}</@ofbizUrl>" class="${styles.link_nav_info_id!}">
                            ${assocProductId}
                          </a>
                        <#if productAssoc.reason?has_content>
                          - <strong>${productAssoc.reason}</strong>
                        </#if>
                        </div>
                      -->
                      <#assign dummy = setRequestAttribute("optProductId", assocProductId)>
                      <#assign dummy = setRequestAttribute("listIndex", listIndex)>
                      <#assign dummy = setRequestAttribute("formNamePrefix", formNamePrefix)>
                      <#if targetRequestName?has_content>
                        <#assign dummy = setRequestAttribute("targetRequestName", targetRequestName)>
                      <#else>
                        <#assign dummy = setRequestAttribute("targetRequestName", "")>
                      </#if>
                      <@render resource="component://shop/widget/CatalogScreens.xml#miniproductsummary" />
                  </li>
                  <#assign product = pageProduct />
                  <#local listIndex = listIndex + 1 />
                </#list>
            </@grid>
            <#assign dummy = setRequestAttribute("optProductId", "")>
            <#assign dummy = setRequestAttribute("formNamePrefix", "")>
            <#assign dummy = setRequestAttribute("targetRequestName", "")>

        </@section>
      </#if>
</#macro>

<#-- migrated from productdetail.ftl -->
<#macro commonAssociatedProducts productValue commonFeatureResultIds=[]>

    <#-- special cross/up-sell area using commonFeatureResultIds (from common feature product search) -->

    <#if commonFeatureResultIds?has_content>            
        <@section title=uiLabelMap.ProductSimilarProducts>
            <#list commonFeatureResultIds as commonFeatureResultId>
                <#assign dummy = setRequestAttribute("optProductId", commonFeatureResultId)>
                <#assign dummy = setRequestAttribute("listIndex", commonFeatureResultId_index)>
                <#assign dummy = setRequestAttribute("formNamePrefix", "cfeatcssl")>                    
                <#-- <#assign dummy = setRequestAttribute("targetRequestName", targetRequestName)> -->
                <@render resource="component://shop/widget/CatalogScreens.xml#miniproductsummary" />
            </#list>
        </@section>
    </#if>

    <#-- Upgrades/Up-Sell/Cross-Sell -->
    <#assign listIndex = 1 />
    <#assign dummy = setRequestAttribute("productValue", productValue)>

    <#-- also bought -->
    <@associatedProducts assocProducts=alsoBoughtProducts beforeName="" showName="N" afterName="${uiLabelMap.ProductAlsoBought}" formNamePrefix="albt" targetRequestName="" />
    <#-- obsolete -->
    <@associatedProducts assocProducts=obsoleteProducts beforeName="" showName="Y" afterName=" ${uiLabelMap.ProductObsolete}" formNamePrefix="obs" targetRequestName="" />
    <#-- cross sell -->
    <@associatedProducts assocProducts=crossSellProducts beforeName="" showName="N" afterName="${uiLabelMap.ProductCrossSell}" formNamePrefix="cssl" targetRequestName="crosssell" />
    <#-- up sell -->
    <@associatedProducts assocProducts=upSellProducts beforeName="${uiLabelMap.ProductUpSell} " showName="Y" afterName=":" formNamePrefix="upsl" targetRequestName="upsell" />
    <#-- obsolescence -->
    <@associatedProducts assocProducts=obsolenscenseProducts beforeName="" showName="Y" afterName=" ${uiLabelMap.ProductObsolescense}" formNamePrefix="obce" targetRequestName="" />
</#macro>


