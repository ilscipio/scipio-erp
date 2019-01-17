<#-- SCIPIO: Common Accounting utilities and definitions library
    May be imported by other applications' templates, preferably using:
      <#import "component://accounting/webapp/accounting/common/acctlib.ftl" as acctlib>
    NOTE: For this application's own templates, please include common.ftl instead (which includes this). -->

<#function getGiftCardDisplayNumber gc mask=-4>
  <#if isObjectType("map", gc)>
    <#local gc = gc.cardNumber!/>
  </#if>
  <#if gc?has_content>
    <#return maskValueLeft(gc, mask)>
  </#if>
</#function>

<#function getCreditCardDisplayNumber cc mask=-4>
  <#if isObjectType("map", cc)>
    <#local cc = cc.cardNumber!/>
  </#if>
  <#if cc?has_content>
    <#return maskValueLeft(cc, mask)>
  </#if>
</#function>

<#function getPayMethDisplayNumber cardNumber paymentMethod defaultMask=0><#-- less logical: defaultMask=4 -->
  <#switch (paymentMethod.paymentMethodTypeId)!"">
    <#case "GIFT_CARD">
        <#return getGiftCardDisplayNumber(cardNumber)>
    <#case "CREDIT_CARD">
        <#return getCreditCardDisplayNumber(cardNumber)>
    <#default>
        <#--<#return maskValueLeft(cardNumber, defaultMask)>-->
        <#-- TODO?: May be more types to handle -->
        <#return cardNumber>
  </#switch>
</#function>

<#-- SCIPIO: Moved here from component://party/webapp/partymgr/party/profileblocks/PaymentMethods.ftl and rewritten -->
<#macro maskSensitiveNumber cardNumber paymentMethod={} defaultMask=0><#-- less logical: defaultMask=4 -->
  <#-- SCIPIO: DEV NOTE: cardNumberDisplay must remain #assign, not #local, because template files check it -->
  <#assign cardNumberDisplay = getPayMethDisplayNumber(cardNumber, paymentMethod)>
  ${escapeVal(cardNumberDisplay, 'html')}<#t/>
</#macro>
