<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<@script>
function makeExpDate() {<#-- SCIPIO: Copied from ordermgr billsettings.ftl -->
    document.editgiftcardform.expireDate.value = document.editgiftcardform.expMonth.options[document.editgiftcardform.expMonth.selectedIndex].value + "/" + document.editgiftcardform.expYear.options[document.editgiftcardform.expYear.selectedIndex].value;
}
</@script>
<#-- <#-- SCIPIO: Already in page title: 
<#if !giftCard??>
  <#assign sectionTitle = uiLabelMap.AccountingCreateNewGiftCard>
<#else>
  <#assign sectionTitle = uiLabelMap.AccountingEditGiftCard>
</#if>
<@section title=sectionTitle>-->
<@section>
    <#macro saveCancelMenu>
      <@menu type="button">
        <@menuitem type="link" href=makeOfbizUrl("${donePage}?partyId=${partyId}") text=uiLabelMap.CommonCancelDone class="+${styles.action_nav!} ${styles.action_cancel!}" />
        <@menuitem type="link" href="javascript:document.editgiftcardform.submit()" text=uiLabelMap.CommonSave class="+${styles.action_run_sys!} ${styles.action_update!}" />
      </@menu>
    </#macro>
        
    <#--<@saveCancelMenu />-->

    <#if !giftCard??>
      <form method="post" action="<@ofbizUrl>createGiftCard?DONE_PAGE=${donePage}</@ofbizUrl>" name="editgiftcardform">
    <#else>
      <form method="post" action="<@ofbizUrl>updateGiftCard?DONE_PAGE=${donePage}</@ofbizUrl>" name="editgiftcardform">
        <input type="hidden" name="paymentMethodId" value="${paymentMethodId}" />
    </#if>
        <input type="hidden" name="partyId" value="${partyId}"/>
        <input type="hidden" name="expireDate" value="${(giftCardData.expireDate)!}"/><#-- SCIPIO -->
        
        <@field type="input" label=uiLabelMap.AccountingCardNumber size="20" maxlength="60" name="cardNumber" value=(giftCardData.cardNumber!) />
        <@field type="input" label=uiLabelMap.AccountingPinNumber size="10" maxlength="60" name="pinNumber" value=(giftCardData.pinNumber!) />
        <@field type="generic" label=uiLabelMap.CommonExpireDate>
            <#assign expMonth = "">
            <#assign expYear = "">
            <#if (giftCardData.expireDate)??>
            <#assign expDate = toSet(["FORTYMILLION"])>
              <#assign expDate = rawString(giftCardData.expireDate)>
              <#if (expDate?? && (expDate?index_of("/") > 0))>
                <#assign expMonth = expDate?substring(0, expDate?index_of("/"))>
                <#assign expYear = expDate?substring(expDate?index_of("/")+1)>
              </#if>
            </#if>
            <@field type="select" inline=true name="expMonth" onChange="javascript:makeExpDate();" tooltip=uiLabelMap.CommonMonth>
              <#if expMonth?has_content>
                <#assign ccExprMonth = expMonth>
              <#else>
                <#assign ccExprMonth = requestParameters.expMonth!>
              </#if>
              <#if ccExprMonth?has_content>
                <option value="${ccExprMonth}">${ccExprMonth}</option>
              </#if>
              <@render resource="component://common/widget/CommonScreens.xml#ccmonths" />
            </@field>
            <@field type="select" inline=true name="expYear" onChange="javascript:makeExpDate();" tooltip=uiLabelMap.CommonYear>
              <#if expYear?has_content>
                <#assign ccExprYear = expYear>
              <#else>
                <#assign ccExprYear = requestParameters.expYear!>
              </#if>
              <#if ccExprYear?has_content>
                <option value="${ccExprYear}">${ccExprYear}</option>
              </#if>
              <@render resource="component://common/widget/CommonScreens.xml#ccyears" />
            </@field>
        </@field>
        <@field type="input" label=uiLabelMap.CommonDescription size="30" maxlength="60" name="description" value=(paymentMethodData.description!)/>
      </form>
      
      <@saveCancelMenu />
</@section>
