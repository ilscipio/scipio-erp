<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#include "component://shop/webapp/shop/customer/customercommon.ftl">

<@script>
    function reloadCaptcha(fieldName) {
        var captchaUri = "<@pageUrl>captcha.jpg?captchaCodeId=" + fieldName + "&amp;unique=_PLACEHOLDER_</@pageUrl>";
        var unique = Date.now();
        captchaUri = captchaUri.replace("_PLACEHOLDER_", unique);
        document.getElementById(fieldName).src = captchaUri;
    }
</@script>
<#assign acsParams = acsParams!requestParameters!>

<@section title=uiLabelMap.CommonContactUs>
    <form id="contactForm" method="post" action="<@pageUrl>submitAnonContact</@pageUrl>">
        <input type="hidden" name="partyIdFrom" value="${(userLogin.partyId)!}" />
        <input type="hidden" name="partyIdTo" value="${productStore.payToPartyId!}"/>
        <input type="hidden" name="contactMechTypeId" value="WEB_ADDRESS" />
        <input type="hidden" name="communicationEventTypeId" value="WEB_SITE_COMMUNICATI" />
        <input type="hidden" name="productStoreId" value="${productStore.productStoreId}" />
        <input type="hidden" name="emailType" value="CONT_NOTI_EMAIL" />

        <@field type="input" name="subject" id="subject" label=uiLabelMap.EcommerceSubject required=true value=(acsParams.subject!)/>
        <@field type="textarea" name="content" id="message" label=uiLabelMap.CommonMessage required=true cols="50" rows="5" value=(acsParams.content!)/>
        <@field type="input" name="emailAddress" id="emailAddress" label=uiLabelMap.FormFieldTitle_emailAddress required=true
            value=(acsParams.emailAddress!partyEmailAddress!)/>

        <#-- SCIPIO: 2019: We really don't need to show these when already logged in; however, maybe still
            show the email address to help confirm to the user where the reply will be going.
            NOTE: sendContactUsEmailToCompany is patched to look them up when omitted. -->
        <#if !(userLogin.partyId)?has_content>
          <@field type="input" name="firstName" id="firstName" label=uiLabelMap.PartyFirstName required=true value=(acsParams.firstName!(person.firstName)!)/>
          <@field type="input" name="lastName" id="lastName" label=uiLabelMap.PartyLastName required=true value=(acsParams.lastName!(person.lastName)!)/>
        </#if>
      
        <@field type="generic" label=uiLabelMap.CommonCaptchaCode>
            <div><img id="captchaImage" src="<@pageUrl>captcha.jpg?captchaCodeId=captchaImage&amp;unique=${escapeVal(nowTimestamp.getTime()!,' html')}</@pageUrl>" alt=""/></div>
            <a href="javascript:reloadCaptcha('captchaImage');">${uiLabelMap.CommonReloadCaptchaCode}</a>
        </@field>
        <@field type="input" name="captcha" label=uiLabelMap.CommonVerifyCaptchaCode autocomplete="off" maxlength="30" size="23" required=true/>
        <@field type="submit" value=uiLabelMap.CommonSubmit class="${styles.link_run_sys!} ${styles.action_send!}"/>
    </form>
</@section>
