<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#if password?has_content>
  <p>${uiLabelMap.SecurityExtThisEmailIsInResponseToYourRequestToHave} <#if useEncryption>${uiLabelMap.SecurityExtANew}<#else>${uiLabelMap.SecurityExtYour}</#if> ${uiLabelMap.SecurityExtPasswordSentToYou}.</p>
  <p>
      <#if useEncryption>
          ${uiLabelMap.SecurityExtNewPasswordMssgEncryptionOn}
      <#else>
          ${uiLabelMap.SecurityExtNewPasswordMssgEncryptionOff}
      </#if>
      "${password}"
    <p>
<#elseif verifyHash?has_content>
    <p>${uiLabelMap.SecurityExtThisEmailIsInResponseToYourRequestToResetPwd}</p>
    <p>
        <a href="${makePageUrl("changePassword?h=" + verifyHash)}" target="_blank" class="" style="display: block; padding: 13px 20px; text-decoration:none; color:#000001;">
            <span class="" style="text-decoration:none; color:#000001;"><strong>${uiLabelMap.SecurityExtResetYourPassword}</strong></span>
        </a>
    <p>
</#if>