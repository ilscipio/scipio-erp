<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

  <p>${uiLabelMap.SecurityExtThisEmailIsInResponseToYourRequestToHave} <#if useEncryption>${uiLabelMap.SecurityExtANew}<#else>${uiLabelMap.SecurityExtYour}</#if> ${uiLabelMap.SecurityExtPasswordSentToYou}.</p>
  <p>
      <#if useEncryption>
          ${uiLabelMap.SecurityExtNewPasswordMssgEncryptionOn}
      <#else>
          ${uiLabelMap.SecurityExtNewPasswordMssgEncryptionOff}
      </#if>
      "${password}"
    <p>