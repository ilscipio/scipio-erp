<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<#assign errorMessage = requestAttributes._ERROR_MESSAGE_!>
<#if requestAttributes.errorMessageList?has_content><#assign errorMessageList=requestAttributes.errorMessageList></#if>

<#-- SCIPIO: NOTE: 2018-02-26: The error message variables below must now be HTML-escaped by this ftl file.
    They will no longer be hard-escaped by ControlServlet - the mechanism here is more thorough and does not interfere with javascript. -->

<@row>
  <@cell class="${styles.grid_large!}10 ${styles.grid_large!}centered" last=true id="error">
  <@section>
    <@heading level=1><i class="${styles.icon!} ${styles.icon_error!}" style="font-size: 4rem;"></i> ${getLabel('PageTitleError')!}</@heading>
      <#if errorMessage?has_content || errorMessageList?has_content>
        ${getLabel('CommonFollowingErrorsOccurred')}
        <ol>
          <#if errorMessage?has_content>
            <li>${escapeEventMsg(errorMessage, 'htmlmarkup')}</li>
          </#if>
          <#if errorMessageList?has_content>         
            <#list errorMessageList as errorMsg>
              <li>${escapeEventMsg(errorMsg, 'htmlmarkup')}</li>
            </#list>         
          </#if>
        </ol>
      <#else>
        ${getLabel('CommonErrorOccurredContactSupport')}
      </#if>
  </@section>
  </@cell>
</@row>