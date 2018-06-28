<#--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
-->
<#--<#include rawString("component://widget/templates/htmlScreenMacroLibrary.ftl")>--> 

<#assign contentMsgContentClass = "content-message-content"><#-- SCIPIO: especially used by JS, cannot put in global styles unless do same in JS -->

  <#-- SCIPIO: FIXME: THESE ASSIGNS ARE DUPLICATED IN commonHeadScripts.ftl; KEEP IN SYNC 
    NOTE: SEE commonHeadScripts.ftl FOR JS-OUTPUTTED ERROR MESSAGES -->
  <#if requestAttributes.errorMessageList?has_content><#assign errorMessageList=requestAttributes.errorMessageList></#if>
  <#if requestAttributes.eventMessageList?has_content><#assign eventMessageList=requestAttributes.eventMessageList></#if>
  <#if requestAttributes.serviceValidationException??><#assign serviceValidationException = requestAttributes.serviceValidationException></#if>
  <#if requestAttributes.uiLabelMap?has_content><#assign uiLabelMap = requestAttributes.uiLabelMap></#if>
  
  <#if !errorMessage?has_content>
    <#assign errorMessage = requestAttributes._ERROR_MESSAGE_!>
  </#if>
  <#if !errorMessageList?has_content>
    <#assign errorMessageList = requestAttributes._ERROR_MESSAGE_LIST_!>
  </#if>
  <#if !eventMessage?has_content>
    <#assign eventMessage = requestAttributes._EVENT_MESSAGE_!>
  </#if>
  <#if !eventMessageList?has_content>
    <#assign eventMessageList = requestAttributes._EVENT_MESSAGE_LIST_!>
  </#if>

  <#-- SCIPIO: DEV NOTE: Please note that the rawString calls here do not prevent escaping;
      FTL escaping is and must still be applied by the #escape statement earlier;
      do not add #noescape statements here for stock variables -->
  
<#assign hasErrorMsg = (errorMessage?has_content || errorMessageList?has_content)>
<#assign hasEventMsg = (eventMessage?has_content || eventMessageList?has_content)>
<#assign msgPresAttrStr> has-scipio-errormsg="${hasErrorMsg?string}" has-scipio-eventmsg="${hasEventMsg?string}"</#assign>
  
<div id="content-messages"${msgPresAttrStr}><#-- SCIPIO: need container always, to locate this via js -->
  <#-- display the error messages -->
  <div id="main-${styles.alert_wrap!}"${msgPresAttrStr}>
  <#if (isErrorPage!false) == false> <#-- Do not display the error messages when on error page -->
      <#if (errorMessage?has_content || errorMessageList?has_content)>
        <@alert type="alert">
          <p>${uiLabelMap.CommonFollowingErrorsOccurred}:</p>
          <#if errorMessage?has_content>
            ${escapeEventMsg(errorMessage, 'htmlmarkup')}
          </#if>
          <#if errorMessageList?has_content>
            <ol>
              <#list errorMessageList as errorMsg>
                <li>${escapeEventMsg(errorMsg, 'htmlmarkup')}</li>
              </#list>
            </ol>
          </#if>
        </@alert>
      </#if>
  </#if>
      <#-- display the event messages -->
      <#if (eventMessage?has_content || eventMessageList?has_content)>
        <@alert type="info">
          <p>${uiLabelMap.CommonFollowingOccurred}:</p>
          <#if eventMessage?has_content>
            ${escapeEventMsg(eventMessage, 'htmlmarkup')}
          </#if>
          <#if eventMessageList?has_content>
            <ol>
              <#list eventMessageList as eventMsg>
                <li>${escapeEventMsg(eventMsg, 'htmlmarkup')}</li>
              </#list>
            </ol>
          </#if>
        </@alert>
      </#if>
    
      <#-- SCIPIO: only if it is gotten from context and needs to include explicit content only 
          WARNING TO DEVELOPERS: infoMessage (scipio-specific) is NOT escaped by this ftl - 
              The screen data preparation code must take care of it! For example, using:
                context.infoMessage = org.ofbiz.base.util.UtilCodec.getHtmlEncoder().encode(msg);
              DO NOT include unsanitized values in infoMessage context variable!
          FIXME: Instead of rawString and leaving escaping to screens, 
              this should try to exploit the #escapeVal (#escapeMsg) opts.allow parameter to allow a subset of HTML only;
              unfortunately still misses parsing library to achieve this at this time -->
      <#if infoMessage?has_content>
        <@alert type="info">
          ${rawString(infoMessage)}
        </@alert>
      </#if>
  </div>
</div>


<#-- SCIPIO: this alert TEMPLATES for JS. JS can copy their inner markup, insert a message and add elsewhere. -->
<div id="content-messages-templates" style="display:none;">
  <div id="content-messages-error-template" content-messages-type-wrapper-id="main-${styles.alert_wrap!}">
    <@alert type="error" class=("+"+contentMsgContentClass)></@alert>
  </div>
  <div id="content-messages-warning-template" content-messages-type-wrapper-id="main-${styles.alert_wrap!}">
    <@alert type="warning" class=("+"+contentMsgContentClass)></@alert>
  </div>
  <div id="content-messages-info-template" content-messages-type-wrapper-id="main-${styles.alert_wrap!}">
    <@alert type="info" class=("+"+contentMsgContentClass)></@alert>
  </div>
</div>

