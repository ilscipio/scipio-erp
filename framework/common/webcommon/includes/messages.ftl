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
<#escape x as x?html>
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

  <#-- SCIPIO: FIXME: All the rawString calls here are dangerous and not right, 
       they should be fixed in the upstream code instead! -->
  
<#assign hasErrorMsg = (errorMessage?has_content || errorMessageList?has_content)>
<#assign hasEventMsg = (eventMessage?has_content || eventMessageList?has_content)>
<#assign msgPresAttrStr> has-scipio-errormsg="${hasErrorMsg?string}" has-scipio-eventmsg="${hasEventMsg?string}"</#assign>
  
<div id="content-messages"${msgPresAttrStr}><#-- SCIPIO: need container always, to locate this via js -->
  <#-- display the error messages -->
  <div id="main-${styles.alert_wrap!}"${msgPresAttrStr}>
  <#if (isErrorPage!false) == false> <#-- Do not display the error messages when on error page -->
      <#if (errorMessage?has_content || errorMessageList?has_content)>
                <@alert type="alert">
          <#noescape><p>${uiLabelMap.CommonFollowingErrorsOccurred}:</p></#noescape>
          <#if errorMessage?has_content>
                            ${rawString(errorMessage)}
          </#if>
          <#noescape>
            <#if errorMessageList?has_content>
                            <ol>
                <#list errorMessageList as errorMsg>
                              <li>${rawString(errorMsg)}</li>
                </#list>
                            </ol>
            </#if>
          </#noescape>
                </@alert>
      </#if>
  </#if>
      <#-- display the event messages -->
      <#if (eventMessage?has_content || eventMessageList?has_content)>
            <@alert type="info">
          <#noescape><p>${uiLabelMap.CommonFollowingOccurred}:</p></#noescape>
          <#if eventMessage?has_content>
                            ${rawString(eventMessage)}
          </#if>
          <#if eventMessageList?has_content>
                            <ol>
            <#list eventMessageList as eventMsg>
                              <li>${rawString(eventMsg)}</li>
            </#list>
                            </ol>
          </#if>
            </@alert>
      </#if>
    
        
      <#-- SCIPIO: only if it is gotten from context and needs to include explicit content only -->
      <#if infoMessage?has_content>
            <@alert type="info">
                <#noescape>${rawString(infoMessage)}</#noescape>
            </@alert>
      </#if>
  </div>
</div>
</#escape>


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

