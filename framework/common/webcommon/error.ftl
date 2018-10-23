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