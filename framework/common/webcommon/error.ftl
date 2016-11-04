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

  <#-- SCIPIO: FIXME: All the rawString calls here are dangerous and not right, 
       they should be fixed in the upstream code instead! -->

<@row>
  <@cell class="${styles.grid_large!}10 ${styles.grid_large!}centered" last=true id="error">
  <@section>
    <@heading level=1><i class="${styles.icon!} ${styles.icon_error!}" style="font-size: 4rem;"></i> ${getLabel('PageTitleError')!}</@heading>

        ${getLabel('CommonFollowingErrorsOccurred')}
        <#if errorMessage?has_content>
                        ${rawString(errorMessage)}
        </#if>
        <#if errorMessageList?has_content>
                        <ol>
            <#list errorMessageList as errorMsg>
                          <li>${rawString(errorMsg)}</li>
            </#list>
                        </ol>
        </#if>

  </@section>
  </@cell>
</@row>