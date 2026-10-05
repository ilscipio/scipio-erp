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
<#--
Changes to this file: Copyright (C) Ilscipio GmbH. The changes are licensed
under the GNU Affero General Public License, version 3, or a commercial
license from Ilscipio GmbH (file LICENSE). The original code stays under
the Apache License, version 2.0, as stated above.
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