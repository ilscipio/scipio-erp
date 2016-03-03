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

<form name="RegisterPerson" onsubmit="javascript:submitFormDisableSubmits(this)" class="basic-form" id="RegisterPerson" action="<@ofbizInterWebappUrl>/myportal/control/createRegister</@ofbizInterWebappUrl>" method="post">
    <input type="hidden" value="${webSiteId}" name="webSiteId"/>
    <input type="hidden" name="reload"/>
    <div id="_G0_" class="fieldgroup">
      <div class="fieldgroup-body" id="_G0__body">
        <@field type="textarea" required=true label="Why Would You Like To Register" id="RegisterPerson_whyWouldYouLikeToRegister" rows="5" cols="60" name="whyWouldYouLikeToRegister"></@field>
        <@field type="input" label="Salutation" autocomplete="off" id="RegisterPerson_salutation" maxlength="60" size="40" name="salutation" value=(requestParameters.salutation!)/>
        <@field type="input" label="First name" required=true autocomplete="off" id="RegisterPerson_firstName" maxlength="60" size="40" name="firstName" value=(requestParameters.firstName!)/>
        <@field type="input" label="Middle Name" autocomplete="off" id="RegisterPerson_middleName" maxlength="60" size="40" name="middleName" value=(requestParameters.middleName!)/>
        <@field type="input" required=true label="Last name" autocomplete="off" id="RegisterPerson_lastName" maxlength="60" size="40" name="lastName" value=(requestParameters.lastName!)/>
        <@field type="input" required=true label="Email" autocomplete="off" id="RegisterPerson_USER_EMAIL" maxlength="250" size="60" name="USER_EMAIL" value=(requestParameters.USER_EMAIL!)/>
    
        <@field type="display" label="User Login"></@field>
        <@field type="input" required=true label="Username" autocomplete="off" id="RegisterPerson_USERNAME" maxlength="250" size="30" name="USERNAME" value=(requestParameters.USERNAME!)/>
        <@field type="password" required=true label="Password" id="RegisterPerson_PASSWORD" maxlength="250" size="15" name="PASSWORD"  value=(requestParameters.PASSWORD!)/>
        <@field type="password" required=true label="Password (Confirm)" id="RegisterPerson_CONFIRM_PASSWORD" maxlength="250" size="15" name="CONFIRM_PASSWORD" value=(requestParameters.CONFIRM_PASSWORD!)/>
                    
        <@field type="generic" label="Verify captcha code">  
            <@field type="display" label="Code Captcha">
                <img id="captchaImage" src="<@ofbizUrl>captcha.jpg?captchaCodeId=captchaImage&amp;unique=${nowTimestamp.getTime()}</@ofbizUrl>" alt="" />
                <a href="javascript:reloadCaptcha('captchaImage');" class="${styles.link_run_local!} ${styles.action_reload!}">${uiLabelMap.CommonReloadCaptchaCode}</a>
            </@field>
            <@script>
                function reloadCaptcha(fieldName) {
                    var captchaUri = "<@ofbizUrl>captcha.jpg?captchaCodeId=" + fieldName + "&amp;unique=_PLACEHOLDER_</@ofbizUrl>";
                    var unique = Date.now();
                    captchaUri = captchaUri.replace("_PLACEHOLDER_", unique);
                    document.getElementById(fieldName).src = captchaUri;
                }
            </@script>
            <@field type="text" required=true label="Verify captcha code" autocomplete="off" id="RegisterPerson_captcha" maxlength="30" size="23" name="captcha"/>
        </@field>

        <@field type="submit" text="Save" class="+${styles.link_run_sys!} ${styles.action_register!}"/>
      </div>
    </div>
</form>
