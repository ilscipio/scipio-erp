<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<@section title=uiLabelMap.EcommerceMyAccount>
  <form id="editUserForm" method="post" action="<@pageUrl>updateCustomerProfile</@pageUrl>">
    <fieldset class="left center">
      <input type="hidden" name="emailContactMechPurposeTypeId" value="PRIMARY_EMAIL" />
      <input type="hidden" name="emailContactMechId" value="${emailContactMechId!}" />
        <@heading>${uiLabelMap.PartyContactInformation}</@heading>
        <div>
          <label for="firstName">${uiLabelMap.PartyFirstName}*<span id="advice-required-firstName" style="display: none" class="errorMessage">(${uiLabelMap.CommonRequired})</span></label>
          <input type="text" name="firstName" id="firstName" class="required" value="${firstName!}" maxlength="30" />
        </div>
        <div>
          <label for="lastName">${uiLabelMap.PartyLastName}*<span id="advice-required-lastName" style="display: none" class="errorMessage">(${uiLabelMap.CommonRequired})</span></label>
          <input type="text" name="lastName" id="lastName" class="required" value="${lastName!}" maxlength="30" />
        </div>
        <div>
          <label for="emailAddress">${uiLabelMap.CommonEmail}*
            <span id="advice-required-emailAddress" style="display: none" class="errorMessage">(${uiLabelMap.CommonRequired})</span>
            <span id="advice-validate-email-emailAddress" class="errorMessage" style="display:none">${uiLabelMap.PartyEmailAddressNotFormattedCorrectly}</span>
          </label>
          <input type="text" class="required validate-email" name="emailAddress" id="emailAddress" value="${emailAddress!}" maxlength="255" />
        </div>
    </fieldset>

    <fieldset class="center right">
        <@heading>${uiLabelMap.EcommerceAccountInformation}</@heading>
        <div>
          <label for="userLoginId">${uiLabelMap.CommonUsername}*</label>
          <input type="text" name="userLoginId" id="userLoginId" value="${userLogin.userLoginId!}" maxlength="255" <#if userLogin.userLoginId??>disabled="disabled"</#if> />
        </div>
        <div>
          <label for="currentPassword">${uiLabelMap.CommonCurrentPassword}*</label>
          <input type="password" name="currentPassword" id="currentPassword" value="" maxlength="16" />
        </div>
        <div>
          <label for="newPassword">${uiLabelMap.CommonNewPassword}*</label>
          <input type="password" name="newPassword" id="newPassword" value="" maxlength="16" />
        </div>
        <div>
          <label for="newPasswordVerify">${uiLabelMap.CommonNewPasswordVerify}*</label>
          <input type="password" name="newPasswordVerify" id="newPasswordVerify" value="" maxlength="16" />
        </div>
    </fieldset>
    <div>
      <input type="submit" id="submitEditUserForm" class="${styles.link_run_sys!} ${styles.action_update!}" value="${uiLabelMap.CommonSubmit}"/>
      <a id="cancelEditUserForm" href="<@pageUrl>viewprofile</@pageUrl>" class="${styles.link_nav_cancel!}">${uiLabelMap.CommonCancel}</a>
    </div>
  </form>
</@section>