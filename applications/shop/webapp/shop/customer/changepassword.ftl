<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#include "component://shop/webapp/shop/customer/customercommon.ftl">

<#-- SCIPIO: This was a message to explain to "Go Back" kludge; however I have now recoded controller and screen
    to redirect automatically.
<@commonMsg type="info-important">${uiLabelMap.ShopSaveGoBackExplanation}</@commonMsg>-->

<#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
        <@menuitem type="link" id="CommonGoBack1" href=makeOfbizUrl(donePage) class="+${styles.action_nav_cancel!}" text=uiLabelMap.CommonGoBack />
        <@menuitem type="link" id="CommonSave1" href="javascript:document.getElementById('changepasswordform').submit()" class="+${styles.action_run_sys!} ${styles.action_update!}" text=uiLabelMap.CommonSave />
    </@menu>
</#macro>
<@section menuContent=menuContent menuLayoutGeneral="bottom"><#--title=uiLabelMap.PartyChangePassword-->
    <#-- SCIPIO: NOTE: view-switch replaced with redirect -->
    <form id="changepasswordform" method="post" action="<@ofbizUrl>updatePassword?DONE_PAGE=${donePage}&amp;targetPageResponse=redirect-done</@ofbizUrl>">

      <@field type="password" name="currentPassword" required=true id="currentPassword" maxlength="20" label=uiLabelMap.PartyOldPassword/>
      <@field type="password" name="newPassword" required=true id="newPassword" maxlength="20" label=uiLabelMap.PartyNewPassword/>
      <@field type="password" name="newPasswordVerify" required=true id="newPasswordVerify" maxlength="20" label=uiLabelMap.PartyNewPasswordVerify/>
      <@field type="input" maxlength="100" name="passwordHint" id="passwordHint" value=(userLoginData.passwordHint!) label=uiLabelMap.PartyPasswordHint/>
      
    </form>
</@section>
