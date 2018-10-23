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
<#include "component://shop/webapp/shop/customer/customercommon.ftl">

<#-- SCIPIO: This was a message to explain to "Go Back" kludge; however I have now recoded controller and screen
    to redirect automatically.
<@commonMsg type="info-important">${uiLabelMap.ShopSaveGoBackExplanation}</@commonMsg>-->


<#if person??>
  <#-- SCIPIO: duplicate: <#assign sectionTitle = uiLabelMap.PartyEditPersonalInformation/>-->
  <#assign sectionTitle = ""/>
<#else>
  <#assign sectionTitle = uiLabelMap.PartyAddNewPersonalInformation/>
</#if>
<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
    <@menuitem type="link" href=makeOfbizUrl(donePage) class="+${styles.action_nav!} ${styles.action_cancel!}" text=uiLabelMap.CommonGoBack />
    <@menuitem type="link" href="javascript:document.editpersonform.submit()" class="+${styles.action_run_sys!} ${styles.action_update!}" text=uiLabelMap.CommonSave />
  </@menu>
</#macro>
<@section title=sectionTitle menuContent=menuContent menuLayoutGeneral="bottom">
  <#-- SCIPIO: Bugfix? action used to be this: <#if person??>updatePerson<#else>createPerson/${donePage}</#if> 
    but the view override is inconsistent with the other edit*.ftl pages. -->
  <form id="editpersonform1" method="post" action="<@ofbizUrl><#if person??>updatePerson?DONE_PAGE=${donePage}&amp;targetPageResponse=redirect-done<#else>createPerson?DONE_PAGE=${donePage}&amp;targetPageResponse=redirect-done</#if></@ofbizUrl>" name="editpersonform">    

  <input type="hidden" name="partyId" value="${person.partyId!}" />
  
    <@personalTitleField label=uiLabelMap.CommonTitle name="personalTitle" personalTitle=((personData.personalTitle)!) />

    <@field type="input" label=uiLabelMap.PartyFirstName required=true size="30" maxlength="30" name="firstName" value=(personData.firstName!)/>
    <@field type="input" label=uiLabelMap.PartyMiddleInitial size="4" maxlength="4" name="middleName" value=(personData.middleName!)/>
    <@field type="input" label=uiLabelMap.PartyLastName required=true size="30" maxlength="30" name="lastName" value=(personData.lastName!)/>
    <@field type="input" label=uiLabelMap.PartySuffix size="10" maxlength="30" name="suffix" value=(personData.suffix!)/>
    <@field type="input" label=uiLabelMap.PartyNickName size="30" maxlength="60" name="nickname" value=(personData.nickname!)/>
    <@field type="select" label=uiLabelMap.PartyGender name="gender">
      <#if personData.gender?has_content >
        <option value="${personData.gender}">
            <#if personData.gender == "M" >${uiLabelMap.CommonMale}</#if>
            <#if personData.gender == "F" >${uiLabelMap.CommonFemale}</#if>
        </option>
        <option value="${personData.gender}"> -- </option>
      <#else>
        <option value="">${uiLabelMap.CommonSelectOne}</option>
      </#if>
      <option value="M">${uiLabelMap.CommonMale}</option>
      <option value="F">${uiLabelMap.CommonFemale}</option>
    </@field>
    <@field type="input" label=uiLabelMap.PartyBirthDate size="11" maxlength="20" name="birthDate" value=((personData.birthDate.toString())!) tooltip=uiLabelMap.CommonFormatDate/>
    <@field type="input" label=uiLabelMap.PartyHeight size="30" maxlength="60" name="height" value=(personData.height!)/>
    <@field type="input" label=uiLabelMap.PartyWeight size="30" maxlength="60" name="weight" value=(personData.weight!)/>

    <@field type="input" label=uiLabelMap.PartyMaidenName size="30" maxlength="60" name="mothersMaidenName" value=(personData.mothersMaidenName!)/>
    <@field type="select" label=uiLabelMap.PartyMaritalStatus name="maritalStatus">
    <#if personData.maritalStatus?has_content>
       <option value="${personData.maritalStatus}">
         <#if personData.maritalStatus == "S">${uiLabelMap.PartySingle}</#if>
         <#if personData.maritalStatus == "M">${uiLabelMap.PartyMarried}</#if>
         <#if personData.maritalStatus == "D">${uiLabelMap.PartyDivorced}</#if>
       </option>
      <option value="${personData.maritalStatus}"> -- </option>
    <#else>
      <option></option>
    </#if>
      <option value="S">${uiLabelMap.PartySingle}</option>
      <option value="M">${uiLabelMap.PartyMarried}</option>
      <option value="D">${uiLabelMap.PartyDivorced}</option>
    </@field>
    <@field type="input" label=uiLabelMap.PartySocialSecurityNumber size="30" maxlength="60" name="socialSecurityNumber" value=(personData.socialSecurityNumber!)/>

    <@field type="input" label=uiLabelMap.PartyPassportNumber size="30" maxlength="60" name="passportNumber" value=(personData.passportNumber!)/>
    <@field type="input" label=uiLabelMap.PartyPassportExpireDate size="11" maxlength="20" name="passportExpireDate" value=(personData.passportExpireDate!) tooltip=uiLabelMap.CommonFormatDate/>
    <@field type="input" label=uiLabelMap.PartyTotalYearsWorkExperience size="30" maxlength="60" name="totalYearsWorkExperience" value=(personData.totalYearsWorkExperience!)/>
    <@field type="input" label=uiLabelMap.CommonComment size="30" maxlength="60" name="comments" value=(personData.comments!)/>
  </form>
</@section>
