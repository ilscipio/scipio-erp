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

<#if !creditCard?has_content>
    <#assign creditCard = requestParameters>
</#if>

<#if !paymentMethod?has_content>
    <#assign paymentMethod = requestParameters>
</#if>

<@fieldset>
    <@field type="input" size="30" maxlength="60" name="companyNameOnCard" value=(creditCard.companyNameOnCard!) label=uiLabelMap.AccountingCompanyNameCard/>     
    <@field type="select" name="titleOnCard" label=uiLabelMap.AccountingPrefixCard>
        <option value="">${uiLabelMap.CommonSelectOne}</option>
        <option<#if ((creditCard.titleOnCard)?default("") == "${uiLabelMap.CommonTitleMr}")> selected="selected"</#if>>${uiLabelMap.CommonTitleMr}</option>
        <option<#if ((creditCard.titleOnCard)?default("") == "Mrs.")> selected="selected"</#if>>${uiLabelMap.CommonTitleMrs}</option>
        <option<#if ((creditCard.titleOnCard)?default("") == "Ms.")> selected="selected"</#if>>${uiLabelMap.CommonTitleMs}</option>
        <option<#if ((creditCard.titleOnCard)?default("") == "Dr.")> selected="selected"</#if>>${uiLabelMap.CommonTitleDr}</option>
    </@field>    
    <@field type="input" size="20" maxlength="60" name="firstNameOnCard" value=((creditCard.firstNameOnCard)!) label=uiLabelMap.AccountingFirstNameCard required=true/>     
    <@field type="input" size="15" maxlength="60" name="middleNameOnCard" value=((creditCard.middleNameOnCard)!) label=uiLabelMap.AccountingMiddleNameCard />    
    <@field type="input" size="20" maxlength="60" name="lastNameOnCard" value=((creditCard.lastNameOnCard)!) label=uiLabelMap.AccountingLastNameCard required=true />  
    <@field type="select" name="suffixOnCard" label=uiLabelMap.AccountingSuffixCard>
        <option value="">${uiLabelMap.CommonSelectOne}</option>
        <option<#if ((creditCard.suffixOnCard)?default("") == "Jr.")> selected="selected"</#if>>Jr.</option>
        <option<#if ((creditCard.suffixOnCard)?default("") == "Sr.")> selected="selected"</#if>>Sr.</option>
        <option<#if ((creditCard.suffixOnCard)?default("") == "I")> selected="selected"</#if>>I</option>
        <option<#if ((creditCard.suffixOnCard)?default("") == "II")> selected="selected"</#if>>II</option>
        <option<#if ((creditCard.suffixOnCard)?default("") == "III")> selected="selected"</#if>>III</option>
        <option<#if ((creditCard.suffixOnCard)?default("") == "IV")> selected="selected"</#if>>IV</option>
        <option<#if ((creditCard.suffixOnCard)?default("") == "V")> selected="selected"</#if>>V</option>
    </@field>
    <@field type="select" name="cardType" label=uiLabelMap.AccountingCardType required=true>
        <#if creditCard.cardType??>
          <option>${creditCard.cardType}</option>
          <option value="${creditCard.cardType}">---</option>
        </#if>
        ${screens.render("component://common/widget/CommonScreens.xml#cctypes")}
    </@field>
   
    <#if creditCard?has_content>
        <#if cardNumberMinDisplay?has_content>
            <#-- create a display version of the card where all but the last four digits are * -->
            <#assign cardNumberDisplay = "">
            <#assign cardNumber = creditCard.cardNumber!>
            <#if cardNumber?has_content>
                <#assign size = cardNumber?length - 4>
                <#if (size > 0)>
                    <#list 0 .. size-1 as foo>
                        <#assign cardNumberDisplay = cardNumberDisplay + "*">
                    </#list>
                    <#assign cardNumberDisplay = cardNumberDisplay + cardNumber[size .. size + 3]>
                <#else>
                    <#-- but if the card number has less than four digits (ie, it was entered incorrectly), display it in full -->
                    <#assign cardNumberDisplay = cardNumber>
                </#if>
            </#if>
            <@field type="input" size="20" maxlength="30" name="cardNumber" value=(cardNumberDisplay!) label=uiLabelMap.AccountingCardNumber required=true />
        <#else>
            <@field type="input" size="20" maxlength="30" name="cardNumber" value=(creditCard.cardNumber!) label=uiLabelMap.AccountingCardNumber required=true/>
        </#if>
    <#else>
        <@field type="input" size="20" maxlength="30" name="cardNumber" value=(creditCard.cardNumber!) label=uiLabelMap.AccountingCardNumber required=true/>
    </#if>
    
  <#--<@tr>
    <@td width="26%" align="right" valign="middle">${uiLabelMap.AccountingCardSecurityCode}</@td>
    <@td width="74%">
        <input type="input" size="5" maxlength="10" name="cardSecurityCode" value="${creditCard.cardSecurityCode!}" />
    </@td>
  </@tr>-->
  
    <#assign expMonth = "">
    <#assign expYear = "">
    <#if creditCard?? && creditCard.expireDate??>
        <#assign expDate = creditCard.expireDate>
        <#if (expDate?? && expDate.indexOf("/") > 0)>
            <#assign expMonth = expDate.substring(0,expDate.indexOf("/"))>
            <#assign expYear = expDate.substring(expDate.indexOf("/")+1)>
        </#if>
    </#if>
      
    <@field type="select" name="expMonth" label=uiLabelMap.AccountingExpirationDate required=true>
        <#if creditCard?has_content && expMonth?has_content>
        <#assign ccExprMonth = expMonth>
        <#else>
        <#assign ccExprMonth = requestParameters.expMonth!>
        </#if>
        <#if ccExprMonth?has_content>
        <option value="${ccExprMonth!}">${ccExprMonth!}</option>
        </#if>
        ${screens.render("component://common/widget/CommonScreens.xml#ccmonths")}
    </@field>
    
    <@field type="select" name="expYear">
        <#if creditCard?has_content && expYear?has_content>
          <#assign ccExprYear = expYear>
        <#else>
          <#assign ccExprYear = requestParameters.expYear!>
        </#if>
        <#if ccExprYear?has_content>
          <option value="${ccExprYear!}">${ccExprYear!}</option>
        </#if>
        ${screens.render("component://common/widget/CommonScreens.xml#ccyears")}
    </@field>

    <@field type="input" size="20" maxlength="30" name="description" value=(paymentMethod.description!) label=uiLabelMap.CommonDescription/>
</@fieldset>