<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
-->
<#--
    Aurora sign-in: a split screen. The graphite half carries the logo, the claim and the
    icons of the applications; the white half carries the form. The messages of the request
    render over the form (see .page-noauth in aurora-layout.css).
-->
<#if requestAttributes.uiLabelMap??><#assign uiLabelMap = requestAttributes.uiLabelMap></#if>
<#assign useMultitenant = getPropertyValue("general", "multitenant")!"">
<#assign username = requestParameters.USERNAME!(userLogin.userLoginId)!(autoUserLogin.userLoginId)!"">
<#assign brandName = (layoutSettings.companyName!"Scipio ERP")?keep_before(" - ")>
<#if !brandName?has_content || brandName == "SCIPIO"><#assign brandName = "Scipio ERP"></#if>
<#assign demoHint = "">
<#if uiLabelMap.WebtoolsForSomethingInteresting?has_content && uiLabelMap.WebtoolsForSomethingInteresting != "WebtoolsForSomethingInteresting">
    <#assign demoHint = uiLabelMap.WebtoolsForSomethingInteresting>
</#if>
<#assign heroApps = ["order", "catalog", "manufacturing", "facility", "accounting", "party", "cms", "CRM", "humanres", "workeffort", "shop", "admin"]>

<div class="au-login" id="login">
    <section class="au-login-hero" aria-label="${brandName}">
        <div class="au-login-brand">
            <img src="<@ofbizContentUrl>/aurora/images/scipio-logo-small.svg</@ofbizContentUrl>" alt="" width="30" height="35"/>
            <span>${brandName}</span>
        </div>
        <div class="au-login-claim">
            <h1>${uiLabelMap.CommonLoginClaim}</h1>
            <p>${uiLabelMap.CommonLoginClaimText}</p>
            <ul class="au-login-icons" aria-hidden="true">
                <#list heroApps as appKey><li><i class="${styles.icon!} ${(styles.app_icon[appKey])!'fa-circle-o'}"></i></li></#list>
            </ul>
        </div>
        <div class="au-login-foot"><span>${brandName}</span><span>&copy; ilscipio GmbH</span></div>
    </section>

    <section class="au-login-main">
        <h2>${uiLabelMap.CommonWelcomeBack}</h2>
        <p class="au-login-lead">${uiLabelMap.CommonSignInToContinue} <strong>${applicationTitle!brandName}</strong>.</p>

        <form method="post" action="<@pageUrl>login</@pageUrl>" name="loginform" class="au-login-form">
            <div class="au-login-field">
                <label for="au-login-user">${uiLabelMap.CommonUsername}</label>
                <div class="au-login-input">
                    <i class="fa fa-user" aria-hidden="true"></i>
                    <input type="text" id="au-login-user" name="USERNAME" value="${username}" autocomplete="username"
                        autocapitalize="none" spellcheck="false" required<#if !username?has_content> autofocus</#if>/>
                </div>
            </div>
            <div class="au-login-field">
                <div class="au-login-row">
                    <label class="au-login-label" for="au-login-password">${uiLabelMap.CommonPassword}</label>
                    <a href="<@pageUrl>forgotPassword</@pageUrl>">${uiLabelMap.CommonForgotYourPassword}</a>
                </div>
                <div class="au-login-input">
                    <i class="fa fa-lock" aria-hidden="true"></i>
                    <input type="password" id="au-login-password" name="PASSWORD" value="" autocomplete="current-password" required<#if username?has_content> autofocus</#if>/>
                    <button type="button" class="au-login-reveal" data-au-reveal="au-login-password" aria-pressed="false" aria-label="${uiLabelMap.CommonShowPassword}"><i class="fa fa-eye" aria-hidden="true"></i></button>
                </div>
            </div>
            <#if "Y" == useMultitenant>
                <div class="au-login-field">
                    <label for="au-login-tenant">${uiLabelMap.CommonTenantId}</label>
                    <div class="au-login-input">
                        <i class="fa fa-building" aria-hidden="true"></i>
                        <input type="text" id="au-login-tenant" name="userTenantId" value="${parameters.userTenantId!}" autocomplete="organization"/>
                    </div>
                </div>
            </#if>
            <button type="submit" class="button is-primary au-login-submit">${uiLabelMap.CommonSignIn} <i class="fa fa-arrow-right" aria-hidden="true"></i></button>
        </form>

        <#if demoHint?has_content>
            <p class="au-login-note"><i class="fa fa-info-circle" aria-hidden="true"></i><span>${demoHint}</span></p>
        </#if>
    </section>
</div>
