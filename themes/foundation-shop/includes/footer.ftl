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
<#include "common.ftl">

</div><#-- /<div class="off-canvas-wrap" data-offcanvas id="body-content"> -->
</div><#-- /<div class="inner-wrap"> -->

<#if (showFooterOtherContent!true) == true && (useMinimalTheme!false) == false>
<@row class="other-content">
    <@cell columns=3>
      <i class="${styles.icon} ${styles.icon_prefix}laptop"></i>
      <p>Lorem ipsum dolor sit amet, consectetur adipisicing elit. Cum maiores alias ea sunt facilis impedit fuga dignissimos illo quaerat iure in nobis id quos, eaque nostrum! Unde, voluptates suscipit repudiandae!</p>
    </@cell>
    <@cell columns=3>
      <i class="${styles.icon} ${styles.icon_prefix}html5"></i>
      <p>Lorem ipsum dolor sit amet, consectetur adipisicing elit. Fugit impedit consequuntur at! Amet sed itaque nostrum, distinctio eveniet odio, id ipsam fuga quam minima cumque nobis veniam voluptates deserunt!</p>
    </@cell>
    <@cell columns=3>
      <@heading>SCIPIO Webstore</@heading>
      <ul class="other-links">
        <#-- language select --> 
          <li>
              <a href="<@pageUrl><#if userHasAccount>viewprofile<#else>ListLocales</#if></@pageUrl>">
                ${uiLabelMap.CommonChooseLanguage}
              </a>
          </li>
        <li><a href="<@pageUrl>showAllPromotions</@pageUrl>">${uiLabelMap.ProductPromotions}</a></li>
        <li><a href="<@pageUrl>license</@pageUrl>">License</a></li>  
        <li><a href="https://www.scipioerp.com/products/faq">FAQ's</a></li>
      </ul>
    </@cell>
    <@cell columns=3>      
    <@heading>Follow Us!</@heading>
      <ul class="other-links">
        <li><a href="#">GitHub</a></li>
        <li><a href="#">Facebook</a></li>
        <li><a href="#">Twitter</a></li>
        <li><a href="#">Instagram</a></li>
      </ul>
    </@cell>
</@row>
</#if>


    <#-- close the off-canvas menu -->
    <a class="exit-off-canvas"></a>

    <#-- not applicable? </section>--><#-- /<section role="main" class="scroll-container"> -->

</div><#-- /<div class="off-canvas-wrap" data-offcanvas id="body-content"> -->
</div><#-- /<div class="inner-wrap"> -->


<#-- FOOTER SECTION -->
<footer id="footer">
    <#-- SCIPIO: 4.0.0: legal links of the store (compliance component); shown on every page, also in the minimal theme -->
    <#if Static["org.ofbiz.base.component.ComponentConfig"].isComponentEnabled("compliance")>
      <#import "component://compliance/templates/shop/complianceLib.ftl" as compliance>
      <nav class="scp-footer-legal" aria-label="${uiLabelMap.ComplianceLegal!"Legal"}"><@compliance.footerLinks/><@compliance.footerActions/></nav>
      <@compliance.consentUi/>
    </#if>
    <@row>
        <@cell columns=12>
         <small>
         ${uiLabelMap.CommonCopyright} (c) 2014-${nowTimestamp?string("yyyy")} <a href="https://www.ilscipio.com" target="_blank">ilscipio GmbH</a>. ${uiLabelMap.CommonPoweredBy} <a href="http://www.scipioerp.com" target="_blank">SCIPIO ERP</a>.
         View <a href="<@pageUrl>license</@pageUrl>">LICENSE</a>.
         <#include "ofbizhome://runtime/svninfo.ftl" /> <#include "ofbizhome://runtime/gitinfo.ftl" />
         </small>
        </@cell>
    </@row>
</footer> <#-- END FOOTER -->

  <@scripts output=true> <#-- ensure @script elems here will always output -->
    <#-- New in scipio; priority footer javascripts (before screen footer javascripts) -->
    <#if layoutSettings.VT_FTPR_JAVASCRIPT?has_content>
        <#--layoutSettings.javaScripts is a list of java scripts. -->
        <#-- use a Set to make sure each javascript is declared only once, but iterate the list to maintain the correct order -->
        <#assign javaScriptsSet = toSet(layoutSettings.VT_FTPR_JAVASCRIPT)/>
        <#list layoutSettings.VT_FTPR_JAVASCRIPT as javaScript>
            <#if javaScriptsSet.contains(javaScript)>
                <#assign nothing = javaScriptsSet.remove(javaScript)/>
                <@script src=makeContentUrl(javaScript) />
            </#if>
        </#list>
    </#if>

    <#-- New in scipio; for app scripts that aren't (exclusively) styling but must go at end of page -->
    <#if layoutSettings.javaScriptsFooter?has_content>
        <#assign javaScriptsSet = toSet(layoutSettings.javaScriptsFooter)/>
        <#list layoutSettings.javaScriptsFooter as javaScript>
            <#if javaScriptsSet.contains(javaScript)>
                <#assign nothing = javaScriptsSet.remove(javaScript)/>
                <@script src=makeContentUrl(javaScript) />
            </#if>
        </#list>
    </#if>

    <#-- For theme styling-related scripts -->
    <#if layoutSettings.VT_FTR_JAVASCRIPT?has_content>
        <#--layoutSettings.javaScripts is a list of java scripts. -->
        <#-- use a Set to make sure each javascript is declared only once, but iterate the list to maintain the correct order -->
        <#assign javaScriptsSet = toSet(layoutSettings.VT_FTR_JAVASCRIPT)/>
        <#list layoutSettings.VT_FTR_JAVASCRIPT as javaScript>
            <#if javaScriptsSet.contains(javaScript)>
                <#assign nothing = javaScriptsSet.remove(javaScript)/>
                <@script src=makeContentUrl(javaScript) />
            </#if>
        </#list>
    </#if>

    <#-- New in scipio; always-bottom guaranteed-last javascripts -->
      <#assign scpScriptBuffer = getRequestVar("scipioScriptBuffer")!"">
      <#if scpScriptBuffer?has_content><#-- output of merged inline scripts -->
          <@script merge=false>${scpScriptBuffer}</@script>
      </#if>
    <#if layoutSettings.VT_BTM_JAVASCRIPT?has_content>
        <#--layoutSettings.javaScripts is a list of java scripts. -->
        <#-- use a Set to make sure each javascript is declared only once, but iterate the list to maintain the correct order -->
        <#assign javaScriptsSet = toSet(layoutSettings.VT_BTM_JAVASCRIPT)/>
        <#list layoutSettings.VT_BTM_JAVASCRIPT as javaScript>
            <#if javaScriptsSet.contains(javaScript)>
                <#assign nothing = javaScriptsSet.remove(javaScript)/>
                <@script src=makeContentUrl(javaScript) />
            </#if>
        </#list>
    </#if>
  </@scripts>
</body>
</html>
