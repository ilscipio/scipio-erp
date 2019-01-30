<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#include "../common.ftl">
    <#-- close the off-canvas menu -->
    <#-- 
    <a class="exit-off-canvas"></a>

    </section><#-- /<section role="main" class="scroll-container"> -->

</div><#-- /<div class="off-canvas-wrap" data-offcanvas id="body-content"> -->
</div><#-- /<div class="inner-wrap"> -->

<#-- FOOTER SECTION -->
<footer id="footer">
    <#--
    <div class="row"> 
        <div class="${styles.grid_small!}12 ${styles.grid_medium!}4 ${styles.grid_large!}4 ${styles.grid_cell!}">
            <p></p>
        </div>
    </div>-->
    <div class="row">
        <div class="${styles.grid_small!}12 ${styles.grid_medium!}12 ${styles.grid_large!}12 ${styles.grid_cell!} ${styles.text_center!}">
         <small>
         ${uiLabelMap.CommonCopyright} (c) 2014-${nowTimestamp?string("yyyy")} <a href="https://www.ilscipio.com" target="_blank">ilscipio GmbH</a>. ${uiLabelMap.CommonPoweredBy} <a href="http://www.scipioerp.com" target="_blank">SCIPIO ERP</a>. <#include "ofbizhome://runtime/svninfo.ftl" /> <#include "ofbizhome://runtime/gitinfo.ftl" />
         <#include "ofbizhome://runtime/svninfo.ftl" /> <#include "ofbizhome://runtime/gitinfo.ftl" />
         </small>
        </div>
    </div>
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
