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
<#include "common.ftl">
    <#-- close the off-canvas menu -->
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
                <@script src=makeOfbizContentUrl(javaScript) />
            </#if>
        </#list>
    </#if>

    <#-- New in scipio; for app scripts that aren't (exclusively) styling but must go at end of page -->
    <#if layoutSettings.javaScriptsFooter?has_content>
        <#assign javaScriptsSet = toSet(layoutSettings.javaScriptsFooter)/>
        <#list layoutSettings.javaScriptsFooter as javaScript>
            <#if javaScriptsSet.contains(javaScript)>
                <#assign nothing = javaScriptsSet.remove(javaScript)/>
                <@script src=makeOfbizContentUrl(javaScript) />
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
                <@script src=makeOfbizContentUrl(javaScript) />
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
                <@script src=makeOfbizContentUrl(javaScript) />
            </#if>
        </#list>
    </#if>
  </@scripts>
</body>
</html>
