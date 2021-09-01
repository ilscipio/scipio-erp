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

        

            <!-- /.container-fluid -->




        <!-- /#page-wrapper -->

    <!-- /#wrapper -->
        </main>
        <#if userLogin?has_content>
            <#if (requestAttributes.externalLoginKey)??><#assign externalKeyParam = "?externalLoginKey=" + (requestAttributes.externalLoginKey!)></#if>
            <#if (externalLoginKey)??><#assign externalKeyParam = "?externalLoginKey=" + (requestAttributes.externalLoginKey!)></#if>
            <#assign ofbizServerName = application.getAttribute("_serverId")!"default-server">
            <#assign contextPath = request.getContextPath()/>
            <#assign displayApps = Static["org.ofbiz.webapp.control.LoginWorker"].getAppBarWebInfos(security, userLogin, ofbizServerName, "main")>
            <#assign displaySecondaryApps = Static["org.ofbiz.webapp.control.LoginWorker"].getAppBarWebInfos(security, userLogin, ofbizServerName, "secondary")>
            <#assign appModelMenu = Static["org.ofbiz.widget.model.MenuFactory"].getMenuFromLocation(applicationMenuLocation,applicationMenuName)>

            <#macro primaryAppsContent>
                <#assign appCount = 0>
                <#assign firstApp = true>
            <#--  <li class="nav-item"><label>${uiLabelMap["CommonPrimaryApps"]}</label></li>-->
                <#list displayApps as display>
                    <#assign thisApp = display.getContextRoot()>
                    <#assign selected = false>
                    <#if thisApp == contextPath || contextPath + "/" == thisApp>
                        <#assign selected = true>
                    </#if>
                    <#assign servletPath = Static["org.ofbiz.webapp.WebAppUtil"].getControlServletPathSafeSlash(display)!"">
                    <#assign thisURL = raw(servletPath)>
                    <#if thisApp != "/">
                        <#if servletPath?has_content>
                            <#assign thisURL = thisURL + "main">
                        <#else>
                            <#assign thisURL = thisApp>
                        </#if>
                    </#if>
                    <#if layoutSettings.suppressTab?? && display.name == layoutSettings.suppressTab>
                    <#-- do not display this component-->
                    <#else>
                        <li class="list-group-item list-group-item-divider small <#if selected> list-group-item-accent-warning </#if>">
                            <a href="${thisURL}${raw(externalKeyParam)}" class="<#if selected> active</#if>"
                            <#if uiLabelMap??> title="${uiLabelMap[display.description]}">
                                ${uiLabelMap[display.title]}
                    <#else> title="${display.description}">
                                ${display.title}
                    </#if>
                        <#if styles.app_icon[display.name]?has_content>

                            <div class="float-right">
                                <i class="${styles.icon!} ${styles.app_icon[display.name]}"></i>
                            </div>
                        </#if>

                        </a>
                        </li>
                        <#assign appCount = appCount + 1>
                    </#if>
                </#list>
            </#macro>

            <#macro secondaryAppsContent>
            <#--<li class="nav-item"><label>${uiLabelMap["CommonSecondaryApps"]}</label></li>-->
                <#list displaySecondaryApps as display>
                    <#assign thisApp = display.getContextRoot()>
                    <#assign selected = false>
                    <#if thisApp == contextPath || contextPath + "/" == thisApp>
                        <#assign selected = true>
                    </#if>
                    <#assign servletPath = Static["org.ofbiz.webapp.WebAppUtil"].getControlServletPathSafeSlash(display)!"">
                    <#assign thisURL = raw(servletPath)>
                    <#if thisApp != "/">
                        <#if servletPath?has_content>
                            <#assign thisURL = thisURL + "main">
                        <#else>
                            <#assign thisURL = thisApp>
                        </#if>
                    </#if>
                    <li class="list-group-item list-group-item-divider small <#if selected> list-group-item-accent-warning </#if>">
                        <a href="${thisURL}${raw(externalKeyParam)}" class="<#if selected> active</#if>"
                            <#if uiLabelMap??> title="${uiLabelMap[display.description]}">
                               ${uiLabelMap[display.title]}
                <#else> title="${display.description}">
                                ${display.title}
                </#if>
                    <#if styles.app_icon[display.name]?has_content>

                        <div class="float-right">
                                <i class="${styles.icon!} ${styles.app_icon[display.name]}"></i>
                        </div>
                    </#if>
                    </a>
                    <#assign appCount = appCount + 1>
                    </li>
                </#list>
            </#macro>


        <#-- (implemented in tail of header.ftl) -->
        <aside class="aside-menu">
                <ul class="nav nav-tabs" role="tablist">
                    <li class="nav-item">
                        <a class="nav-link active" data-toggle="tab" href="#apps" role="tab">
                            <i class="icon-list"></i>
                        </a>
                    </li>
                </ul>

                <div class="tab-content">
                    <div class="tab-pane active" id="apps" role="tabpanel">
                        <ul class="list-group list-group-accent">
                            <li class="list-group-item list-group-item-accent-primary bg-light text-center font-weight-bold text-muted text-uppercase small">
                                ${uiLabelMap["CommonPrimaryApps"]}
                            </li>
                            <@primaryAppsContent/>
                            <li class="list-group-item list-group-item-accent-success bg-light text-center font-weight-bold text-muted text-uppercase small">
                                ${uiLabelMap["CommonSecondaryApps"]}
                            </li>
                            <@secondaryAppsContent/>
                        </ul>
                    </div>
                </div>
        </aside>
        </#if>
    </div>
    <footer class="app-footer">
            ${uiLabelMap.CommonCopyright} (c) 2014-${nowTimestamp?string("yyyy")} <a href="https://www.ilscipio.com" target="_blank">ilscipio GmbH</a>.<span class="float-right">${uiLabelMap.CommonPoweredBy} <a href="http://www.scipioerp.com" target="_blank">SCIPIO ERP</a>. <#include "ofbizhome://runtime/svninfo.ftl" /> <#include "ofbizhome://runtime/gitinfo.ftl" /></span>
    </footer>


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
                <@script src=makeOfbizContentUrl(javaScript) />
            </#if>
        </#list>
    </#if>
  </@scripts>
</body>
</html>
