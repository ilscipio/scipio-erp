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
<#--
Changes to this file: Copyright (C) Ilscipio GmbH. The changes are licensed
under the GNU Affero General Public License, version 3, or a commercial
license from Ilscipio GmbH (file LICENSE). The original code stays under
the Apache License, version 2.0, as stated above.
-->
<@panel type="callout">${uiLabelMap.CommonVisualThemeUsage}</@panel>

<@section title=uiLabelMap.CommonVisualThemes>
  
  <#if visualThemes?has_content>
    <#assign orderByList = UtilMisc.toList("visualThemeId")/>
    <@grid>  <#-- tilesType="default" -->
        <#list visualThemes as visualTheme>
        <#assign screenshots = delegator.findByAnd("VisualThemeResource", {"visualThemeId":"${visualTheme.visualThemeId}", "resourceTypeEnumId":"VT_SCREENSHOT"}, orderByList, false)>
            <li <#if visualTheme.visualThemeId == visualThemeId> class="${styles.selected!}"</#if>>
            <form name="SetUserPreferences_${visualTheme.visualThemeId}" method="post" action="<@pageUrl>setUserPreference</@pageUrl>">
              <input type="hidden" name="userPrefGroupTypeId" value="GLOBAL_PREFERENCES"/>
              <input type="hidden" name="userPrefTypeId" value="VISUAL_THEME"/>
              <input type="hidden" name="userPrefValue" value="${visualTheme.visualThemeId}"/>
            </form>
            <@pul title=visualTheme.visualThemeId!"">
                   <#if screenshots?has_content>
                      <#list screenshots as screenshot>
                        <#-- TODO: <a id="single_image" href="<@contentUrl>${screenshot.resourceValue}</@contentUrl>">-->
                        <@img src=makeContentUrl(screenshot.resourceValue) width="100%" height="250px" type="contain"/>
                        <#--<span class="${style.text_right}"><@modal icon="${styles.icon} fa-search">
                            <@img src=makeContentUrl(screenshot.resourceValue)/>
                        </@modal>
                        </span>-->
                        <#--</a>-->
                      </#list>
                   <#else>
                      <@pli>${uiLabelMap.CommonVisualThemeNoScreenshots}</@pli>
                    </#if>

                <@pli type="description">
                    ${visualTheme.get("description", locale)!visualTheme.visualThemeId}
                </@pli>
                <#if visualTheme.visualThemeId == visualThemeId>
                    <@pli type="button">
                        <a href="#" class="${styles.button_default} ${styles.disabled}">${uiLabelMap.CommonSelected}</a>
                    </@pli>
                    <#else>
                    <@pli type="button">
                        <a href="javascript:document.SetUserPreferences_${visualTheme.visualThemeId}.submit()" class="${styles.button_default}">${uiLabelMap.CommonSelect}</a>
                    </@pli>
                </#if>
            </@pul>
          </li>
      </#list>
    </@grid>
      
  </#if>
</@section>