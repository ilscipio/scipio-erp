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
Menu styles can be set via menu-container-style attribute. The rendering will differ if one of the following classes is set
    * menu-main
    * menu-sidebar
    * menu-button
    * menu-tab // ToDo

-->
<#macro renderMenuBegin boundaryComment="" id="" style="" title="">
<#if boundaryComment?has_content>
<!-- ${boundaryComment} -->
</#if>
      <#--
          <nav>
              <h2>${navigation!}</h2>
              <ul class="side-nav">
                  <li>
      -->
      
    <#if style?contains("menu-main")>
        <li class="has-dropdown not-click active"><a href="#">${title!}</a>
            <ul class="dropdown">
    <#elseif style?contains("menu-sidebar")>
        <nav>
              <h2>${navigation!}</h2>
              <ul class="side-nav">
                  <li>
    <#elseif style?contains("menu-button")>
        <ul class="${style_button_group!} ${style_button_force!}">
    <#elseif style?contains("menu-tab")>    
        <#-- none -->
    <#elseif style?contains("button-bar")>
        <#-- right now translating button-bar menu-container-style here to avoid modifying all menu 
             styles, but this limits possible styles
             note: button-bar usually accompanied by one of: button-style-2, tab-bar; also found: no-clear -->
        <ul class="${style_button_group!} ${style_button_force!}">
    <#else>
        <#-- This is a fallback so unhandled cases don't produce invalid HTML and classes can pass through -->
        <div class="${style!}">
            <ul>
    </#if>   
       
</#macro>

<#macro renderMenuEnd boundaryComment="" style="">
            <#--</li>
            </ul>
            </nav>
            -->    
    <#--        
    <#if isSubMenu>

            </ul>
    <#else>
        </ul>
        </li>
        <#global isSubMenu=true/>
    </#if>
    -->
    <#if style?contains("menu-main")>
            </ul>
        </li>
    <#elseif style?contains("menu-sidebar")>
                </li>
            </ul>
        </nav>
    <#elseif style?contains("menu-button")>
        </ul>
    <#elseif style?contains("menu-tab")>
        <#-- none -->
    <#elseif style?contains("button-bar")>
        </ul>
    <#else>
            </ul>
        </div>
    </#if> 
    
<#if boundaryComment?has_content>
<!-- ${boundaryComment} -->
</#if>
</#macro>

<#macro renderImage src id style width height border>
<img src="${src}"<#if id?has_content> id="${id}"</#if><#if style?has_content> class="${style}"</#if><#if width?has_content> width="${width}"</#if><#if height?has_content> height="${height}"</#if><#if border?has_content> border="${border}"</#if> />
</#macro>

<#macro renderLink linkUrl parameterList targetWindow uniqueItemName actionUrl linkType="" id="" style="" name="" height="" width="" text="" imgStr="">
  <#if linkType?has_content && "hidden-form" == linkType>
<form method="post" action="${actionUrl}"<#if targetWindow?has_content> target="${targetWindow}"</#if> onsubmit="javascript:submitFormDisableSubmits(this)" name="${uniqueItemName}"><#rt/>
    <#list parameterList as parameter>
<input name="${parameter.name}" value="${parameter.value}" type="hidden"/><#rt/>
    </#list>
</form><#rt/>
  </#if>
<#if (linkType?has_content && "hidden-form" == linkType) || linkUrl?has_content>
<a<#if id?has_content> id="${id}"</#if> class="<#if style?has_content>${style}</#if>"<#if name?has_content> name="${name}"</#if><#if targetWindow?has_content> target="${targetWindow}"</#if> href="<#if "hidden-form"==linkType>javascript:document.${uniqueItemName}.submit()<#else>${linkUrl}</#if>"><#rt/>
</#if>
<#if imgStr?has_content>${imgStr}</#if><#if text?has_content>${text}</#if><#rt/>
<#if (linkType?has_content && "hidden-form" == linkType) || linkUrl?has_content></a><#rt/></#if>
</#macro>

<#macro renderMenuItemBegin style toolTip linkStr containsNestedMenus>
        <li<#if style?has_content> class="${style}"</#if><#if toolTip?has_content> title="${toolTip}"</#if>><#if linkStr?has_content>${linkStr}</#if><#if containsNestedMenus><ul></#if><#rt/>
</#macro>

<#macro renderMenuItemEnd containsNestedMenus>
<#if containsNestedMenus></ul></#if></li>
</#macro>
