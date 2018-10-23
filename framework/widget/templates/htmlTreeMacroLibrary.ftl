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
<#include "htmlCommonMacroLibrary.ftl">
<#-- 
SCIPIO: NOTE: since macro renderer initial context mod, macros here now have access to a few widget context objects part of the initial
context, such as request, response, etc. however it is only from the initial context,
not "current" context (too intrusive in current renderer design). still relies on macro params.
2016-01-06: the globalContext is now also dumped into the data model, so uiLabelMap should be available.

NOTE: 2016-10-05: Widget early HTML encoding is now DISABLED for all HTML macros.
    As a result all macros here must take care to html-escape as well as js-escape values.
    Use escapeVal/escapeFullUrl for this.
-->
<#macro renderNodeBegin style extraArgs...>
<#if style?has_content><ul class="${escapeVal(style, 'html')}"></#if>
<li><#rt/>
</#macro>

<#macro renderLastElement style extraArgs...>
<ul<#if style?has_content> class="${escapeVal(style, 'html')}"</#if>>
<#rt/>
</#macro>
  
<#macro renderNodeEnd processChildren isRootNode extraArgs...>
<#if processChildren?has_content && processChildren>
</ul><#lt/>
</#if>
</li><#rt/>
<#if isRootNode?has_content && isRootNode>
</ul><#lt/>
</#if> 
</#macro>
 
<#macro renderLabel id style labelText extraArgs...>
<span<#if id?has_content> id="${escapeVal(id, 'html')}"</#if><#if style?has_content> class="${escapeVal(style, 'html')}"</#if>><#rt/>
<#if labelText?has_content>${escapeVal(labelText, 'htmlmarkup')}</#if><#rt/>
</span>    
</#macro>

<#macro formatBoundaryComment boundaryType widgetType widgetName extraArgs...>
<!-- ${escapeVal(boundaryType, 'html')}  ${escapeVal(widgetType, 'html')}  ${escapeVal(widgetName, 'html')} -->
</#macro>

<#macro renderLink id style name title targetWindow linkUrl linkText imgStr extraArgs...>
<#-- SCIPIO: 2018-05: detect the special collapse/expand links and set a simple "icon" here, 
    otherwise functionality loss - see also extra space at end -->
<#local expColl = false>
<#if linkText == " ">
  <#if style == "expanded">
    <#local expColl = true>
    <#local linkText = "[-]">
  <#elseif style == "collapsed">
    <#local expColl = true>
    <#local linkText = "[+]">
  </#if>
</#if>
<a<#if id?has_content> id="${escapeVal(id, 'html')}"</#if><#rt/>
<#if style?has_content> class="${escapeVal(style, 'html')}"</#if><#rt/>
<#if name?has_content> name="${escapeVal(name, 'html')}"</#if><#rt/>
<#if title?has_content> title="${escapeVal(title, 'html')}"</#if><#rt/>
<#if targetWindow?has_content> target="${escapeVal(targetWindow, 'html')}</#if><#if linkUrl?has_content> href="${escapeFullUrl(linkUrl, 'html')}"<#else> href="javascript:void(0);"</#if>><#rt/>
<#if imgStr?has_content>${imgStr}<#elseif linkText?has_content/>${escapeVal(linkText, 'htmlmarkup')}<#else>&nbsp;</#if></a><#rt/>
<#if expColl> </#if><#rt/><#-- SCIPIO: extra space -->
</#macro>

<#macro renderImage src id style wid hgt border alt urlString extraArgs...>
<#if src?has_content>
<img<#if id?has_content> id="${escapeVal(id, 'html')}"</#if><#if style?has_content> class="${escapeVal(style, 'html')}"</#if><#if wid?has_content> width="${wid}"</#if><#if hgt?has_content> height="${hgt}"</#if><#if border?has_content> border="${escapeVal(border, 'html')}"</#if> alt="<#if alt?has_content>${escapeVal(alt, 'html')}</#if>" src="${escapeFullUrl(urlString, 'html')}" /><#rt/>
</#if>
</#macro>
 