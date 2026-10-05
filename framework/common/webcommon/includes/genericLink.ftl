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

<#if requestParameters?? && genericLinkName?? && genericLinkTarget?? && genericLinkText??>
<form name="${escapeVal(genericLinkName, 'html')}"<#if genericLinkWindow??> target="${escapeVal(genericLinkWindow, 'html')}"</#if> action="${escapeVal(makePageUrl(genericLinkTarget), 'html')}" method="post">
<#if (!excludeParameters?? || excludeParameters != "N") && requestParameters??>
<#assign requestParameterKeys = requestParameters.keySet().iterator()>
<#list requestParameterKeys as requestParameterKey>
<#assign requestParameterValue = requestParameters.get(requestParameterKey)!>
<#if requestParameterValue?has_content>
<input type="hidden" name="${escapeVal(requestParameterKey, 'html')}" value="${escapeVal(requestParameterValue, 'html')}"/>
</#if>
</#list>
</#if>
<a href="javascript:document['${escapeVal(genericLinkName, 'js-html')}'].submit();"<#if genericLinkStyle??> class="${escapeVal(genericLinkStyle, 'html')}"</#if>>${escapeVal(genericLinkText, 'htmlmarkup')}</a>
</form>
</#if>
