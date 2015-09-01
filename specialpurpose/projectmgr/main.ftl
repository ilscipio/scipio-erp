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

<@heading>${text1}</@heading>
<br />
<#if text2??>
<@heading>${text2}</@heading>
</#if><br />
<#if link1??>
<@heading>1. <a href="${link1}" target="new1">${link1Text}</a></@heading>
</#if>
<#if link2??>
<@heading>2. <a href="${link2}" target="new2">${link2Text}</a></@heading>
</#if>
<#if link3??>
<@heading>3. <a href="${link3}" target="new3">${link3Text}</a></@heading>
</#if>
