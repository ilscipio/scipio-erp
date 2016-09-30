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

<@menu type="button">
  <@menuitem type="link" href=makeOfbizUrl("EditProductConfigItemContent?configItemId=${configItemId}") text="${rawString(uiLabelMap.ProductProduct)} ${rawString(uiLabelMap.ProductConfigItem)} ${rawString(uiLabelMap.ProductContent)} ${rawString(uiLabelMap.CommonList)}" class="+${styles.action_nav!}" />
<#if contentId?has_content>
  <@menuitem type="link" href=makeOfbizInterWebappUrl("/content/control/EditContent?contentId=${contentId}") target='_blank' text="${rawString(uiLabelMap.ProductContent)} ${rawString(uiLabelMap.CommonPage)}" class="+${styles.action_nav!}" />
</#if>
</@menu>
<br />
<#if configItemId?has_content && productContent?has_content>
    ${updateProductContentWrapper.renderFormString(context)}
</#if>