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

<#assign selected = rawString(activeMainMenuItem)!"void">
<div id="app-navigation">
  <ul>
    <li>
      <@heading>${uiLabelMap.ExampleApplication}</@heading>
      <@menu type="main">
      <#if userLogin?has_content>
        <@menuitem type="link" href=makeOfbizUrl("FindExample?portalPageId=${rawLabel('ExampleExample')}") text=uiLabelMap.ExampleExample selected=(selected=="${rawLabel('ExampleExample')}") class="+${styles.action_nav!} ${styles.action_find!}" />
        <@menuitem type="link" href=makeOfbizUrl("FindExampleFeature?portalPageId=${rawLabel('ExampleFeature')}") text=uiLabelMap.ExampleFeature selected=(selected=="${rawLabel('ExampleFeature')}") class="+${styles.action_nav!}" />
        <@menuitem type="link" href=makeOfbizUrl("FormWidgetExamples?portalPageId=${rawLabel('ExampleFormWidgetExamples')}") text=uiLabelMap.ExampleFormWidgetExamples selected=(selected=="${rawLabel('ExampleFormWidgetExamples')}") class="+${styles.action_nav!}" />
        <@menuitem type="link" href=makeOfbizUrl("authview/findExampleAjax?portalPageId=${rawLabel('ExampleAjaxExamples')}") text=uiLabelMap.ExampleAjaxExamples selected=(selected=="${rawLabel('ExampleAjaxExamples')}") class="+${styles.action_nav!}" />

        <#if portalPages?has_content>
            <#list portalPages as page>
              <#if page.portalPageName?has_content>
                <@menuitem type="link" href=makeOfbizUrl("showPortalPage?portalPageId=${rawString(page.portalPageId)}") text=(page.portalPageName!"?") selected=(selected=="${rawString(page.portalPageId)}") class="+${styles.action_nav!}" />
              </#if>
            </#list>
        </#if>
        <@menuitem type="link" href=makeOfbizUrl("ManagePortalPages?parentPortalPageId=EXAMPLE") text=uiLabelMap.CommonDashboard class="+${styles.action_nav!} opposed" />
      </#if>
      </@menu>
    </li>
  </ul>
</div>
