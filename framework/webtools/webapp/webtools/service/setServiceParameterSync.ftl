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
<#include "component://webtools/webapp/webtools/service/servicecommon.ftl">
<form name="scheduleForm" method="post" action="<@ofbizUrl>scheduleServiceSync</@ofbizUrl>">

    <#list scheduleOptions as scheduleOption>
      <input type="hidden" name="${scheduleOption.name}" value="${scheduleOption.value}"/>
    </#list>

  <#-- SCIPIO: leave room for the label area because service parameter names can be long -->
  <@fields fieldArgs={"labelColumns":4}>
    <@serviceFields serviceParameters=(serviceParameters!)/>
  </@fields>

    <@field type="submit" text=uiLabelMap.CommonSubmit class="${styles.link_run_sys!} ${styles.action_begin!}" />

</form>