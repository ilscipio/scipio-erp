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
        <form action="<@ofbizUrl>DuplicateWorkEffort</@ofbizUrl>" method="post">
            <input type="hidden" name="oldWorkEffortId" value="${workEffortId!}"/>
            <div>
                <span>${uiLabelMap.ProductDuplicateRemoveSelectedWithNewId}</span>
                <input type="text" size="20" maxlength="20" name="workEffortId"/>&nbsp;<input type="submit" class="${styles.link_run_sys!} ${styles.action_copy!}" value="${uiLabelMap.CommonDuplicate}!"/>
            </div>
            <div>
                <span>${uiLabelMap.CommonDuplicate}</span>
                ${uiLabelMap.FormFieldTitle_rate}&nbsp;<input type="checkbox" name="duplicateWorkEffortAssignmentRates" value="Y" checked="checked"/>
                ${uiLabelMap.WorkEffortAssoc}&nbsp;<input type="checkbox" name="duplicateWorkEffortAssocs" value="Y" checked="checked"/>
                ${uiLabelMap.ProductContent}&nbsp;<input type="checkbox" name="duplicateWorkEffortContents" value="Y" checked="checked"/>
                ${uiLabelMap.WorkEffortNotes}&nbsp;<input type="checkbox" name="duplicateWorkEffortNotes" value="Y" checked="checked"/>
            </div>
            <div>
                <span>${uiLabelMap.CommonRemove}</span>
                ${uiLabelMap.FormFieldTitle_rate}&nbsp;<input type="checkbox" name="removeWorkEffortAssignmentRates" value="Y"/>
                ${uiLabelMap.WorkEffortAssoc}&nbsp;<input type="checkbox" name="removeWorkEffortAssocs" value="Y"/>
                ${uiLabelMap.ProductContent}&nbsp;<input type="checkbox" name="removeWorkEffortContents" value="Y"/>
                ${uiLabelMap.WorkEffortNotes}&nbsp;<input type="checkbox" name="removeWorkEffortNotes" value="Y"/>
            </div>
            <@field type="select" name="statusId" label=uiLabelMap.FormFieldTitle_statusId>
                <option value=""></option>
              <#list workEffortStatusList as weStatus>
                <option value="${weStatus.statusId}"<#rt/>
                    <#lt/><#if rawString(workEffort.currentStatusId!) == rawString(weStatus.statusId!)> selected="selected"</#if>>${weStatus.get("description", locale)!weStatus.statusId}</option>
              </#list>
            </@field>
        </form>