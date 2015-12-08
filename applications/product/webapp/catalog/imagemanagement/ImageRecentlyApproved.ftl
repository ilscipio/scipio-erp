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
        
<@table type="generic" class="${styles.table_basic!}" cellspacing="0"> <#-- orig: class="basic-table" -->
  <@tr>
    <#list imageEntries as imageEntry>
    <#if imageEntry?has_content> <#-- Cato: WARN: entries may be null! -->
      <@td style="vertical-align:top;">
        <@table type="data-complex"> <#-- orig: class="" -->
            <#if imageEntry.approved?has_content>
                <@tr><@td>${imageEntry.date}</@td></@tr>
                <#list imageEntry.approved as show>
                    <@tr>
                        <@td>
                            <a href="<@ofbizUrl>ListImageRecentlyApproved?productId=${show.productId}&date1=${imageEntry.timeStampDate1}&date2=${imageEntry.timeStampDate2}&showDate=${imageEntry.date}</@ofbizUrl>" class="test">${show.productId}</a> - ${imageEntry.time[show_index]}
                        </@td>
                    </@tr>
                </#list>
            </#if>
        </@table>
      </@td>
    </#if>
    </#list>
  </@tr>
</@table>
