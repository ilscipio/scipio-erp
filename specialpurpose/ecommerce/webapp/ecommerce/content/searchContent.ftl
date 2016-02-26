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


<form method="post"  action="<@ofbizUrl>searchContent</@ofbizUrl>"  name="searchQuery">
<@table border="0" cellpadding="2" cellspacing="0">

<@tr>
<@td width="20%" align="right">
<span class="tableheadtext">${uiLabelMap.EcommerceEnterQueryParameters}</span>
</@td>
<@td>&nbsp;</@td>
<@td width="80%">
<input type="text" class="inputBox" name="queryLine" size="60"/>
</@td>
</@tr>

<!-- category form -->
<@tr>
  <@td colspan="3">
  <@table>
    <@tr>
      <@td align="right" valign="middle">${uiLabelMap.ProductFeatures}:
      </@td>
      <@td align="right" valign="middle">
          ${uiLabelMap.CommonAll} <input type="radio" name="any_or_all" value="all" checked="checked" />
          ${uiLabelMap.CommonAny} <input type="radio" name="any_or_all" value="any" />
      </@td>
    </@tr>
    <#list productFeatureTypeIdsOrdered as productFeatureTypeId>
      <#assign findPftMap = {"productFeatureTypeId":productFeatureTypeId}>
      <#assign productFeatureType = delegator.findOne("ProductFeatureType", findPftMap, true)>
      <#assign productFeatures = productFeaturesByTypeMap[productFeatureTypeId]>
      <@tr>
        <@td align="right" valign="middle">${(productFeatureType.get("description",locale))!}:
        </@td>
        <@td valign="middle">
            <select class="selectBox" name="pft_${productFeatureTypeId}">
              <option value="">- ${uiLabelMap.CommonSelectAny} -</option>
              <#list productFeatures as productFeature>
              <option value="${productFeature.productFeatureId}">${productFeature.description?default("No Description")} [${productFeature.productFeatureId}]</option>
              </#list>
            </select>
        </@td>
      </@tr>
    </#list>
    <#if searchConstraintStrings?has_content>
      <@tr>
        <@td align="right" valign="top">${uiLabelMap.ProductLastSearch}:
        </@td>
        <@td valign="top">
            <#list searchConstraintStrings as searchConstraintString>
                <div>&nbsp;-&nbsp;${searchConstraintString}</div>
            </#list>
            <div>${uiLabelMap.ProductSortedBy}: ${searchSortOrderString}</div>
            <div>
              ${uiLabelMap.ProductNewSearch}<input type="radio" name="clearSearch" value="Y" checked="checked" />
              ${uiLabelMap.CommonRefineSearch}<input type="radio" name="clearSearch" value="N" />
            </div>
        </@td>
      </@tr>
    </#if>
  </@table>
  </@td>
</@tr>    
    
<@tr>    
<@td width="20%" align="right">
&nbsp;</@td>
<@td>&nbsp;</@td>
<@td width="80%" colspan="4">
<input type="submit" class="${styles.link_run_sys!} ${styles.action_find!}" name="submitButton" value="${uiLabelMap.CommonQuery}"/>
</@td>

</@tr>
</@table>
</form>



    ${listWrapper.renderFormString()}

<#macro listSiteIds contentId indentIndex=0>
  <#assign dummy=Static["org.ofbiz.base.util.Debug"].logInfo("in listSiteIds, contentId:" + contentId,"")/>
  <#assign dummy=Static["org.ofbiz.base.util.Debug"].logInfo("in listSiteIds, indentIndex:" + indentIndex,"")/>
  <#local indent = ""/>
  <#if (0 < indentIndex)>
    <#list 0..(indentIndex - 1) as idx>
      <#local indent = indent + "&nbsp;&nbsp;"/>
    </#list>
  </#if>
<@loopSubContent contentId=contentId viewIndex=0 viewSize=9999 returnAfterPickWhen="1==1";>
  <option value="${content.contentId?lower_case}">${indent}${content.description}</option>
  <@listSiteIds contentId=content.contentId indentIndex=indentIndex + 1 />
</@loopSubContent>
</#macro>
