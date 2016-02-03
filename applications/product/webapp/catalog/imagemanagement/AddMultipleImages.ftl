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

<form id="addMultipleImagesForm" name="addMultipleImagesForm" method="post" action="<@ofbizUrl>addImageForProduct</@ofbizUrl>" enctype="multipart/form-data">

  <@field type="generic" label="${uiLabelMap.ProductProductId}">
    <@htmlTemplate.lookupField name="productId" id="productId" formName="addMultipleImagesForm" fieldFormName="LookupProduct" value="${productId!}"/>
  </@field>

  <@fields type="default-nolabels">
    <@field type="generic">
      <select name="imageResize">
          <#list productFeatures as productFeature>
              <option value="${productFeature.abbrev!}">${productFeature.description!}</option>
          </#list>
          <option selected="" value="">Do not resize</option>
      </select>
    </@field>
    <#macro imageField name>
      <@field type="generic"><input type="file" size="20" name="${name}"/></@field>
    </#macro>
    <#assign namePrefix = "additionalImage">
    <@imageField name=namePrefix + "One" />
    <@imageField name=namePrefix + "Two" />
    <@imageField name=namePrefix + "Three" />
    <@imageField name=namePrefix + "Four" />
    <@imageField name=namePrefix + "Five" />
    <@imageField name=namePrefix + "Six" />
    <@imageField name=namePrefix + "Seven" />
    <@imageField name=namePrefix + "Eight" />
    <@imageField name=namePrefix + "Nine" />
    <@imageField name=namePrefix + "Ten" />
    <@field type="submit" text="${uiLabelMap.CommonUpload}"/>
  </@fields>
</form>
