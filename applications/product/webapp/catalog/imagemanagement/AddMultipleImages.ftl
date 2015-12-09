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

  <p>${uiLabelMap.ProductProductId} <@htmlTemplate.lookupField name="productId" id="productId" formName="addMultipleImagesForm" fieldFormName="LookupProduct"/></p>

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
    <@imageField name="additionalImageOne" />
    <@imageField name="additionalImageTwo" />
    <@imageField name="additionalImageThree" />
    <@imageField name="additionalImageFour" />
    <@imageField name="additionalImageFive" />
    <@imageField name="additionalImageSix" />
    <@imageField name="additionalImageSeven" />
    <@imageField name="additionalImageEight" />
    <@imageField name="additionalImageNine" />
    <@imageField name="additionalImageTen" />
    <@field type="submit" text='${uiLabelMap.CommonUpload}'/>
  </@fields>
</form>
