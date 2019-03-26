<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<form id="addMultipleImagesForm" name="addMultipleImagesForm" method="post" action="<@pageUrl>addImageForProduct</@pageUrl>" enctype="multipart/form-data">

  <@field type="lookup" label=uiLabelMap.ProductProductId name="productId" id="productId" formName="addMultipleImagesForm" fieldFormName="LookupProduct" value=(productId!)/>

  <@fields type="default-nolabelarea">
    <@field type="select" name="imageResize">
      <option selected="selected" value="">${uiLabelMap.ImageManagementDoNotResize}</option>
      <#list productFeatures as productFeature>
        <option value="${productFeature.abbrev!}">${productFeature.description!}</option>
      </#list>
    </@field>
    <#macro imageField name>
      <@field type="file" size="20" name=name/>
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
    <@field type="submit" text=uiLabelMap.CommonUpload/>
  </@fields>
</form>
