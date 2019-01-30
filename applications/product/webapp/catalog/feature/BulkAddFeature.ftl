<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<@section title="${rawLabel('ProductAddProductFeatureInBulk')} ${rawLabel('CommonFor')} ${rawString(featureCategory.description)}">
      <form method="post" action="<@pageUrl>BulkAddProductFeatures</@pageUrl>" name="selectAllForm">
        <input type="hidden" name="_useRowSubmit" value="Y" />
        <input type="hidden" name="_checkGlobalScope" value="N" />
        <input type="hidden" name="productFeatureCategoryId" value="${productFeatureCategoryId}" />
      <#-- SCIPIO: don't enable responsive on this table, comes out wrong -->
      <@table type="data-complex" autoAltRows=true>
        <@thead>
          <@tr class="header-row">
            <@th>${uiLabelMap.CommonDescription}</@th>
            <@th>${uiLabelMap.ProductFeatureType}</@th>
            <@th>${uiLabelMap.ProductIdSeqNum}</@th>
            <@th>${uiLabelMap.ProductIdCode}</@th>
            <@th align="right">${uiLabelMap.CommonAll}<input type="checkbox" name="selectAll" value="Y" checked="checked" onclick="javascript:toggleAll(this, 'selectAllForm');highlightAllRows(this, 'productFeatureTypeId_tableRow_', 'selectAllForm');" /></@th>
          </@tr>
        </@thead>
        <@tbody>
        <#list 0..featureNum-1 as feature>
          <@tr id="productFeatureTypeId_tableRow_${feature_index}" valign="middle">
              <@td><input type="text" size="15" name="description_o_${feature_index}" /></@td>
              <@td><select name="productFeatureTypeId_o_${feature_index}" size="1">
                  <#list productFeatureTypes as productFeatureType>
                  <option value="${productFeatureType.productFeatureTypeId}">${productFeatureType.get("description",locale)!}</option>
                  </#list>
                  </select>
                  <input name="productFeatureCategoryId_o_${feature_index}" type="hidden" value="${productFeatureCategoryId}" />
              </@td>
              <@td><input type="text" size="5" name="defaultSequenceNum_o_${feature_index}" /></@td>
              <@td><input type="text" size="5" name="idCode_o_${feature_index}" /></@td>
              <@td align="right"><input type="checkbox" name="_rowSubmit_o_${feature_index}" value="Y" checked="checked" onclick="javascript:checkToggle(this, 'selectAllForm');highlightRow(this,'productFeatureTypeId_tableRow_${feature_index}');" /></@td>
          </@tr>
        </#list>
        </@tbody>
        <@tfoot>
          <@tr><@td colspan="11" align="center">
            <input type="hidden" name="_rowCount" value="${featureNum}" />
            <input type="submit" value="${uiLabelMap.CommonCreate}" class="${styles.link_run_sys!} ${styles.action_add!}"/>
          </@td></@tr>
        </@tfoot>
      </@table>
      </form>
</@section>


