<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<@script>
jQuery(document).ready( function() {
<#if 0 < productPriceConds.size()>
  <#list 0..productPriceConds.size()-1 as i>
    if (document.getElementById('EditProductPriceRulesCond_o_${i}')) {
      jQuery('#EditProductPriceRulesCond_condValueInput_o_${i}').hide();
      jQuery('#EditProductPriceRulesCond_inputParamEnumId_o_${i}').change( function() {
        getDependentDropdownValues('getAssociatedPriceRulesConds', 'inputParamEnumId', 'EditProductPriceRulesCond_inputParamEnumId_o_${i}', 'EditProductPriceRulesCond_condValue_o_${i}', 'productPriceRulesCondValues', 'condValue_o_${i}', 'description', '${productPriceConds[i].condValue}', '', '', '', '', 'EditProductPriceRulesCond_condValueInput_o_${i}');
    });
    getDependentDropdownValues('getAssociatedPriceRulesConds', 'inputParamEnumId', 'EditProductPriceRulesCond_inputParamEnumId_o_${i}', 'EditProductPriceRulesCond_condValue_o_${i}', 'productPriceRulesCondValues', 'condValue_o_${i}', 'description', '${productPriceConds[i].condValue}', '', '', '', '', 'EditProductPriceRulesCond_condValueInput_o_${i}');
    }
  </#list>
</#if>
  if (document.getElementById('AddProductPriceRulesCond_o_0')) {
    jQuery('#AddProductPriceRulesCond_condValueInput_o_0').hide();
    jQuery('#AddProductPriceRulesCond_inputParamEnumId_o_0').change( function() {
      getDependentDropdownValues('getAssociatedPriceRulesConds', 'inputParamEnumId', 'AddProductPriceRulesCond_inputParamEnumId_o_0', 'AddProductPriceRulesCond_condValue_o_0', 'productPriceRulesCondValues', 'condValue_o_0', 'description', '', '', '', '', '', 'AddProductPriceRulesCond_condValueInput_o_0');
    });
    getDependentDropdownValues('getAssociatedPriceRulesConds', 'inputParamEnumId', 'AddProductPriceRulesCond_inputParamEnumId_o_0', 'AddProductPriceRulesCond_condValue_o_0', 'productPriceRulesCondValues', 'condValue_o_0', 'description', '', '', '', '', '', 'AddProductPriceRulesCond_condValueInput_o_0');
  }
});
</@script>
