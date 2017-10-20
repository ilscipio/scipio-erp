<#-- SCIPIO: common macros for catalog tree and forms -->

<#macro ectCommonTreeFormFields params={}>
    <#-- targetNodePath implements the node pre-selection -->
    <@field type="hidden" name="ectTargetNodePath" value=(params.ectTargetNodePath!) class="+ect-inputfield"/>
    <@field type="hidden" name="ectSubmittedFormId" value="" class="+ect-inputfield"/>
</#macro>