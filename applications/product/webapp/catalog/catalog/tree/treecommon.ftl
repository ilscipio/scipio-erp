<#-- SCIPIO: common macros for catalog tree and forms -->

<#macro ectCommonTreeFormFields params={}>
    <#-- ectTargetNodePath implements the node pre-selection -->
    <@field type="hidden" name="ectTargetNodePath" value=(params.ectTargetNodePath!) class="+ect-inputfield"/>
    <#-- ectNewTargetNodePath may be used instead of ectTargetNodePath when event succeeds -->
    <@field type="hidden" name="ectNewTargetNodePath" value=(params.ectNewTargetNodePath!) class="+ect-inputfield"/>
    <@field type="hidden" name="ectSubmittedFormId" value="" class="+ect-inputfield"/>
</#macro>