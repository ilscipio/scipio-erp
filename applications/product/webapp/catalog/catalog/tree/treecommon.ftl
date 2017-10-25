<#-- SCIPIO: common macros for catalog tree and forms -->

<#-- fields needed for every form submitted back to the catalog tree
    initialValues are required only if form is submitted NOT by the tree, otherwise tree automatically fills them. -->
<#macro ectCommonTreeFormFields params={} initialValues={}>
    <#-- ectTargetNodePath implements the node pre-selection; maintains the selection event if event error -->
    <@field type="hidden" name="ectTargetNodePath" value=(initialValues.ectTargetNodePath!) class="+ect-inputfield"/>
    <#-- ectNewTargetNodePath is used instead of ectTargetNodePath when event succeeds -->
    <@field type="hidden" name="ectNewTargetNodePath" value=(initialValues.ectNewTargetNodePath!) class="+ect-inputfield"/>
    <#-- id of the submitted form -->
    <@field type="hidden" name="ectSubmittedFormId" value=(initialValues.ectSubmittedFormId!) class="+ect-inputfield"/>
</#macro>