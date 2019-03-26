<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<@script>
    $("#dialog").dialog('open');
    $(function() {
        $( "#rmvemplposition" ).dialog({ autoOpen: true, width: 900});
    });
</@script>
<div id="rmvemplposition" title="Remove Employee Position">
    <@render resource="component://humanres/widget/EmplPositionScreens.xml#RemoveEmplPositionOnlyForm" />
</div>