<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<@script>
    $("#dialog").dialog('open');
    $(function() {
        $( "#person" ).dialog({ autoOpen: true, width: 450});
    });
</@script>
<div id="person" title="Add Person">
    <@render resource="component://humanres/widget/EmplPositionScreens.xml#EditEmplPositionFulfillmentsFtl" />
</div>