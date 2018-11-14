<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<@script>
    $("#dialog").dialog('open');
    $(function() {
        $( "#emplposition" ).dialog({ autoOpen: true, width: 450});
    });
</@script>
<div id="emplposition" title="Add Employee Position">
    <@render resource="component://humanres/widget/EmplPositionScreens.xml#EditEmplPositionOnlyForm" />
</div>