<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<@script src=makeOfbizContentUrl("/images/imagemanagement/sizzle.min.js") />
<@script src=makeOfbizContentUrl("/images/imagemanagement/jQueryRotateCompressed.js") />
<style type="text/css">
    .cropbox span { 
        width: auto !important; 
        height: auto !important; 
    }
    .cropbox span canvas{ 
        top : 10px !important;
        left : 10px !important;
    }
</style>
<@script>
<#-- SCIPIO: this breaks everything (?)
jQuery.noConflict();-->
jQuery(document).ready(function(){
    var angleHold = 0;
    if((jQuery.browser.mozilla) || (jQuery.browser.msie)) {
        var rot = jQuery('.cropbox img').rotate(angleHold);
        jQuery('#rotate-left').click(function(){
            angleHold = angleHold - 45;
            rot[0].rotate(angleHold);
            jQuery('#ImageRotating_angle').val(angleHold);
        });
        jQuery('#rotate-right').click(function(){
            angleHold = angleHold + 45;
            rot[0].rotate(angleHold);
            jQuery('#ImageRotating_angle').val(angleHold);
        });
    } else {
        var rot = jQuery('.cropbox img');
            jQuery('#rotate-left').click(function(){
            angleHold = angleHold - 45;
            rot.rotate(angleHold);
            jQuery('#ImageRotating_angle').val(angleHold);
        });
        jQuery('#rotate-right').click(function(){
            angleHold = angleHold + 45;
            rot.rotate(angleHold);
            jQuery('#ImageRotating_angle').val(angleHold);
        });
    }
});
</@script>
