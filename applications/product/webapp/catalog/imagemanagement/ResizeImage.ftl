<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<@script src=makeOfbizContentUrl("/images/imagemanagement/sizzle.min.js") />
<@script>
<#-- SCIPIO: this breaks everything (?)
jQuery.noConflict();-->

jQuery(document).ready(function(){
    jQuery('img').attr('id',"previewImage");
    
    var size = getSizeVars();
    var maxWidth = size;
    var maxHeight = size;
    var ratio = 0;
    var width = jQuery('#previewImage').width();
    var height = jQuery('#previewImage').height();
    
    // Check if the current width is larger than the max
    if(width > maxWidth){
        ratio = maxWidth / width;
        jQuery('#previewImage').css("width", maxWidth); 
        jQuery('#previewImage').css("height", height * ratio); 
        height = height * ratio;
        width = width * ratio;
    }
    
    // Check if current height is larger than max
    if(height > maxHeight){
        ratio = maxHeight / height; 
        jQuery('#previewImage').css("height", maxHeight);
        jQuery('#previewImage').css("width", width * ratio);
        width = width * ratio;
    }
    
});

function getUrlVars(){
    var vars = [], hash;
    var hashes = window.location.href.slice(window.location.href.indexOf('?') + 1).split('&');
    for(var i = 0; i < hashes.length; i++){
        hash = hashes[i].split('=');
        vars.push(hash[1]);
    }
    return vars;
}

function getSizeVars(){
    var vars = [], hash;
    var result = null;
    var hashes = window.location.href.slice(window.location.href.indexOf('?') + 1).split('/');
    hash = hashes[6].split('-');
    var pathElement = hash[hash.length-1];
    result = pathElement.substring(0, pathElement.lastIndexOf(".")); 
    return result;
}
</@script>
