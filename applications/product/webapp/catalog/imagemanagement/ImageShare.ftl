<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<#assign contentURL = getPropertyValue("url.properties", "content.url.prefix.standard")!"">
<@script>
$(document).ready(function(){
    var form = document.ImageShare;
    var protocol = document.location.protocol;
    var host = document.location.host;
    var contentURL = '${contentURL}';
    var imageUrl = form.imageURL.value;
    var imageThumbUrl = form.imageThumbnail.value;
    form.direct.setAttribute('readOnly','readonly');
    form.html.setAttribute('readOnly','readonly');
    form.forumCode.setAttribute('readOnly','readonly');
    form.altForumCode.setAttribute('readOnly','readonly');
    
    if (contentURL != "") {
        var pathImageUrlEcommerce = contentURL + imageUrl;
        var pathImageUrl = contentURL + imageUrl;
        var pathImageThumbnailUrl = contentURL + imageThumbUrl;
    }
    else {
        var pathImageUrlEcommerce = "http://localhost:8080" + imageUrl;
        var pathImageUrl = "http://" + host + imageUrl;
        var pathImageThumbnailUrl = "http://"+ host + imageThumbUrl;
    }
    
    if (form.contentId.value != "") {
        pathImageUrl = $('#ImageShare_direct').html(pathImageUrl).text();
        pathImageThumbnailUrl = $('#ImageShare_direct').html(pathImageThumbnailUrl).text();
        form.link.value = pathImageUrlEcommerce;
        form.direct.value = pathImageUrl;
        form.html.value = '<a target="_blank" title="Image" href="' + pathImageUrl + '"><img src="' + pathImageUrl + '" border="0"/></a>Uploaded with <a target="_blank" href="http://images.ofbiz.com">ofbiz.com</a>';
        form.forumCode.value = "[URL=" + pathImageUrl + "/][IMG]" + pathImageUrl + "[/IMG][/URL]Uploaded with [URL=http://images.ofbiz.com]ofbiz.com[/URL]";
        form.altForumCode.value = "[URL=" + pathImageUrl + "/][IMG]" + pathImageUrl + "[/IMG][/URL]Uploaded with [URL=http://images.ofbiz.com]ofbiz.com[/URL]";
        form.forumThumbnail.value = "[URL=" + pathImageThumbnailUrl + "/][IMG]" + pathImageThumbnailUrl + "[/IMG][/URL]Uploaded with [URL=http://images.ofbiz.com]ofbiz.com[/URL]";
        form.altForumThumbnail.value = "[URL=" + pathImageThumbnailUrl + "/][IMG]" + pathImageThumbnailUrl + "[/IMG][/URL]Uploaded with [URL=http://images.ofbiz.com]ofbiz.com[/URL]";
        form.htmlThumbnail.value = '<a target="_blank" title="Image" href="' + pathImageThumbnailUrl + '"><img src="' + pathImageThumbnailUrl + '" border="0"/></a>Uploaded with <a target="_blank" href="http://images.ofbiz.com">ofbiz.com</a>';
    }
});

</@script>
