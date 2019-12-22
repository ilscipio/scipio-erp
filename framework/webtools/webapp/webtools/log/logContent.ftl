<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->


<div id="log">
    <@code type="log" class="scrollable" style="word-break: break-all;"><#list logLines as logLine><div class="${logLine.type!}">${logLine.line}</div></#list><#t>
    </@code>
</div>
<@script>
function isScrolledToBottom(el) {
    return ($(el).scrollTop() + $(el).innerHeight() >= $(el)[0].scrollHeight);
}

$(function(){
    var maxLines = 1000;
    var webSocket = new WebSocket('wss://' + window.location.host + '<@appUrl fullPath="false">/ws/log/subscribe</@appUrl>');

    webSocket.onopen = function(event){
        var msg = {

        };
      webSocket.send(JSON.stringify(msg));
      $('#log pre').scrollTop($('#log pre').prop('scrollHeight'));
    };


    webSocket.onmessage = function(event){
         var jsonObject, message,type;
         var atBottom = isScrolledToBottom($('#log pre'));
         jsonObject = JSON.parse(event.data);
          try {
              message = jsonObject.message;
              type = jsonObject.type;
              if($('#log code').children().length == maxLines){
                $('#log code').first().remove();
              }
              $('<div/>', {
                    class: type.standardLevel
              }).html(message).appendTo('#log code');
              if(atBottom){
                $('#log pre').scrollTop($('#log pre').prop('scrollHeight'));
              }

            } catch (error) {
            }
    };
});
</@script>