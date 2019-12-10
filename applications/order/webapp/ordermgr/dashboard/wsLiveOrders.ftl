<@script>
      $(function(){
            var webSocket = new WebSocket('wss://' + window.location.host + '<@appUrl fullPath="false">/ws/orderdata/subscribe</@appUrl>');

            webSocket.onopen = function(event){
                var msg = {

                };
              webSocket.send(JSON.stringify(msg));
            };


            webSocket.onmessage = function(event){
                 var jsonObject, message;
                 var text = event.data;
                 message = text;
                  try {
                      jsonObject =  JSON.parse(text);
                      message = jsonObject.message;
                    } catch (error) {
                    }
                  $('#testMessage code').append(message);
            };
      });
</@script>

<div id="testMessage">
    <@code>
    </@code>
</div>