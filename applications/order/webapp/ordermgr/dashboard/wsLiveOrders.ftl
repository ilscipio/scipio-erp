<@script>
      $(function(){
            var webSocket = new WebSocket('wss://' + window.location.host + '<@appUrl fullPath="false">/ws/orderdatalive/subscribe</@appUrl>');

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

            };


            function addChartData(chart, label, data) {
                myLineChart.data.datasets[0].data.push(Math.random() * 100);
                myLineChart.data.datasets[1].data.push(Math.random() * 100);
                myLineChart.data.labels.push(time)
                chart.data.labels.push(label);
                chart.data.datasets.forEach((dataset) => {
                    dataset.data.push(data);
                });
                chart.update();
            }
      });
</@script>
<@section>
    <@chart id="orderchart"  label1="" label2="" type="bar">

    </@chart>
</@section>