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
                      var chart = orderchart;
                      var curIndex = orderchart.data.labels.indexOf(jsonObject.global.dateTimeStr)
                      if(curIndex > -1){
                         chart.data.datasets[0].data[index] = jsonObject.global.totalAmount;
                         console.log(chart.data.datasets[0].data[index]);
                         chart.data.datasets[1].data[index] = jsonObject.global.totalOrders;
                         console.log(chart.data.datasets[1].data[index]);
                      }else{
                         <#-- Remove old data and add new -->
                         chart.data.labels.shift();
                         chart.data.datasets[0].data.shift();
                         chart.data.datasets[1].data.shift();
                         <#-- Add new element -->
                         chart.data.labels.push(jsonObject.global.dateTimeStr);
                         chart.data.datasets[0].data.push(jsonObject.global.totalAmount);
                         chart.data.datasets[1].data.push(jsonObject.global.totalOrders);
                      }
                      chart.update();
                    } catch (error) {
                        console.log(error);
                    }
            };
      });
</@script>

<#assign currData=rewrapMap(orderStats!{}, "raw-simple")/>
<#assign fieldIdNum=fieldIdNum!0/>
<@section title=title!"">
    <@chart id="orderchart"  label1=label1 label2=label2 xlabel=xlabel ylabel=ylabel type="bar">
        <#if currData?has_content>
            <#list mapKeys(currData) as key>
                <#if chartType=="bar" || chartType=="line">
                    <@chartdata value=((currData[key].total)!0) value2=((currData[key].count)!0) title=(currData[key]["dateTime"])/>
                <#else>
                    <@chartdata value=((currData[key].total)!0) title=key/>
                </#if>
            </#list>
        </#if>
    </@chart>
</@section>