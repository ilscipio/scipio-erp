<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<form method="post" name="lookuporder" id="lookuporder" action="<@pageUrl>FindRequest</@pageUrl>" >
<input type="hidden" name="viewSize" value="${viewSize}"/>
<input type="hidden" name="viewIndex" value="${viewIndex}"/>

<@section title=uiLabelMap.OrderFindOrder>
  <@row>
    <@cell columns=9>
      <@field type="input" label=uiLabelMap.OrderOrderId name="orderId"/>

      <@field type="generic" label=uiLabelMap.CommonDateFilter>
          <@field type="datetime" dateType="datetime" label=uiLabelMap.CommonFrom name="minDate" value=(requestParameters.minDate!) size="25" maxlength="30" id="minDate1" collapse=true/>
          <@field type="datetime" dateType="datetime" label=uiLabelMap.CommonThru name="maxDate" value=(requestParameters.maxDate!) size="25" maxlength="30" id="maxDate" collapse=true/>
      </@field>
      
        <@fieldset title=uiLabelMap.CommonAdvancedSearch collapsed=true>
          
        </@fieldset>
        <input type="hidden" name="showAll" value="Y"/>
        <@field type="submit" text=uiLabelMap.CommonFind class="+${styles.link_run_sys!} ${styles.action_find!}"/>
    </@cell>
  </@row>    
</@section>
<input type="image" src="<@contentUrl>/images/spacer.gif</@contentUrl>" onclick="javascript:lookupOrders(true);"/>
</form>
