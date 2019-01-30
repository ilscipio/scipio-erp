<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<p>NOTE: These report are for demonstration purposes only.
They use the JasperReports reporting tool. They have not been polished yet, but
they are good examples of creating detailed reports that you have a lot of
control over. special thanks for Britton LaRoche for creating the first pass of
these reports and helping to improve them.</p>

<form method="post" name="orderreportform" action="<@pageUrl>orderreportjasper.pdf</@pageUrl>" target="OrderReport">
  <@field type="datetime" label="From Date" name="fromDate" value="" size="22" maxlength="25" id="fromDate1"/>
  <@field type="datetime" label="To Date" name="toDate" value="" size="22" maxlength="25" id="toDate1"/>
<#--
  <@field type="select" label="Report" name="groupName" tabindex="14" class="+stateSelectBox">
        <option value="orderStatus"></option>
        <option value="orderStatus">Orders by Order Status</option>
        <option value="ship">Orders by Ship Method</option>
        <option value="payment">Orders by Payment Method</option>
        <option value="adjustment">Order Items by Adjustment</option>
        <option value="itemStatus">Order Items by Status</option>
        <option value="product">Order Items by Product</option>
  </@field>
-->
  <@field type="submit" tabindex="16" name="GoReport" text="Order Report" class="+${styles.link_run_sys!} ${styles.action_export!}"/>
</form>

<form method="post" name="itemreportform" action="<@pageUrl>orderitemreportjasper.pdf</@pageUrl>" target="OrderReport">
  <@field type="datetime" label="From Date" name="fromDate" value="" size="22" maxlength="25" id="fromDate2"/>
  <@field type="datetime" label="To Date" name="toDate" value="" size="22" maxlength="25" id="toDate2"/>
<#--
  <@field type="select" label="Report" name="groupName" tabindex="14"  class="+stateSelectBox">
        <option value="orderStatus"></option>
        <option value="orderStatus">Orders by Order Status</option>
        <option value="ship">Orders by Ship Method</option>
        <option value="payment">Orders by Payment Method</option>
        <option value="adjustment">Order Items by Adjustment</option>
        <option value="itemStatus">Order Items by Status</option>
        <option value="product">Order Items by Product</option>
  </@field>
-->
  <@field type="submit" tabindex="16" name="GoReport" text="Item Report"  class="+${styles.link_run_sys!} ${styles.action_export!}"/>
</form>
