<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<h1>Attention!</h1>
<div>&nbsp;</div>

<@table type="fields" class="+${styles.table_spacing_tiny_hint!}" width="70%"> <#-- orig: class="" --> <#-- orig: cellspacing="" -->
  <@tr>
    <@td align="right"><b>Order #:</b></@td>
    <@td>${orderId!}</@td>
  </@tr>
  <@tr>
    <@td align="right"><b>Order Date:</b></@td>
    <@td>${orderDate!}</@td>
  </@tr>
  <@tr>
    <@td colspan="2">&nbsp;</@td>
  </@tr>
  <@tr>
    <@td align="right"><b>Estimated Start Date:</b></@td>
    <@td>${estimatedStartDate!}</@td>
  </@tr>
  <@tr>
    <@td align="right"><b>Actual Start Date:</b></@td>
    <@td>${actualStartDate!}</@td>
  </@tr>
  <@tr>
    <@td align="right"><b>Current State:<b></@td>
    <@td>${omgStatusId!} <#--WfUtil.getOMGStatus(request.getParameter("currentStatusId"))--></@td>
  </@tr>
  <@tr>
    <@td colspan="2">&nbsp;</@td>
  </@tr>

  <#list assignments as assign>
  <@tr>
    <@td align="right"><b>Assigned Party ID:</b></@td>
    <@td>${assign.partyId!}</@td>
  </@tr>
  <@tr>
    <@td align="right"><b>Assigned Role Type:</b></@td>
    <@td>${assign.roleTypeId!}</@td>
  </@tr>
  <@tr>
    <@td align="right"><b>Assignment Status:</b></@td>
    <@td>${assign.statusId!}</@td>
  </@tr>
  </#list>

  <@tr>
    <@td colspan="2">&nbsp;</@td>
  </@tr>
  <@tr>
    <@td colspan="2" align="center">
      <a href="${baseSecureUrl}/ordermgr/control/orderview?orderId=${orderId}">View Order #${orderId}</a>
    </@td>
  </@tr>
</@table>