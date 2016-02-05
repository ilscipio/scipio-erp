<#--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
-->
<div id="payGiftCard" style="display:none">
  <@table type="fields" class="+${styles.table_spacing_tiny_hint!}" border="0" width="100%"> <#-- orig: class="" --> <#-- orig: cellspacing="" -->
    <@tr>
      <@td colspan="2">&nbsp;</@td>
    </@tr>
    <@tr>
      <@td width="100%" align="center" colspan="2">
        <b>${uiLabelMap.WebPosTransactionTotalDue} <span id="giftCardTotalDue"/></b>
      </@td>
    </@tr>
    <@tr>
      <@td width="100%" align="center" colspan="2">
        <b>${uiLabelMap.WebPosPayGiftCardTotal} <span id="giftCardTotalPaid"/></b>
        <a id="removeGiftCardTotalPaid" href="javascript:void(0);" class="${styles.link_type_image!} ${styles.action_run_sys!} ${styles.action_remove!}"><img src="/images/collapse.gif"/></a>
      </@td>
    </@tr>
    <@tr>
      <@td width="50%" align="right">${uiLabelMap.WebPosPayGiftCard}</@td>
      <@td width="50%" align="left"><input type="text" id="amountGiftCard" name="amountGiftCard" size="10" value=""/></@td>
    </@tr>
    <@tr>
      <@td width="50%" align="right">${uiLabelMap.WebPosPayGiftCardRefNum}</@td>
      <@td width="50%" align="left"><input type="text" id="refNumGiftCard" name="refNum" size="10" value=""/></@td>
    </@tr>
    <@tr>
      <@td colspan="2">&nbsp;</@td>
    </@tr>
    <@tr>
      <@td colspan="2" align="center">
        <input type="submit" value="${uiLabelMap.CommonConfirm}" id="payGiftCardConfirm" class="${styles.link_run_sys!} ${styles.action_update!}"/>
        <input type="submit" value="${uiLabelMap.CommonCancel}" id="payGiftCardCancel" class="${styles.link_nav_cancel!}"/>
      </@td>
    </@tr>
    <@tr>
      <@td colspan="2"><div class="errorPosMessage"><span id="payGiftCardFormServerError"/></div></@td>
    </@tr>
  </@table>
</div>