<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<@script>
function submit_add() {
    window.close();
    document.addSubSite.submit();
}
function win_cancel() {
    window.close();
}
</@script>

<@section>
<form name="addSubSite" method="post" action="<@ofbizUrl>postNewSubSite?rootForumId=${requestParameters.rootForumId}</@ofbizUrl>">
  <@field type="input" label="Site Name:" size="20" name="contentName"/>
  <@field type="input" label="Site Description:" size="40" name="description"/>
  <@field type="select" label="Posted Msg Default Status:" name="statusId">
        <option value="CTNT_IN_PROGRESS">Draft - not attached to any site</option>
        <option value="CTNT_FINAL_DRAFT">Final Draft - but must be approve (moderated)</option>
        <option value="CTNT_PUBLISHED">Publish immediately</option>
  </@field>
  <@field type="submitarea"> 
      <@field type="submit" name="submitBtn" text="Create" class="+${styles.link_run_sys!} ${styles.action_add!}"/>
      <#--
      <@field type="submit" submitType="link" href="javascript:submit_add()" class="+${styles.link_run_sys!} ${styles.action_add!}" text="Create" />
      <@field type="submit" submitType="link" href="javascript:win_cancel()" class="+${styles.link_nav_cancel!}" text="Cancel" />
      -->
  </@field>
<input type="hidden" name="contentIdTo" value="${requestParameters.parentForumId}" />
<input type="hidden" name="ownerContentId" value="${requestParameters.parentForumId}" />
<input type="hidden" name="contentTypeId" value="WEB_SITE_PUB_PT" />
<input type="hidden" name="contentAssocTypeId" value="SUBSITE" />

</form>
</@section>
