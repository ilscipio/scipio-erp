<#include "../common/common.ftl">


<#-- TODO
<form method="post" action="<@ofbizUrl>rebuildAllMediaVariants</@ofbizUrl>" id="mediaresizeallform">
</form>
<form method="post" action="<@ofbizUrl>rebuildAllMediaVariants</@ofbizUrl>" id="mediaresizeallforceform">
    <input type="hidden" name="force" value="true" />
</form> -->
<#macro menuContent menuArgs={}>
    <@row>
        <@cell columns=6>
            <@menu args=menuArgs>
                <@menuitem type="link" href=makeOfbizUrl("editMedia") class="+${styles.action_run_sys!} ${styles.action_add!}" text=uiLabelMap.CmsCreateMedia/>
                
                <@menuitem type="generic">
                    <@modal id="modal_new_script" label="${rawLabel('CmsMediaVariants')}: ${rawLabel('CommonRecreate')}" anchorClass="+${styles.menu_button_item_link!} ${styles.action_nav!} ${styles.action_update!}">
                        <@heading>${uiLabelMap.CmsMediaRecreateImageVariants}</@heading>
                        <p>${uiLabelMap.CmsMediaRecreateImageVariantsDesc}<br/>
                          <em>${uiLabelMap.CommonNote}: ${uiLabelMap.CmsFunctionAsyncBackground}</em></p>
                        <@fields type="default-compact">
                          <form method="post" action="<@ofbizUrl>rebuildAllMediaVariants</@ofbizUrl>">
                            <@field type="select" label=uiLabelMap.CmsTargetImages name="force" size="20">
                              <@field type="option" value="false">${uiLabelMap.CmsMediaRecreateOnlyImagesWithExistingVariants}</@field>
                              <@field type="option" value="true">${uiLabelMap.CmsMediaRecreateAllImagesForce}</@field>
                            </@field>
                            <@field type="submit" />
                          </form>
                        </@fields>
                    </@modal>
                </@menuitem>
            </@menu>  
        </@cell>
        <@cell columns=6 class="+${styles.text_right!}">
          <@form method="post" action=makeOfbizUrl("media")>
            <@field type="input" inline=true name="simpleSearchText" value=(parameters.simpleSearchText!) placeholder="${rawString(uiLabelMap.CommonSearch)}..."/>
            <@field type="submit" inline=true text=uiLabelMap.CommonFind />
          </@form>
        </@cell>
    </@row>
</#macro>
<@section title=uiLabelMap.CmsMedia menuContent=menuContent id="mediaList">
    <#if mediaFiles?has_content>
        <@section>        
            <@paginate mode="content" url=makeOfbizUrl("media") viewSize=(viewSize!10) viewIndex=(viewIndex!0) listSize=(listSize!0)>
                <form method="post" action="<@ofbizUrl>media</@ofbizUrl>" name="mediaForm">                
                    <input type="hidden" name="VIEW_SIZE" value="${viewSize!}"/>
                    <input type="hidden" name="VIEW_INDEX" value="${viewIndex!}"/>    
                    <@grid type="tiles">                                
                        <#list mediaFiles as mediaFile>
                            <#assign editMediaUrl=makeOfbizWebappUrl("/control/editMedia?contentId=" + rawString(mediaFile.contentId)) /><#--  + "&dataResourceTypeId=" + mediaFile.dataResourceTypeId -->
                            <#if mediaFile.dataResourceTypeId == "IMAGE_OBJECT">
                                <#assign contentUrl=makeOfbizWebappUrl("/media?contentId=" + mediaFile.contentId + "&dataResourceTypeId=" + mediaFile.dataResourceTypeId) />
                                <@tile image=contentUrl icon="${styles.icon} ${styles.icon_prefix}camera ${styles.icon_prefix}picture-o" link=editMediaUrl title=mediaFile.contentName!mediaFile.contentId!"" />
                            <#elseif mediaFile.dataResourceTypeId == "VIDEO_OBJECT">
                                <@tile color="3" icon="${styles.icon} ${styles.icon_prefix}play-video ${styles.icon_prefix}video-camera" link=editMediaUrl title=mediaFile.contentName!mediaFile.contentId!"" />
                            <#elseif mediaFile.dataResourceTypeId == "AUDIO_OBJECT">
                                <@tile color="4" icon="${styles.icon} ${styles.icon_prefix}sound ${styles.icon_prefix}music" link=editMediaUrl title=mediaFile.contentName!mediaFile.contentId!"" />
                            <#elseif mediaFile.dataResourceTypeId == "DOCUMENT_OBJECT">
                                <@tile color="5" icon="${styles.icon} ${styles.icon_prefix}page ${styles.icon_prefix}file" link=editMediaUrl title=mediaFile.contentName!mediaFile.contentId!"" />
                            </#if>                        
                        </#list>
                    </@grid>
                </form>
            </@paginate>
        </@section>
    <#else>
      <@commonMsg type="result-norecord"/>
    </#if>
</@section>