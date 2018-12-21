<#include "../common/common.ftl">


<#macro menuContent menuArgs={}>
    <@row>
        <@cell columns=6>
            <@menu args=menuArgs>
                <@menuitem type="generic">
                    <@modal id="modal_new_image_size_preset" label=uiLabelMap.CmsCreateCustomImageSizePreset linkClass="+${styles.menu_button_item_link!} ${styles.action_nav!} ${styles.action_add!}">
                        <@heading>${uiLabelMap.CmsCreateCustomImageSizePreset}</@heading>
                        <div class="cmsmedia-customvariantsize-area">
                            <form method="post" action="<@ofbizUrl>createCustomImageSizePreset</@ofbizUrl>" name="customVariantSizesForm">
                                <@field type="text" label=uiLabelMap.CmsMediaCustomSizeVariantsPresetName required=true name="presetName" value=""/>
                                <hr/>
                                <div class="cmsmedia-customvariantsize-method customVariantSizesForm">
                                    <@customVariantSizeForm />
                                </div>
                                <@td><@field type="submit" value=uiLabelMap.CommonCreate class="${styles.link_run_sys!} ${styles.action_create!}" /></@td>
                                <@commonCustomVariantSizeScript/>
                            </form>
                        </div>
                    </@modal>
                </@menuitem>
            </@menu>
        </@cell>
    </@row>
</#macro>

<@section title=uiLabelMap.CmsCustomImageSizePresets menuContent=menuContent id="customImagePresets">
    <#if customImageSizePresets?has_content>
        <@section>
            <@paginate mode="content" url=makeOfbizUrl("customImageSizePresets") viewSize=(viewSize!10) viewIndex=(viewIndex!0) listSize=(listSize!0)>
                <form method="post" action="<@ofbizUrl>media</@ofbizUrl>" name="mediaForm">
                    <input type="hidden" name="VIEW_SIZE" value="${viewSize!}"/>
                    <input type="hidden" name="VIEW_INDEX" value="${viewIndex!}"/>
                    
                    <@table type="data-list" autoAltRows=true>
                        <@thead>
                            <@tr class="header-row">
                                <@th>${uiLabelMap.CmsMediaCustomSizeVariantsPresetId}</@th>
                                <@th>${uiLabelMap.CmsMediaCustomSizeVariantsPresetName}</@th>
                                <@th>${uiLabelMap.CommonUpdate}</@th>
                            </@tr>
                        </@thead>
                        <#list customImageSizePresets as preset>
                            <@tr>
                                <@td>${preset.presetId}</@td>
                                <@td><@field type="text" name="presetName" value=preset.presetName! required=true /></@td>
                                <@td>
                                    <form id="ImageSizePreset_${preset_index}" name="ImageSizePreset_${preset_index}" method="post" action="<@ofbizUrl>updateCustomImageSizePreset</@ofbizUrl>">
                                        <input name="presetId" type="hidden" value="${preset.presetId}"/>
                                        <@field type="submit" value=uiLabelMap.CommonUpdate class="${styles.link_run_sys!} ${styles.action_update!}" />
                                    </form>
                                </@td>
                            </@tr>
                        </#list>
                    </@table>
                </form>
            </@paginate>
        </@section>
    <#else>
      <@commonMsg type="result-norecord"/>
    </#if>
</@section>