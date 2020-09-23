<#include "../common/common.ftl">


<#macro menuContent menuArgs={}>
    <@row>
        <@cell columns=6>
            <@menu args=menuArgs>
                <@menuitem type="generic">
                    <@modal id="modal_new_image_size_preset" label=uiLabelMap.CmsCreateCustomImageSizePreset linkClass="+${styles.menu_button_item_link!} ${styles.action_nav!} ${styles.action_add!}">
                        <@heading>${uiLabelMap.CmsCreateCustomImageSizePreset}</@heading>
                        <div class="cmsmedia-customvariantsize-area">
                            <form method="post" action="<@pageUrl>createCustomImageSizePreset</@pageUrl>" name="customVariantSizesForm">
                                <@field type="text" label=uiLabelMap.CmsMediaCustomSizeVariantsPresetId required=false name="presetId" value="" maxlength=20/>
                                <@field type="text" label=uiLabelMap.CmsMediaCustomSizeVariantsPresetName required=true name="presetName" value=""/>
                                <hr/>
                                <div class="cmsmedia-customvariantsize-method customVariantSizesForm">
                                    <@customVariantSizeForm />
                                </div>
                                <@row class="+cmsmedia-customvariantsize-add-cnt">        
                                    <@cell class="+${styles.grid_large_offset}11" columns=1>
                                        <a href="javascript:void(0);" class="cmsmedia-customvariantsize-add">[+]</a>
                                    </@cell>
                                </@row>
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
            <@paginate mode="content" url=makePageUrl("customImageSizePresets") viewSize=(viewSize!10) viewIndex=(viewIndex!0) listSize=(listSize!0)>
                <@table type="data-list" autoAltRows=true width="100%">
                    <@thead>
                        <@tr class="header-row">
                            <@th>${uiLabelMap.CmsMediaCustomSizeVariantsPresetId}</@th>
                            <@th>${uiLabelMap.CmsMediaCustomSizeVariantsPresetName}</@th>
                            <@th width="10%">${uiLabelMap.CmsMediaViewCustomSizeVariants}</@th>
                            <@th width="10%">${uiLabelMap.CommonUpdate}</@th>
                        </@tr>
                    </@thead>
                    <#list customImageSizePresets as preset>
                        <@tr>
                            <form method="post" action="" name="customImageSizePresets_${preset_index}">
                                <input type="hidden" name="VIEW_SIZE" value="${viewSize!}"/>
                                <input type="hidden" name="VIEW_INDEX" value="${viewIndex!}"/>
                                <input name="presetId" type="hidden" value="${preset.presetId}"/>
                                <@td>${preset.presetId}</@td>
                                <@td><@field type="text" name="presetName" value=preset.presetName! required=true /></@td>
                                <@td>
                                    <@modal id=("modal_view_image_sizes_"+(preset?index)) label=uiLabelMap.CommonView linkClass="+${styles.menu_button_item_link!} ${styles.action_nav!} ${styles.action_edit!}">
                                        <@heading>${uiLabelMap.CmsMediaViewCustomSizeVariants}</@heading>
                                        <@table autoAltRows=true>
                                            <@thead>
                                                <@tr class="header-row">
                                                    <@th width="60%">${uiLabelMap.ImageCustomVariantSizeName}</@th>
                                                    <@th width="10%">${uiLabelMap.ImageCustomVariantSizeWidth}</@th>
                                                    <@th width="10%">${uiLabelMap.ImageCustomVariantSizeHeight}</@th>
                                                    <@th width="10%">${uiLabelMap.CommonFormat}</@th>
                                                    <@th width="10%">${uiLabelMap.ImageCustomVariantSizeUpscaleMode}</@th>
                                                </@tr>
                                            </@thead>
                                            <@tbody>
                                            <#assign imageSizes = preset.getRelated("ImageSize")!>
                                            <#if imageSizes?has_content>
                                                <#list imageSizes as imageSize>
                                                    <@tr>
                                                        <#assign imageSizeDimension = imageSize.getRelatedOne("ImageSizeDimension")!>
                                                        <@td>${imageSizeDimension.sizeName!}</@td>
                                                        <@td>${imageSizeDimension.dimensionWidth!}</@td>
                                                        <@td>${imageSizeDimension.dimensionHeight!}</@td>
                                                        <@td>${imageSizeDimension.format!"original"}</@td>
                                                        <@td>${imageSizeDimension.upscaleMode!"on"}</@td>
                                                    </@tr>
                                                </#list>
                                            </#if>
                                            </@tbody>
                                      </@table>
                                    </@modal>
                                </@td>
                                <@td>
                                    <a href="javascript:document.forms['customImageSizePresets_${preset_index}'].action='${makePageUrl('updateCustomImageSizePreset')}'; 
                                        document.forms['customImageSizePresets_${preset_index}'].submit();" 
                                        class="${styles.link_run_sys!} ${styles.action_update!}">${uiLabelMap.CommonUpdate}</a>
                                </@td>
                            </form>
                        </@tr>
                    </#list>
                </@table>
            </@paginate>
        </@section>
    <#else>
      <@commonMsg type="result-norecord"/>
    </#if>
</@section>