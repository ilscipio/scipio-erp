/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */
package com.ilscipio.scipio.webtools.widget;

import com.ilscipio.scipio.widget.def.form.*;
import com.ilscipio.scipio.widget.def.screen.SetAction;
import com.ilscipio.scipio.widget.def.screen.ServiceAction;
import com.ilscipio.scipio.widget.def.screen.FieldMap;
import com.ilscipio.scipio.widget.def.screen.EntityOneAction;
import com.ilscipio.scipio.widget.def.screen.EntityConditionAction;
import com.ilscipio.scipio.widget.def.screen.ScriptAction;
import com.ilscipio.scipio.widget.def.screen.PropertyToFieldAction;

/**
 * Auto-generated annotation-based widget definitions.
 *
 * <p>Generated from XML by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class CacheForms {

    @Form(
        name = "MemoryInfo",
        location = "component://webtools/widget/CacheForms.xml",
        defaultMapName = "memoryInfo",
        fields = {
            @FormField(name = "memory", title = "${uiLabelMap.WebtoolsTotalMemory}", display = @DisplayField),
            @FormField(name = "maxMemory", title = "${uiLabelMap.WebtoolsMaxMemory}", display = @DisplayField),
            @FormField(name = "freeMemory", title = "${uiLabelMap.WebtoolsFreeMemory}", display = @DisplayField),
            @FormField(name = "usedMemory", title = "${uiLabelMap.WebtoolsUsedMemory}", display = @DisplayField),
            @FormField(name = "totalCacheMemory", title = "${uiLabelMap.WebtoolsCacheMemory}", display = @DisplayField)
        }
    )
    public interface MemoryInfo {}

    @Form(
        name = "ListCache",
        location = "component://webtools/widget/CacheForms.xml",
        type = FormType.LIST,
        listName = "cacheList",
        paginateTarget = "FindUtilCache",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "cacheName", title = "${uiLabelMap.WebtoolsCacheName}", sortField = true, display = @DisplayField),
            @FormField(name = "cacheSize", title = "${uiLabelMap.WebtoolsSize}", sortField = true, display = @DisplayField),
            @FormField(name = "hitCount", title = "${uiLabelMap.WebtoolsHits}", sortField = true, display = @DisplayField),
            @FormField(name = "sizeLimit", title = "${uiLabelMap.WebtoolsMaxSize}", sortField = true, display = @DisplayField),
            @FormField(name = "maxInMemory", title = "${uiLabelMap.WebtoolsMaxInMemory}", sortField = true, display = @DisplayField),
            @FormField(name = "expireTime", title = "${uiLabelMap.WebtoolsExpireTime}", sortField = true, display = @DisplayField),
            @FormField(name = "useSoftReference", title = "${uiLabelMap.WebtoolsUseSoftRef}", sortField = true, display = @DisplayField),
            @FormField(name = "cacheMemory", title = "${uiLabelMap.WebtoolsCacheMemory}", sortField = true, display = @DisplayField),
            @FormField(name = "administration", title = " ", useWhen = "hasUtilCacheEdit", widgetStyle = "${styles.link_nav} ${styles.action_find}", hyperlink = @HyperlinkField(target = "FindUtilCacheElements", description = "${uiLabelMap.WebtoolsElements}", parameters = {@ParameterDef(paramName = "UTIL_CACHE_NAME", fromField = "cacheName")})),
            @FormField(name = "admin_edit", title = " ", useWhen = "hasUtilCacheEdit", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditUtilCache", description = "${uiLabelMap.CommonEdit}", parameters = {@ParameterDef(paramName = "UTIL_CACHE_NAME", fromField = "cacheName")})),
            @FormField(name = "admin_clear", title = " ", useWhen = "hasUtilCacheEdit", widgetStyle = "${styles.link_run_sys} ${styles.action_clear}", hyperlink = @HyperlinkField(target = "FindUtilCacheClear", description = "${uiLabelMap.CommonClear}", parameters = {@ParameterDef(paramName = "UTIL_CACHE_NAME", fromField = "cacheName")}))
        }
    )
    public interface ListCache {}

    @Form(
        name = "ListCacheElements",
        location = "component://webtools/widget/CacheForms.xml",
        type = FormType.LIST,
        listName = "cacheElementsList",
        paginateTarget = "FindUtilCacheElements",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "elementKey", title = "${uiLabelMap.WebtoolsCacheElementKey}", sortField = true, display = @DisplayField),
            @FormField(name = "expireTimeMillis", title = "${uiLabelMap.WebtoolsExpireTime}", sortField = true, display = @DisplayField),
            @FormField(name = "lineSize", title = "${uiLabelMap.WebtoolsBytes}", sortField = true, display = @DisplayField),
            @FormField(name = "administration", title = " ", useWhen = "hasUtilCacheEdit", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "FindUtilCacheElementsRemoveElement", description = "${uiLabelMap.CommonRemove}", parameters = {@ParameterDef(paramName = "UTIL_CACHE_NAME", fromField = "cacheName"), @ParameterDef(paramName = "UTIL_CACHE_ELEMENT_NUMBER", fromField = "keyNum")}))
        }
    )
    public interface ListCacheElements {}

    @Form(
        name = "EditCache",
        location = "component://webtools/widget/CacheForms.xml",
        target = "EditUtilCacheUpdate",
        defaultMapName = "cache",
        fields = {
            @FormField(name = "UTIL_CACHE_NAME", entryName = "cacheName", title = "${uiLabelMap.WebtoolsCacheName}", display = @DisplayField),
            @FormField(name = "cacheSize", title = "${uiLabelMap.WebtoolsSize}", display = @DisplayField),
            @FormField(name = "hitCount", title = "${uiLabelMap.WebtoolsHits}", display = @DisplayField),
            @FormField(name = "missCountTot", title = "${uiLabelMap.WebtoolsMissesTotal}", display = @DisplayField),
            @FormField(name = "missCountNotFound", title = "${uiLabelMap.WebtoolsMissesNotFound}", display = @DisplayField),
            @FormField(name = "missCountExpired", title = "${uiLabelMap.WebtoolsMissesExpire}", display = @DisplayField),
            @FormField(name = "missCountSoftRef", title = "${uiLabelMap.WebtoolsMissesSoftReference}", display = @DisplayField),
            @FormField(name = "removeHitCount", title = "${uiLabelMap.WebtoolsRemovesHit}", display = @DisplayField),
            @FormField(name = "removeMissCount", title = "${uiLabelMap.WebtoolsRemovesMisses}", display = @DisplayField),
            @FormField(name = "UTIL_CACHE_MAX_SIZE", entryName = "sizeLimit", title = "${uiLabelMap.WebtoolsMaxSize}", text = @TextField),
            @FormField(name = "UTIL_CACHE_MAX_IN_MEMORY", entryName = "maxInMemory", title = "${uiLabelMap.WebtoolsMaxInMemory}", text = @TextField),
            @FormField(name = "UTIL_CACHE_EXPIRE_TIME", entryName = "expireTime", title = "${uiLabelMap.WebtoolsExpireTime}", text = @TextField),
            @FormField(name = "UTIL_CACHE_USE_SOFT_REFERENCE", entryName = "useSoftReference", title = "${uiLabelMap.WebtoolsUseSoftRef}", dropDown = @DropDownField(options = {@Option(key = "false", description = "${uiLabelMap.CommonFalse}"), @Option(key = "true", description = "${uiLabelMap.CommonTrue}")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonApply}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditCache {}

    @Form(
        name = "FindCache",
        location = "component://webtools/widget/CacheForms.xml",
        target = "FindUtilCache",
        method = "get",
        fields = {
            @FormField(name = "cacheName", title = "${uiLabelMap.WebtoolsCacheName}", textFind = @TextFindField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", submit = @SubmitField)
        }
    )
    public interface FindCache {}

}
