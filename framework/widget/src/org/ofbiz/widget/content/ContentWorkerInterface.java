/*******************************************************************************
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *******************************************************************************/
package org.ofbiz.widget.content;

import java.io.IOException;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.GeneralException;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.LocalDispatcher;

/**
 * ContentWorkerInterface
 */
public interface ContentWorkerInterface {

    // helper methods
    public GenericValue getCurrentContentExt(Delegator delegator, List<Map<String, ? extends Object>> trail, GenericValue userLogin, Map<String, Object> ctx, Boolean nullThruDatesOnly, String contentAssocPredicateId)  throws GeneralException;
    public GenericValue getWebSitePublishPointExt(Delegator delegator, String contentId, boolean ignoreCache) throws GenericEntityException;
    public String getMimeTypeIdExt(Delegator delegator, GenericValue view, Map<String, Object> ctx);

    // new rendering methods
    public void renderContentAsTextExt(LocalDispatcher dispatcher, Delegator delegator, String contentId, Appendable out, Map<String, Object> templateContext, Locale locale, String mimeTypeId, boolean cache) throws GeneralException, IOException;
    public String renderContentAsTextExt(LocalDispatcher dispatcher, Delegator delegator, String contentId, Map<String, Object> templateContext, Locale locale, String mimeTypeId, boolean cache) throws GeneralException, IOException;

    public void renderSubContentAsTextExt(LocalDispatcher dispatcher, Delegator delegator, String contentId, Appendable out, String mapKey, Map<String, Object> templateContext, Locale locale, String mimeTypeId, boolean cache) throws GeneralException, IOException;
    public String renderSubContentAsTextExt(LocalDispatcher dispatcher, Delegator delegator, String contentId, String mapKey, Map<String, Object> templateContext, Locale locale, String mimeTypeId, boolean cache) throws GeneralException, IOException;

    // SCIPIO: 2018-09-26: Below default methods are forward-compatibility implementations for recent upstream changes where delegator param was removed from these interfaces.
    // TODO?: REVIEW: might be tempted to flip the delegation logic around later on...

    default void renderContentAsTextExt(LocalDispatcher dispatcher, String contentId, Appendable out, Map<String, Object> templateContext, Locale locale, String mimeTypeId, boolean cache) throws GeneralException, IOException {
        renderContentAsTextExt(dispatcher, dispatcher.getDelegator(), contentId, out, templateContext, locale, mimeTypeId, cache);
    }

    default String renderContentAsTextExt(LocalDispatcher dispatcher, String contentId, Map<String, Object> templateContext, Locale locale, String mimeTypeId, boolean cache) throws GeneralException, IOException {
        return renderContentAsTextExt(dispatcher, dispatcher.getDelegator(), contentId, templateContext, locale, mimeTypeId, cache);
    }

    default void renderSubContentAsTextExt(LocalDispatcher dispatcher, String contentId, Appendable out, String mapKey, Map<String, Object> templateContext, Locale locale, String mimeTypeId, boolean cache) throws GeneralException, IOException {
        renderSubContentAsTextExt(dispatcher, dispatcher.getDelegator(), contentId, out, mapKey, templateContext, locale, mimeTypeId, cache);
    }

    default String renderSubContentAsTextExt(LocalDispatcher dispatcher, String contentId, String mapKey, Map<String, Object> templateContext, Locale locale, String mimeTypeId, boolean cache) throws GeneralException, IOException {
        return renderSubContentAsTextExt(dispatcher, dispatcher.getDelegator(), contentId, mapKey, templateContext, locale, mimeTypeId, cache);
    }
}
