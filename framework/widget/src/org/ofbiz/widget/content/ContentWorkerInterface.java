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
