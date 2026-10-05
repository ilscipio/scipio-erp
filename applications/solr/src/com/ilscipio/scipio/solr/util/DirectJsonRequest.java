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
package com.ilscipio.scipio.solr.util;

import org.apache.solr.client.solrj.SolrClient;
import org.apache.solr.client.solrj.SolrRequest;
import org.apache.solr.client.solrj.request.IsUpdateRequest;
import org.apache.solr.client.solrj.request.RequestWriter;
import org.apache.solr.client.solrj.response.UpdateResponse;
import org.apache.solr.common.params.SolrParams;

/**
 * Pure JSON request, based on {@link org.apache.solr.client.solrj.request.DirectXmlRequest}.
 * <p>
 * TODO: REVIEW: There must be a better way than this.
 */
@SuppressWarnings("serial")
public class DirectJsonRequest extends SolrRequest<UpdateResponse> implements IsUpdateRequest {

    public static final String TEXT_JSON = "application/json; charset=UTF-8";
    //public static final String TEXT_JSON = "application/json";

    private final String jsonBody;
    private SolrParams params;

    public DirectJsonRequest(String path, String body) {
        super(METHOD.POST, path);
        jsonBody = body;
    }

    public RequestWriter.ContentWriter getContentWriter(String expectedType) {
        return new RequestWriter.StringPayloadContentWriter(this.jsonBody, TEXT_JSON);
    }

    @Override
    protected UpdateResponse createResponse(SolrClient client) {
        return new UpdateResponse();
    }

    @Override
    public SolrParams getParams() {
        return params;
    }

    public void setParams(SolrParams params) {
        this.params = params;
    }

}