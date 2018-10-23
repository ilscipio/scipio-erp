package com.ilscipio.scipio.solr.util;

import java.util.Collection;

import org.apache.solr.client.solrj.SolrClient;
import org.apache.solr.client.solrj.SolrRequest;
import org.apache.solr.client.solrj.request.IsUpdateRequest;
import org.apache.solr.client.solrj.response.UpdateResponse;
import org.apache.solr.client.solrj.util.ClientUtils;
import org.apache.solr.common.params.SolrParams;
import org.apache.solr.common.util.ContentStream;

/**
 * Pure JSON request, based on {@link org.apache.solr.client.solrj.request.DirectXmlRequest}.
 * <p>
 * TODO: REVIEW: There must be a better way than this.
 */
@SuppressWarnings("serial")
public class DirectJsonRequest extends SolrRequest<UpdateResponse> implements IsUpdateRequest {

    // public static final String TEXT_JSON = "application/json; charset=UTF-8";
    public static final String TEXT_JSON = "application/json";

    final String jsonBody;
    private SolrParams params;

    public DirectJsonRequest(String path, String body) {
        super(METHOD.POST, path);
        jsonBody = body;
    }

    @Override
    public Collection<ContentStream> getContentStreams() {
        return ClientUtils.toContentStreams(jsonBody, TEXT_JSON);
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