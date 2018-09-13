/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.ilscipio.scipio.solr.util;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.nio.charset.StandardCharsets;

import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpRequestBase;
import org.apache.http.message.BasicHeader;
import org.apache.solr.client.solrj.ResponseParser;
import org.apache.solr.client.solrj.SolrRequest;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.BinaryResponseParser;
import org.apache.solr.client.solrj.impl.HttpClientUtil;
import org.apache.solr.client.solrj.impl.HttpSolrClient;
import org.apache.solr.common.params.ModifiableSolrParams;
import org.apache.solr.common.util.Base64;
import org.apache.solr.common.util.NamedList;

import com.ilscipio.scipio.solr.SolrUtil;

/**
 * Special HttpSolrClient implementation that adds missing support for basic auth in the client.
 * <p>
 * Without this, we would be forced to rewrite all queries because basic auth can only be set
 * in SolrRequest, which is not supported through the HttpSolrClient.commit/add/deleteByQuery/etc methods.
 * <p>
 * NOTE: Client code should never use this class directly! Use the factory methods
 * in SolrUtil, {@link SolrUtil#getQueryHttpSolrClient} and {@link SolrUtil#getUpdateHttpSolrClient}.
 * <p>
 * DEV NOTE: We must keep this up-to-date with the {@link HttpSolrClient} implementation
 * on Solrj version updates, because due to class design, there are small parts that had to be duplicated,
 * though quite manageable. The ugliest part is that we cannot reuse {@link HttpSolrClient.Builder} at all,
 * so we must hardcode the defaults (
 */
public class ScipioHttpSolrClient extends HttpSolrClient {

    private static final long serialVersionUID = -1505671185572707459L;

    protected final String solrUsername;
    protected final String solrPassword;
    
    protected ScipioHttpSolrClient(String baseURL, HttpClient httpClient, ResponseParser parser, boolean allowCompression,
            ModifiableSolrParams invariantParams, String solrUsername, String solrPassword) {
        super(baseURL, httpClient, parser, allowCompression, invariantParams);
        this.solrUsername = solrUsername;
        this.solrPassword = solrPassword;
    }
    
    /**
     * Creates a new client from URL and username/password, where all operations will use
     * the given auth.
     * <p>
     * DEV NOTE: Implementation must be maintained with the superclass; the default values
     * are taken from {@link HttpSolrClient.Builder} and are subject to change at solrj updates.
     */
    public static HttpSolrClient create(String baseURL, HttpClient httpClient, String solrUsername, String solrPassword, 
            Integer maxConnections, Integer maxConnectionsPerHost, Integer connectTimeout, Integer socketTimeout) {
        
        if (httpClient == null) {
            ModifiableSolrParams params = new ModifiableSolrParams();
            if (maxConnections != null) {
                params.set(HttpClientUtil.PROP_MAX_CONNECTIONS, maxConnections);
            }
            if (maxConnectionsPerHost != null) {
                params.set(HttpClientUtil.PROP_MAX_CONNECTIONS_PER_HOST, maxConnectionsPerHost);
            }
            params.set(HttpClientUtil.PROP_FOLLOW_REDIRECTS, true);
            httpClient = HttpClientUtil.createClient(params);
        }
 
        // DEV NOTE: the defaults must match what HttpSolrClient.Builder does! Must keep up to date!
        HttpSolrClient client = new ScipioHttpSolrClient(baseURL, httpClient, new BinaryResponseParser(), 
                false, new ModifiableSolrParams(), solrUsername, solrPassword);
        
        // TODO: In Solr 7, these are deprecated and moved to Builder/constructor 
        if (connectTimeout != null) {
            client.setConnectionTimeout(connectTimeout);
        }
        if (socketTimeout != null) {
            client.setSoTimeout(socketTimeout);
        }
        
        return client;
    }

    /** 
     * Executes request.
     * <p>
     * DEV NOTE: This is copied from superclass, 
     */
    @SuppressWarnings("rawtypes")
    @Override
    public NamedList<Object> request(final SolrRequest request, final ResponseParser processor, String collection)
            throws SolrServerException, IOException {
        HttpRequestBase method = createMethod(request, collection);
        setBasicAuthHeaderScipio(request, method);
        return executeMethod(method, processor);
    }

    /**
     * Sets basic auth header to either the one in the SolrRequest or the one stored in this client.
     * <p>
     * DEV NOTE: Derived from {@link HttpSolrClient#setBasicAuthHeader}, which is private in superclass.
     */
    @SuppressWarnings("rawtypes")
    protected void setBasicAuthHeaderScipio(SolrRequest request, HttpRequestBase method) throws UnsupportedEncodingException {
        if (request.getBasicAuthUser() != null && request.getBasicAuthPassword() != null) {
            setBasicAuthHeader(method, request.getBasicAuthUser(), request.getBasicAuthPassword());
        } else if (this.solrUsername != null && this.solrPassword != null) {
            setBasicAuthHeader(method, this.solrUsername, this.solrPassword);
        }
    }
    
    /**
     * Sets basic auth header to the given username and password.
     * <p>
     * DEV NOTE: Derived from {@link HttpSolrClient#setBasicAuthHeader}, which is private in superclass.
     */
    protected void setBasicAuthHeader(HttpRequestBase method, String username, String password) throws UnsupportedEncodingException {
        String userPass = username + ":" + password;
        String encoded = Base64.byteArrayToBase64(userPass.getBytes(StandardCharsets.UTF_8));
        method.setHeader(new BasicHeader("Authorization", "Basic " + encoded));
    }
    
}
