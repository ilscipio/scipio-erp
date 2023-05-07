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

package com.redfin.sitemapgenerator;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Date;
import java.util.List;

/**
 * Encapsulates a single URL to be inserted into a Web sitemap (as opposed to a Geo sitemap, a Mobile sitemap, a Video sitemap, etc which are Google specific).
 * Specifying a lastMod, changeFreq, or priority is optional; you specify those by using an Options object.
 * 
 * @see Options
 * @author Dan Fabulich
 *
 */
public class WebSitemapUrl implements ISitemapUrl {
	private final URL url;
	private final Date lastMod;
	private final ChangeFreq changeFreq;
	private final Double priority;
	private final List<AltLink> altLinks; // SCIPIO: 3.0.0: Added
	
	/** Encapsulates a single simple URL */
	public WebSitemapUrl(String url) throws MalformedURLException {
		this(new URL(url));
	}
	
	/** Encapsulates a single simple URL */
	public WebSitemapUrl(URL url) {
		this.url = url;
		this.lastMod = null;
		this.changeFreq = null;
		this.priority = null;
		this.altLinks = null;
	}
	
	/** Creates an URL with configured options */
	public WebSitemapUrl(Options options) {
		this((AbstractSitemapUrlOptions<?,?>)options);
	}
	
	WebSitemapUrl(AbstractSitemapUrlOptions<?,?> options) {
		this.url = options.url;
		this.lastMod = options.lastMod;
		this.changeFreq = options.changeFreq;
		this.priority = options.priority;
		this.altLinks = options.altLinks;
	}
	
	/** Retrieves the {@link Options#lastMod(Date)} */
	public Date getLastMod() { return lastMod; }
	/** Retrieves the {@link Options#changeFreq(ChangeFreq)} */
	public ChangeFreq getChangeFreq() { return changeFreq; }
	/** Retrieves the {@link Options#priority(Double)} */
	public Double getPriority() { return priority; }
	/** Retrieves the url */
	public URL getUrl() { return url; }

	/**
	 * Retrieves the alt links (xhtml:link).
	 *
	 * <p>SCIPIO: 3.0.0: Added.</p>
	 */
	public List<AltLink> getAltLinks() {
		return altLinks;
	}

	/** Options to configure web sitemap URLs */
	public static class Options extends AbstractSitemapUrlOptions<WebSitemapUrl, Options> {

		/** Configure this URL */
		public Options(String url)throws MalformedURLException {
			this(new URL(url));
		}

		/** Configure this URL */
		public Options(URL url) {
			super(url, WebSitemapUrl.class);
		}
		
	}
}
