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
