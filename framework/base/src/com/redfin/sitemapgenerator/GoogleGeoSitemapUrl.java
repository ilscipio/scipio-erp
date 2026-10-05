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

/**
 * One configurable Geo URL, either KML or GeoRSS.  At this time, SitemapGen4j can't generate either
 * KML or GeoRSS (sorry).  To configure this class, use {@link Options}
 * @author Dan Fabulich
 * @see Options
 * @see <a href="http://www.google.com/support/webmasters/bin/answer.py?answer=94555">Creating Geo Sitemaps</a>
 */
public class GoogleGeoSitemapUrl extends WebSitemapUrl {

	/** The two Geo URL formats: KML and GeoRSS */
	public enum Format { KML, GEORSS;
		@Override
		public String toString() {
			return this.name().toLowerCase();
		};
	}
	private final Format format;
	
	
	/** Options to configure Geo URLs */
	public static class Options extends AbstractSitemapUrlOptions<GoogleGeoSitemapUrl, Options> {
		private Format format;

		/** Specifies a Geo URL and its format */
		public Options(String url, Format format) throws MalformedURLException {
			super(url, GoogleGeoSitemapUrl.class);
			this.format = format;
		}
		
		/** Specifies a Geo URL and its format */
		public Options(URL url, Format format) {
			super(url, GoogleGeoSitemapUrl.class);
			this.format = format;
		}
	}
	
	/** Specifies a Geo URL and its format */
	public GoogleGeoSitemapUrl(URL url, Format format) {
		this(new Options(url, format));
	}
	
	/** Specifies a Geo URL and its format */
	public GoogleGeoSitemapUrl(String url, Format format) throws MalformedURLException {
		this(new Options(url, format));
	}

	/** Configures the URL with {@link Options} */
	public GoogleGeoSitemapUrl(Options options) {
		super(options);
		format = options.format;
	}
	
	/** Retrieves the URL {@link Format} */
	public Format getFormat() {
		return format;
	}

}
