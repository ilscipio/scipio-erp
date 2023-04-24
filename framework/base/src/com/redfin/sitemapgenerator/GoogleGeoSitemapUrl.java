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
