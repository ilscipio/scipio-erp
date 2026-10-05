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

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;

/** Builds a Google Geo Sitemap (which points to KML or GeoRSS files).  At this time, SitemapGen4j can't
 * generate either KML or GeoRSS (sorry).
 * 
 * @author Dan Fabulich
 * @see <a href="http://www.google.com/support/webmasters/bin/answer.py?answer=94555">Creating Geo Sitemaps</a>
 */
public class GoogleGeoSitemapGenerator extends SitemapGenerator<GoogleGeoSitemapUrl,GoogleGeoSitemapGenerator> {

	/** Configures a builder so you can specify sitemap generator options
	 * 
	 * @param baseUrl All URLs in the generated sitemap(s) should appear under this base URL
	 * @param baseDir Sitemap files will be generated in this directory as either "sitemap.xml" or "sitemap1.xml" "sitemap2.xml" and so on.
	 * @return a builder; call .build() on it to make a sitemap generator
	 */
	public static SitemapGeneratorBuilder<GoogleGeoSitemapGenerator> builder(URL baseUrl, File baseDir) {
		return new SitemapGeneratorBuilder<GoogleGeoSitemapGenerator>(baseUrl, baseDir, GoogleGeoSitemapGenerator.class);
	}
	
	/** Configures a builder so you can specify sitemap generator options
	 * 
	 * @param baseUrl All URLs in the generated sitemap(s) should appear under this base URL
	 * @param baseDir Sitemap files will be generated in this directory as either "sitemap.xml" or "sitemap1.xml" "sitemap2.xml" and so on.
	 * @return a builder; call .build() on it to make a sitemap generator
	 */
	public static SitemapGeneratorBuilder<GoogleGeoSitemapGenerator> builder(String baseUrl, File baseDir) throws MalformedURLException {
		return new SitemapGeneratorBuilder<GoogleGeoSitemapGenerator>(baseUrl, baseDir, GoogleGeoSitemapGenerator.class);
	}
	
	GoogleGeoSitemapGenerator(AbstractSitemapGeneratorOptions<?> options) {
		super(options, new Renderer());
	}

	/**Configures the generator with a base URL and directory to write the sitemap files.
	 * 
	 * @param baseUrl All URLs in the generated sitemap(s) should appear under this base URL
	 * @param baseDir Sitemap files will be generated in this directory as either "sitemap.xml" or "sitemap1.xml" "sitemap2.xml" and so on.
	 * @throws MalformedURLException
	 */
	public GoogleGeoSitemapGenerator(String baseUrl, File baseDir)
			throws MalformedURLException {
		this(new SitemapGeneratorOptions(baseUrl, baseDir));
	}

	/**Configures the generator with a base URL and directory to write the sitemap files.
	 * 
	 * @param baseUrl All URLs in the generated sitemap(s) should appear under this base URL
	 * @param baseDir Sitemap files will be generated in this directory as either "sitemap.xml" or "sitemap1.xml" "sitemap2.xml" and so on.
	 */
	public GoogleGeoSitemapGenerator(URL baseUrl, File baseDir) {
		this(new SitemapGeneratorOptions(baseUrl, baseDir));
	}
	
	/**Configures the generator with a base URL and a null directory. The object constructed
	 * is not intended to be used to write to files. Rather, it is intended to be used to obtain
	 * XML-formatted strings that represent sitemaps.
	 * 
	 * @param baseUrl All URLs in the generated sitemap(s) should appear under this base URL
	 */
	public GoogleGeoSitemapGenerator(String baseUrl) throws MalformedURLException {
		this(new SitemapGeneratorOptions(new URL(baseUrl)));
	}
	
	
	/**Configures the generator with a base URL and a null directory. The object constructed
	 * is not intended to be used to write to files. Rather, it is intended to be used to obtain
	 * XML-formatted strings that represent sitemaps.
	 * 
	 * @param baseUrl All URLs in the generated sitemap(s) should appear under this base URL
	 */
	public GoogleGeoSitemapGenerator(URL baseUrl) {
		this(new SitemapGeneratorOptions(baseUrl));
	}

	private static class Renderer extends AbstractSitemapUrlRenderer<GoogleGeoSitemapUrl> implements ISitemapUrlRenderer<GoogleGeoSitemapUrl> {

		public Class<GoogleGeoSitemapUrl> getUrlClass() {
			return GoogleGeoSitemapUrl.class;
		}

		public String getXmlNamespaces() {
			return "xmlns:geo=\"http://www.google.com/geo/schemas/sitemap/1.0\"";
		}

		public void render(GoogleGeoSitemapUrl url, StringBuilder sb, W3CDateFormat dateFormat) {
			StringBuilder tagSb = new StringBuilder();
			tagSb.append("    <geo:geo>\n");
			tagSb.append("      <geo:format>"+url.getFormat()+"</geo:format>\n");
			tagSb.append("    </geo:geo>\n");
			super.render(url, sb, dateFormat, tagSb.toString());
		}
		
	}
}
