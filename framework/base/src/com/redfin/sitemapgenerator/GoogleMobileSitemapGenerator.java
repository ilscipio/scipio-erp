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

/**
 * Builds a Google Mobile Sitemap, consisting of only mobile-friendly content.  To configure options, use {@link #builder(URL, File)}
 * @author Dan Fabulich
 * @see <a href="http://www.google.com/support/webmasters/bin/answer.py?answer=34648">Creating Mobile Sitemaps</a>
 */
public class GoogleMobileSitemapGenerator extends SitemapGenerator<GoogleMobileSitemapUrl,GoogleMobileSitemapGenerator> {

	/** Configures a builder so you can specify sitemap generator options
	 * 
	 * @param baseUrl All URLs in the generated sitemap(s) should appear under this base URL
	 * @param baseDir Sitemap files will be generated in this directory as either "sitemap.xml" or "sitemap1.xml" "sitemap2.xml" and so on.
	 * @return a builder; call .build() on it to make a sitemap generator
	 */
	public static SitemapGeneratorBuilder<GoogleMobileSitemapGenerator> builder(URL baseUrl, File baseDir) {
		return new SitemapGeneratorBuilder<GoogleMobileSitemapGenerator>(baseUrl, baseDir, GoogleMobileSitemapGenerator.class);
	}
	
	/** Configures a builder so you can specify sitemap generator options
	 * 
	 * @param baseUrl All URLs in the generated sitemap(s) should appear under this base URL
	 * @param baseDir Sitemap files will be generated in this directory as either "sitemap.xml" or "sitemap1.xml" "sitemap2.xml" and so on.
	 * @return a builder; call .build() on it to make a sitemap generator
	 */
	public static SitemapGeneratorBuilder<GoogleMobileSitemapGenerator> builder(String baseUrl, File baseDir) throws MalformedURLException {
		return new SitemapGeneratorBuilder<GoogleMobileSitemapGenerator>(baseUrl, baseDir, GoogleMobileSitemapGenerator.class);
	}
	
	GoogleMobileSitemapGenerator(AbstractSitemapGeneratorOptions<?> options) {
		super(options, new Renderer());
	}

	/** Configures the generator with a base URL and directory to write the sitemap files.
	 * 
	 * @param baseUrl All URLs in the generated sitemap(s) should appear under this base URL
	 * @param baseDir Sitemap files will be generated in this directory as either "sitemap.xml" or "sitemap1.xml" "sitemap2.xml" and so on.
	 * @throws MalformedURLException
	 */
	public GoogleMobileSitemapGenerator(String baseUrl, File baseDir)
			throws MalformedURLException {
		this(new SitemapGeneratorOptions(baseUrl, baseDir));
	}

	/** Configures the generator with a base URL and directory to write the sitemap files.
	 * 
	 * @param baseUrl All URLs in the generated sitemap(s) should appear under this base URL
	 * @param baseDir Sitemap files will be generated in this directory as either "sitemap.xml" or "sitemap1.xml" "sitemap2.xml" and so on.
	 */
	public GoogleMobileSitemapGenerator(URL baseUrl, File baseDir) {
		this(new SitemapGeneratorOptions(baseUrl, baseDir));
	}
	
	/**Configures the generator with a base URL and a null directory. The object constructed
	 * is not intended to be used to write to files. Rather, it is intended to be used to obtain
	 * XML-formatted strings that represent sitemaps.
	 * 
	 * @param baseUrl All URLs in the generated sitemap(s) should appear under this base URL
	 */
	public GoogleMobileSitemapGenerator(String baseUrl) throws MalformedURLException {
		this(new SitemapGeneratorOptions(new URL(baseUrl)));
	}
	
	/**Configures the generator with a base URL and a null directory. The object constructed
	 * is not intended to be used to write to files. Rather, it is intended to be used to obtain
	 * XML-formatted strings that represent sitemaps.
	 * 
	 * @param baseUrl All URLs in the generated sitemap(s) should appear under this base URL
	 */
	public GoogleMobileSitemapGenerator(URL baseUrl) {
		this(new SitemapGeneratorOptions(baseUrl));
	}

	private static class Renderer extends AbstractSitemapUrlRenderer<GoogleMobileSitemapUrl> implements ISitemapUrlRenderer<GoogleMobileSitemapUrl> {

		public Class<GoogleMobileSitemapUrl> getUrlClass() {
			return GoogleMobileSitemapUrl.class;
		}

		public String getXmlNamespaces() {
			return "xmlns:mobile=\"http://www.google.com/schemas/sitemap-mobile/1.0\"";
		}

		public void render(GoogleMobileSitemapUrl url, StringBuilder sb, W3CDateFormat dateFormat) {
			String additionalData = "    <mobile:mobile/>\n";
			super.render(url, sb, dateFormat, additionalData);
		}
		
	}
}
