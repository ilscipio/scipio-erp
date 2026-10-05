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
 * Builds a sitemap for Google Video search.  To configure options, use {@link #builder(URL, File)}
 * @author Dan Fabulich
 * @see <a href="http://www.google.com/support/webmasters/bin/answer.py?answer=80472">Creating Video Sitemaps</a>
 */
public class GoogleVideoSitemapGenerator extends SitemapGenerator<GoogleVideoSitemapUrl,GoogleVideoSitemapGenerator> {

	/** Configures a builder so you can specify sitemap generator options
	 * 
	 * @param baseUrl All URLs in the generated sitemap(s) should appear under this base URL
	 * @param baseDir Sitemap files will be generated in this directory as either "sitemap.xml" or "sitemap1.xml" "sitemap2.xml" and so on.
	 * @return a builder; call .build() on it to make a sitemap generator
	 */
	public static SitemapGeneratorBuilder<GoogleVideoSitemapGenerator> builder(URL baseUrl, File baseDir) {
		return new SitemapGeneratorBuilder<GoogleVideoSitemapGenerator>(baseUrl, baseDir, GoogleVideoSitemapGenerator.class);
	}
	
	/** Configures a builder so you can specify sitemap generator options
	 * 
	 * @param baseUrl All URLs in the generated sitemap(s) should appear under this base URL
	 * @param baseDir Sitemap files will be generated in this directory as either "sitemap.xml" or "sitemap1.xml" "sitemap2.xml" and so on.
	 * @return a builder; call .build() on it to make a sitemap generator
	 */
	public static SitemapGeneratorBuilder<GoogleVideoSitemapGenerator> builder(String baseUrl, File baseDir) throws MalformedURLException {
		return new SitemapGeneratorBuilder<GoogleVideoSitemapGenerator>(baseUrl, baseDir, GoogleVideoSitemapGenerator.class);
	}

	GoogleVideoSitemapGenerator(AbstractSitemapGeneratorOptions<?> options) {
		super(options, new Renderer());
	}
	
	/**Configures the generator with a base URL and directory to write the sitemap files.
	 * 
	 * @param baseUrl All URLs in the generated sitemap(s) should appear under this base URL
	 * @param baseDir Sitemap files will be generated in this directory as either "sitemap.xml" or "sitemap1.xml" "sitemap2.xml" and so on.
	 * @throws MalformedURLException
	 */
	public GoogleVideoSitemapGenerator(String baseUrl, File baseDir)
			throws MalformedURLException {
		this(new SitemapGeneratorOptions(baseUrl, baseDir));
	}

	/**Configures the generator with a base URL and directory to write the sitemap files.
	 * 
	 * @param baseUrl All URLs in the generated sitemap(s) should appear under this base URL
	 * @param baseDir Sitemap files will be generated in this directory as either "sitemap.xml" or "sitemap1.xml" "sitemap2.xml" and so on.
	 */
	public GoogleVideoSitemapGenerator(URL baseUrl, File baseDir) {
		this(new SitemapGeneratorOptions(baseUrl, baseDir));
	}
	
	/**Configures the generator with a base URL and a null directory. The object constructed
	 * is not intended to be used to write to files. Rather, it is intended to be used to obtain
	 * XML-formatted strings that represent sitemaps.
	 * 
	 * @param baseUrl All URLs in the generated sitemap(s) should appear under this base URL
	 */
	public GoogleVideoSitemapGenerator(String baseUrl) throws MalformedURLException {
		this(new SitemapGeneratorOptions(new URL(baseUrl)));
	}
	
	/**Configures the generator with a base URL and a null directory. The object constructed
	 * is not intended to be used to write to files. Rather, it is intended to be used to obtain
	 * XML-formatted strings that represent sitemaps.
	 * 
	 * @param baseUrl All URLs in the generated sitemap(s) should appear under this base URL
	 */
	public GoogleVideoSitemapGenerator(URL baseUrl) {
		this(new SitemapGeneratorOptions(baseUrl));
	}

	private static class Renderer extends AbstractSitemapUrlRenderer<GoogleVideoSitemapUrl> implements ISitemapUrlRenderer<GoogleVideoSitemapUrl> {

		public Class<GoogleVideoSitemapUrl> getUrlClass() {
			return GoogleVideoSitemapUrl.class;
		}
		
		public String getXmlNamespaces() {
			return "xmlns:video=\"http://www.google.com/schemas/sitemap-video/1.1\"";
		}

		public void render(GoogleVideoSitemapUrl url, StringBuilder sb, W3CDateFormat dateFormat) {
			StringBuilder tagSb = new StringBuilder();
			tagSb.append("    <video:video>\n");
			renderTag(tagSb, "video", "content_loc", url.getContentUrl());
			if (url.getPlayerUrl() != null) {
				tagSb.append("      <video:player_loc allow_embed=\"");
				tagSb.append(url.getAllowEmbed());
				tagSb.append("\">");
				tagSb.append(url.getPlayerUrl());
				tagSb.append("</video:player_loc>\n");
			}
			renderTag(tagSb, "video", "thumbnail_loc", url.getThumbnailUrl());
			renderTag(tagSb, "video", "title", url.getTitle());
			renderTag(tagSb, "video", "description", url.getDescription());
			renderTag(tagSb, "video", "rating", url.getRating());
			renderTag(tagSb, "video", "view_count", url.getViewCount());
			if (url.getPublicationDate() != null) {
				renderTag(tagSb, "video", "publication_date", dateFormat.format(url.getPublicationDate()));
			}
			if (url.getTags() != null) {
				for (String tag : url.getTags()) {
					renderTag(tagSb, "video", "tag", tag);
				}
			}
			renderTag(tagSb, "video", "category", url.getCategory());
			renderTag(tagSb, "video", "family_friendly", url.getFamilyFriendly());
			renderTag(tagSb, "video", "duration", url.getDurationInSeconds());
			tagSb.append("    </video:video>\n");
			super.render(url, sb, dateFormat, tagSb.toString());
		}
		
	}
	
}
