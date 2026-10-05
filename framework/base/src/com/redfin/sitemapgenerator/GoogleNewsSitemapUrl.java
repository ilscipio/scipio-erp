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
import java.util.Arrays;
import java.util.Date;

/**
 * One configurable Google News Search URL.  To configure, use {@link Options}
 * @author Dan Fabulich
 * @see Options
 * @see <a href="http://www.google.com/support/news_pub/bin/answer.py?answer=74288">Creating a News Sitemap</a>
 */
public class GoogleNewsSitemapUrl extends WebSitemapUrl {

	private final Date publicationDate;
	private final String keywords;
	private final String genres;
	private final String title;
	private final GoogleNewsPublication publication;
	
	/** Options to configure Google News URLs */
	public static class Options extends AbstractSitemapUrlOptions<GoogleNewsSitemapUrl, Options> {
		private Date publicationDate;
		private String keywords;
		private String genres;
		private String title;
		private GoogleNewsPublication publication;
	
		/** Specifies an URL and publication date (which is mandatory for Google News) */
		public Options(String url, Date publicationDate, String title, GoogleNewsPublication publication) throws MalformedURLException {
			this(new URL(url), publicationDate, title, publication);
		}
		
		public Options(String url, Date publicationDate, String title, String name, String language) throws MalformedURLException {
			this(new URL(url), publicationDate, title, new GoogleNewsPublication(name, language));
		}

		public Options(URL url, Date publicationDate, String title, String name, String language) {
			this(url, publicationDate, title, new GoogleNewsPublication(name, language));
		}

		/** Specifies an URL and publication date (which is mandatory for Google News) */
		public Options(URL url, Date publicationDate, String title, GoogleNewsPublication publication) {
			super(url, GoogleNewsSitemapUrl.class);
			if (publicationDate == null) throw new NullPointerException("publicationDate must not be null");
			this.publicationDate = publicationDate;
			if (title == null) throw new NullPointerException("title must not be null");
			this.title = title;
			if (publication == null) throw new NullPointerException("publication must not be null");
			if (publication.getName() == null) throw new NullPointerException("publication name must not be null");
			if (publication.getLanguage() == null) throw new NullPointerException("publication language must not be null");
			this.publication = publication;
		}
		
		/** Specifies a list of comma-delimited keywords */
		public Options keywords(String keywords) {
			this.keywords = keywords;
			return this;
		}
		
		/** Specifies a list of comma-delimited keywords */
		public Options keywords(Iterable<String> keywords) {
			this.keywords = getListAsCommaSeparatedString(keywords);
			return this;
		}

		public Options genres(String genres) {
			this.genres = genres;
			return this;
		}

		public Options genres(Iterable<String> genres) {
			this.genres = getListAsCommaSeparatedString(genres);
			return this;
		}

		private String getListAsCommaSeparatedString(Iterable<String> values) {
			StringBuilder sb = new StringBuilder();
			boolean first = true;
			for (String value : values) {
				if (first) {
					first = false;
				} else {
					sb.append(", ");
				}
				sb.append(value);
			}
			return sb.toString();
		}
		
		/** Specifies a list of comma-delimited keywords */
		public Options keywords(String... keywords) {
			return keywords(Arrays.asList(keywords));
		}
		
		public Options genres(String... genres) {
			return genres(Arrays.asList(genres));
		}
		
	}
	
	/** Specifies an URL and publication date, title and publication (which are mandatory for Google News) */
	public GoogleNewsSitemapUrl(URL url, Date publicationDate, String title, String name, String language) {
		this(new Options(url, publicationDate, title, name, language));
	}
	
	/** Specifies an URL and publication date, title and publication (which are mandatory for Google News) */
	public GoogleNewsSitemapUrl(URL url, Date publicationDate, String title, GoogleNewsPublication publication) {
		this(new Options(url, publicationDate, title, publication));
	}

	/** Specifies an URL and publication date, title and publication (which are mandatory for Google News) */
	public GoogleNewsSitemapUrl(String url, Date publicationDate, String title, String name, String language) throws MalformedURLException {
		this(new Options(url, publicationDate, title, name, language));
	}

	/** Specifies an URL and publication date, title and publication (which are mandatory for Google News) */
	public GoogleNewsSitemapUrl(String url, Date publicationDate, String title, GoogleNewsPublication publication) throws MalformedURLException {
		this(new Options(url, publicationDate, title, publication));
	}

	/** Configures an URL with options */
	public GoogleNewsSitemapUrl(Options options) {
		super(options);
		publicationDate = options.publicationDate;
		keywords = options.keywords;
		genres = options.genres;
		title = options.title;
		publication = options.publication;
	}

	/** Retrieves the publication date */
	public Date getPublicationDate() {
		return publicationDate;
	}

	/** Retrieves the list of comma-delimited keywords */
	public String getKeywords() {
		return keywords;
	}

	/**
	 * Retrieves the Genres
	 */
	public String getGenres() {
		return genres;
	}

	/**
	 * Retrieves the title
	 */
	public String getTitle() {
		return title;
	}

	/**
	 * Retrieves the publication with name and language
	 */
	public GoogleNewsPublication getPublication() {
		return publication;
	}


}
