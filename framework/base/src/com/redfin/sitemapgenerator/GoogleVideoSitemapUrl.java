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

import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;

/** One configurable Google Video Search URL.  To configure, use {@link Options}
 * 
 * @author Dan Fabulich
 * @see Options
 * @see <a href="http://www.google.com/support/webmasters/bin/answer.py?answer=80472">Creating Video Sitemaps</a>
 */
public class GoogleVideoSitemapUrl extends WebSitemapUrl {

	private final URL playerUrl;
	private final URL contentUrl;
	private final URL thumbnailUrl;
	private final String title;
	private final String description;
	private final Double rating;
	private final Integer viewCount;
	private final Date publicationDate;
	private final ArrayList<String> tags;
	private final String category;
	// TODO can there be multiple categories?
	// "Usually a video will belong to a single category."
	// http://www.google.com/support/webmasters/bin/answer.py?answer=80472
	private final String familyFriendly;
	private final Integer durationInSeconds;
	private final String allowEmbed;
	
	/** Options to configure Google Video URLs */
	public static class Options extends AbstractSitemapUrlOptions<GoogleVideoSitemapUrl, Options> {
		private URL playerUrl;
		private URL contentUrl;
		private URL thumbnailUrl;
		private String title;
		private String description;
		private Double rating;
		private Integer viewCount;
		private Date publicationDate;
		private ArrayList<String> tags;
		private String category;
		// TODO can there be multiple categories?
		// "Usually a video will belong to a single category."
		// http://www.google.com/support/webmasters/bin/answer.py?answer=80472
		private Boolean familyFriendly;
		private Integer durationInSeconds;
		private Boolean allowEmbed;
	
		/** Specifies a landing page URL, together with a "player" (e.g. SWF)
		 *  
		 * @param url the landing page URL
		 * @param playerUrl the URL of the "player" (e.g. SWF file)
		 * @param allowEmbed when specifying a player, you must specify whether embedding is allowed
		 */
		public Options(URL url, URL playerUrl, boolean allowEmbed) {
			super(url, GoogleVideoSitemapUrl.class);
			this.playerUrl = playerUrl;
			this.allowEmbed = allowEmbed;
		}
		
		/** Specifies a landing page URL, together with the URL of the underlying video (e.g. FLV)
		 * 
		 * @param url the landing page URL
		 * @param contentUrl the URL of the underlying video (e.g. FLV)
		 */
		public Options(URL url, URL contentUrl) {
			super(url, GoogleVideoSitemapUrl.class);
			this.contentUrl = contentUrl;
		}
		
		/** Specifies a player URL (e.g. SWF)
		 *  
		 * @param playerUrl the URL of the "player" (e.g. SWF file)
		 * @param allowEmbed when specifying a player, you must specify whether embedding is allowed
		 */
		public Options playerUrl(URL playerUrl, boolean allowEmbed) {
			this.playerUrl = playerUrl;
			this.allowEmbed = allowEmbed;
			return this;
		}
		
		/** Specifies the URL of the underlying video (e.g FLV) */
		public Options contentUrl(URL contentUrl) {
			this.contentUrl = contentUrl;
			return this;
		}
		
		/**
		 * A URL pointing to the URL for the video thumbnail image file. This
		 * allows you to suggest the thumbnail you want displayed in search
		 * results. If you provide a {@link #contentUrl(URL)}, Google will attempt
		 * to generate a set of representative thumbnail images from your actual
		 * video content. However, we strongly recommended that you provide a
		 * thumbnail URL to increase the likelihood of your video being included
		 * in the video index.
		 */
		public Options thumbnailUrl(URL thumbnailUrl) {
			this.thumbnailUrl = thumbnailUrl;
			return this;
		}
		
		/** The title of the video. Limited to 100 characters. */
		public Options title(String title) {
			if (title != null) {
				if (title.length() > 100) {
					throw new RuntimeException("Video title is limited to 100 characters: " + title);
				}
			}
			this.title = title;
			return this;
		}
		
		/** The description of the video. Descriptions longer than 2048 characters will be truncated. */
		public Options description(String description) {
			if (description != null) {
				if (description.length() > 2048) {
					throw new RuntimeException("Truncate video descriptions to 2048 characters: " + description);
				}
			}
			this.description = description;
			return this;
		}
		
		/** The rating of the video. The value must be number in the range 0.0-5.0. */
		public Options rating(Double rating) {
			if (rating != null) {
				if (rating < 0 || rating > 5.0) {
					throw new RuntimeException("Rating must be between 0.0 and 5.0:" + rating);
				}
			}
			this.rating = rating;
			return this;
		}
		
		/** The number of times the video has been viewed */
		public Options viewCount(int viewCount) {
			this.viewCount = viewCount;
			return this;
		}
		
		/** The date the video was first published, in {@link W3CDateFormat}. */
		public Options publicationDate(Date publicationDate) {
			this.publicationDate = publicationDate;
			return this;
		}
		
		/**
		 * Tag associated with the video; tags are generally very short
		 * descriptions of key concepts associated with a video or piece of
		 * content. A single video could have several tags, although it might
		 * belong to only one category. For example, a video about grilling food
		 * may belong in the Grilling category, but could be tagged "steak",
		 * "meat", "summer", and "outdoor". Create a new <video:tag> element for
		 * each tag associated with a video. A maximum of 32 tags is permitted.
		 */
		public Options tags(ArrayList<String> tags) {
			this.tags = tags;
			return this;
		}
		
		/**
		 * Tag associated with the video; tags are generally very short
		 * descriptions of key concepts associated with a video or piece of
		 * content. A single video could have several tags, although it might
		 * belong to only one category. For example, a video about grilling food
		 * may belong in the Grilling category, but could be tagged "steak",
		 * "meat", "summer", and "outdoor". Create a new <video:tag> element for
		 * each tag associated with a video. A maximum of 32 tags is permitted.
		 */
		public Options tags(Iterable<String> tags) {
			this.tags = new ArrayList<String>();
			for (String tag : tags) {
				this.tags.add(tag);
			}
			return this;
		}
		
		/**
		 * Tag associated with the video; tags are generally very short
		 * descriptions of key concepts associated with a video or piece of
		 * content. A single video could have several tags, although it might
		 * belong to only one category. For example, a video about grilling food
		 * may belong in the Grilling category, but could be tagged "steak",
		 * "meat", "summer", and "outdoor". Create a new <video:tag> element for
		 * each tag associated with a video. A maximum of 32 tags is permitted.
		 */
		public Options tags(String... tags) {
			return tags(Arrays.asList(tags));
		}
		
		/**
		 * The video's category; for example, <code>cooking</code>. The value
		 * should be a string no longer than 256 characters. In general,
		 * categories are broad groupings of content by subject. Usually a video
		 * will belong to a single category. For example, a site about cooking
		 * could have categories for Broiling, Baking, and Grilling
		 */
		public Options category(String category) {
			if (category != null) {
				if (category.length() > 256) {
					throw new RuntimeException("Video category is limited to 256 characters: " + title);
				}
			}
			this.category = category;
			return this;
		}
		
		/** Whether the video is suitable for viewing by children */
		public Options familyFriendly(boolean familyFriendly) {
			this.familyFriendly = familyFriendly;
			return this;
		}
		
		/** The duration of the video in seconds; value must be between 0 and 28800 (8 hours). */
		public Options durationInSeconds(int durationInSeconds) {
			if (durationInSeconds < 0 || durationInSeconds > 28800) {
				throw new RuntimeException("Duration must be between 0 and 28800 (8 hours):" + durationInSeconds);
			}
			this.durationInSeconds = durationInSeconds;
			return this;
		}
		
	}

	/** Specifies a landing page URL, together with a "player" (e.g. SWF)
	 *  
	 * @param url the landing page URL
	 * @param playerUrl the URL of the "player" (e.g. SWF file)
	 * @param allowEmbed when specifying a player, you must specify whether embedding is allowed
	 */
	public GoogleVideoSitemapUrl(URL url, URL playerUrl, boolean allowEmbed) {
		this(new Options(url, playerUrl, allowEmbed));
	}
	
	/** Specifies a landing page URL, together with the URL of the underlying video (e.g. FLV)
	 * 
	 * @param url the landing page URL
	 * @param contentUrl the URL of the underlying video (e.g. FLV)
	 */
	public GoogleVideoSitemapUrl(URL url, URL contentUrl) {
		this(new Options(url, contentUrl));
	}
	
	/** Configures the url with options */
	public GoogleVideoSitemapUrl(Options options) {
		super(options);
		contentUrl = options.contentUrl;
		playerUrl = options.playerUrl;
		if (playerUrl == null && contentUrl == null) {
			throw new RuntimeException("You must specify either contentUrl or playerUrl or both; neither were specified");
		}
		allowEmbed = convertBooleanToYesOrNo(options.allowEmbed);
		if (playerUrl != null && allowEmbed == null) {
			throw new RuntimeException("allowEmbed must be specified if playerUrl is specified");
		}
		category = options.category;
		
		description = options.description;
		durationInSeconds = options.durationInSeconds;
		familyFriendly = convertBooleanToYesOrNo(options.familyFriendly);
		
		publicationDate = options.publicationDate;
		rating = options.rating;
		tags = options.tags;
		if (tags != null && tags.size() > 32) {
			throw new RuntimeException("A maximum of 32 tags is permitted");
		}
		thumbnailUrl = options.thumbnailUrl;
		title = options.title;
		viewCount = options.viewCount;
	}
	
	private static String convertBooleanToYesOrNo(Boolean value) {
		if (value == null) return null;
		return value ? "Yes" : "No";
	}
	
	
	/** Retrieves the {@link Options#playerUrl}*/
	public URL getPlayerUrl() {
		return playerUrl;
	}
	
	/** Retrieves the {@link Options#contentUrl}*/
	public URL getContentUrl() {
		return contentUrl;
	}

	/** Retrieves the {@link Options#thumbnailUrl}*/
	public URL getThumbnailUrl() {
		return thumbnailUrl;
	}

	/** Retrieves the {@link Options#title}*/
	public String getTitle() {
		return title;
	}

	/** Retrieves the {@link Options#description}*/
	public String getDescription() {
		return description;
	}

	/** Retrieves the {@link Options#rating}*/
	public Double getRating() {
		return rating;
	}

	/** Retrieves the {@link Options#viewCount}*/
	public Integer getViewCount() {
		return viewCount;
	}

	/** Retrieves the {@link Options#publicationDate}*/
	public Date getPublicationDate() {
		return publicationDate;
	}

	/** Retrieves the {@link Options#tags}*/
	public ArrayList<String> getTags() {
		return tags;
	}

	/** Retrieves the {@link Options#category}*/
	public String getCategory() {
		return category;
	}

	/** Retrieves whether the video is {@link Options#familyFriendly}*/
	public String getFamilyFriendly() {
		return familyFriendly;
	}

	/** Retrieves the {@link Options#durationInSeconds}*/
	public Integer getDurationInSeconds() {
		return durationInSeconds;
	}

	/** Retrieves whether embedding is allowed */
	public String getAllowEmbed() {
		return allowEmbed;
	}





}
