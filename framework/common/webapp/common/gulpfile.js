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
'use strict'

var gulp = require('gulp');
//var browserSync = require('browser-sync').create();
var concat = require('gulp-concat');
var filter = require('gulp-filter');
var uglify = require('gulp-uglify');
var rename = require('gulp-rename');
var del = require('del');
var runSequence = require('run-sequence');
var replace = require('gulp-replace');
var npmDist = require('gulp-npm-dist');

// required for recompiling coreui
//var babel = require('gulp-babel');
//var babelify = require('babelify');
//var browserify = require('browserify');
//var source = require('vinyl-source-stream');
//var buffer = require('vinyl-buffer');
//var sourcemaps = require('gulp-sourcemaps');
//var glob = require('node-glob');
//var transform = require('vinyl-transform');

gulp.paths = {
    dist: 'dist',
};

var paths = gulp.paths;


gulp.task('clean:dist', function () {
    return del(paths.dist);
});

gulp.task('copy:libs', function() {
    gulp.src(npmDist(), {base:'./node_modules/'})
        .pipe(rename(function(path) {
            path.dirname = path.dirname.replace(/\/dist/, '').replace(/\\dist/, '');
        }))
        .pipe(gulp.dest(paths.dist+'/libs'));
});

gulp.task('copy:img', function() {
   return gulp.src('./imgages/**/*')
   .pipe(gulp.dest(paths.dist+'/images'));
});

gulp.task('copy:fonts', function() {
   return gulp.src('./fonts/**/*')
   .pipe(gulp.dest(paths.dist+'/fonts'));
});

gulp.task('js:minify', function() {
  gulp.src('./js/*.js')
    .pipe(concat('all.min.js'))
    .pipe(gulp.dest(paths.dist+'/js/'))
    .pipe(uglify())
    .pipe(gulp.dest(paths.dist+'/js/'))
});

gulp.task('copy:js', function() {
   return gulp.src('./js/*.js')
   .pipe(gulp.dest(paths.dist+'/js'));
});


gulp.task('build:dist', function(callback) {
    runSequence('clean:dist', 'copy:libs', 'copy:img', 'copy:fonts', 'copy:js', 'js:minify', callback);
});

