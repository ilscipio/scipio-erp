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
var gulp        = require('gulp');
var browserSync = require('browser-sync').create();
var sass        = require('gulp-sass')(require('node-sass'));
var del         = require('del');
var replace     = require('gulp-replace');
var rename      = require('gulp-rename');
var concat        =  require('gulp-concat');
let uglify      = require('gulp-uglify-es').default;
var csso        =  require('gulp-csso');

paths = {
    dist: 'dist',
    src: 'src',
    node: 'node_modules'
};

gulp.task('sass', function () {
    return gulp.src(paths.src+'/scss/style.scss')
        .pipe(sass({noCache : true,
            style   : "compact"}))
        .pipe(rename('main.css'))
        .pipe(gulp.dest(paths.dist+'/css'))
        .pipe(browserSync.stream());
});

gulp.task('clean:dist', function () {
    return del(paths.dist);
});

gulp.task("clean:css", function () {
    return del(paths.dist+'/css');
});

gulp.task("clean:js", function () {
    return del(paths.dist+'/js');
});

gulp.task('minify:css', function(){
    return gulp.src([paths.dist+'/css/main.css'
    ])
        .pipe(csso())
        .pipe(rename({
            suffix: '.min'
        }))
        .pipe(gulp.dest(paths.dist+'/css'));
});

gulp.task('compile:js', function(){
    return gulp.src([paths.src+'/js/**.js',
        paths.node+'/chart.js/dist/chart.umd.js',
        paths.node+'/bulma-carousel/dist/js/bulma-carousel.min.js',
        paths.node+'/bulma-calendar/dist/js/bulma-calendar.min.js'])
        .pipe(concat('main.js'))
        .pipe(gulp.dest(paths.dist+'/js'));
});

gulp.task('minify:js', function(){
    return gulp.src([paths.dist+'/js/main.js'])
        .pipe(uglify())
        .pipe(rename({
            suffix: '.min'
        }))
        .pipe(gulp.dest(paths.dist+'/js'));
});

gulp.task('copy:js', function() {
    return gulp.src([paths.dist+'/js/**/*.min.js'
    ])
        .pipe(gulp.dest('./js'));
});

gulp.task('copy:css', function() {
    return gulp.src(paths.dist+'/css/**/*.min.css')
        .pipe(gulp.dest('./css'));
});


gulp.task('build:dist', gulp.series('clean:css', 'sass', 'minify:css','clean:js','compile:js','minify:js','copy:js', 'copy:css'));

//Synch with Scipios Layout demo page.
gulp.task('serve:scipio', function() {

    browserSync.init({
        proxy: {
            target: "https://localhost:8443/admin/layoutdemo"
        }
    });

    gulp.watch(paths.src+'/scss/**/*.scss', gulp.series('clean:css', 'sass','minify:css','copy:css'));
    gulp.watch(paths.src+'/js/**/*.js', gulp.series('clean:js','compile:js','minify:js','copy:js'));
    gulp.watch('css/*.css').on('change', browserSync.reload);
    gulp.watch('js/**/*.js').on('change', browserSync.reload);

});

gulp.task('dev', gulp.series('build:dist','serve:scipio'));