'use strict'

var gulp = require('gulp');
var browserSync = require('browser-sync').create();
var sass = require('gulp-sass');
var concat = require('gulp-concat');
var filter = require('gulp-filter');
var uglify = require('gulp-uglify');
var rename = require('gulp-rename');
var del = require('del');
var runSequence = require('run-sequence');
var replace = require('gulp-replace');
var npmDist = require('gulp-npm-dist');

// required for recompiling coreui
var babel = require('gulp-babel');
var babelify = require('babelify');
var browserify = require('browserify');
var source = require('vinyl-source-stream');
var buffer = require('vinyl-buffer');
var sourcemaps = require('gulp-sourcemaps');
var glob = require('node-glob');
var transform = require('vinyl-transform');





gulp.paths = {
    dist: 'dist',
};

var paths = gulp.paths;

// Static Server + watching scss/html files
gulp.task('serve', ['sass'], function() {

    browserSync.init({
        server: "./"
    });

    gulp.watch('scss/**/*.scss', ['sass']);
    gulp.watch('**/*.html').on('change', browserSync.reload);
    gulp.watch('js/**/*.js').on('change', browserSync.reload);

});

// Snych with Scipios Layout demo page.
gulp.task('serve:scipio', ['sass'], function() {

    browserSync.init({
    	proxy: {
    		target: "https://localhost:8443/admin/control/WebtoolsLayoutDemo"
    	},
    	files: './**'
    });

    gulp.watch('scss/**/*.scss', ['sass','copy:css']);
    gulp.watch('**/*.html').on('change', browserSync.reload);
    gulp.watch('js/**/*.js').on('change', browserSync.reload);

});

// Static Server without watching scss files
gulp.task('serve:lite', function() {

    browserSync.init({
        server: "./"
    });

    gulp.watch('**/*.css').on('change', browserSync.reload);
    gulp.watch('**/*.html').on('change', browserSync.reload);
    gulp.watch('js/**/*.js').on('change', browserSync.reload);

});

gulp.task('sass', function () {
    return gulp.src('./scss/style.scss')
        .pipe(sass({includePaths: ['node_modules']}))
        .pipe(gulp.dest(paths.dist+'/css'))
        .pipe(browserSync.stream());
});

gulp.task('sass:watch', function () {
    gulp.watch('./scss/**/*.scss');
});

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

gulp.task('copy:css', function() {
   gulp.src('./css/**/*')
   .pipe(gulp.dest(paths.dist+'/css'));
});

gulp.task('copy:img', function() {
   return gulp.src('./imgages/**/*')
   .pipe(gulp.dest(paths.dist+'/images'));
});

gulp.task('copy:fonts', function() {
   return gulp.src('./fonts/**/*')
   .pipe(gulp.dest(paths.dist+'/fonts'));
});

gulp.task('js:coreui', function() {
   browserify({
           entries: './js/coreui/index.js',
           debug: true
       })
       .transform(babelify.configure({
          presets: ['@babel/preset-env']
         }), { global: true })
       .bundle()
       .on('error', function (err) { console.error(err); })
       .pipe(source('coreui.js'))
       .pipe(buffer())
       .pipe(sourcemaps.init({ loadMaps: true }))
       .pipe(uglify())
       .pipe(rename({ extname: '.min.js' }))
       .pipe(sourcemaps.write('./'))
       .pipe(gulp.dest(paths.dist+'/js/'));

    browserify({
              entries: './js/coreui/utilities/index.js',
              debug: true
          })
          .transform(babelify.configure({
             presets: ['@babel/preset-env']
            }), { global: true })
          .bundle()
          .on('error', function (err) { console.error(err); })
          .pipe(source('coreui-utilities.js'))
          .pipe(buffer())
          .pipe(sourcemaps.init({ loadMaps: true }))
          .pipe(uglify())
          .pipe(rename({ extname: '.min.js' }))
          .pipe(sourcemaps.write('./'))
          .pipe(gulp.dest(paths.dist+'/js/'));
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

gulp.task('copy:html', function() {
   return gulp.src('./**/*.html')
   .pipe(gulp.dest(paths.dist+'/'));
});

gulp.task('build:dist', function(callback) {
    runSequence('clean:dist', 'copy:libs', 'sass', 'copy:css', 'copy:img', 'copy:fonts',  'js:coreui', 'copy:js', 'js:minify', 'copy:html', callback);
});

gulp.task('default', ['serve:scipio']);
