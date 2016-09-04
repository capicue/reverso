var del = require('del');
var elm = require('gulp-elm');
var gulp = require('gulp');
var plumber = require('gulp-plumber');

var config = {
    elm: {
        src: "elm/**/*.elm",
        dest: "build"
    }
}

gulp.task('clean', function() {
    del([config.elm.dest]);
});

gulp.task('elm-init', function() {
    elm.init;
});

gulp.task('elm', ['elm-init'], function() {
    return gulp.src(config.elm.src)
        .pipe(plumber())
        .pipe(elm.make({filetype: "html", warn: true}))
        .pipe(gulp.dest(config.elm.dest));
});

gulp.task('watch', function() {
    gulp.watch(config.elm.src, ['elm']);
});

gulp.task('build', ['clean', 'elm'], function() {
});

gulp.task('default', ['build', 'watch']);
