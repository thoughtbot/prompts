import autoprefixer from "autoprefixer";
import browserSync from "browser-sync";
import spawn from "cross-spawn";
import cssnano from "cssnano";
import { dest, series, src, task, watch } from "gulp";
import postcss from "gulp-postcss";
import precss from 'precss';
import atimport from "postcss-import";
import tailwindcss from "tailwindcss";

const SITE_ROOT = "./public";
const POST_BUILD_STYLESHEET = `./src/`;
const PRE_BUILD_STYLESHEET = "./src/styles/main.css";
const TAILWIND_CONFIG = "./tailwind.config.js";

const isDevelopmentBuild = process.env.NODE_ENV === "development";

task("processStyles", () => {
  browserSync.notify("Compiling styles...");

  return src(PRE_BUILD_STYLESHEET)
    .pipe(
      postcss([
        atimport(),
        precss(),
        tailwindcss(TAILWIND_CONFIG),
        ...(isDevelopmentBuild ? [] : [autoprefixer(), cssnano()]),
      ])
    )
    .pipe(dest(POST_BUILD_STYLESHEET));
});

const buildSite = series("processStyles");

exports.serve = series(buildSite);
exports.default = series(buildSite);
