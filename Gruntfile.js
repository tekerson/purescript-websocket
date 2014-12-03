module.exports = function(grunt) {
  "use strict";
  grunt.initConfig({

    pscMake: {
      options: {
      },
      main: {
        files: {
          src: ["src/**/*.purs", "bower_components/**/src/**/*.purs"]
        }
      },
    },

    pscDocs: {
      readme: {
        src: "src/**/*.purs",
        dest: "README.md"
      }
    }
  });

  grunt.loadNpmTasks("grunt-purescript");
  grunt.registerTask("default", ["pscMake:main", "pscDocs:readme"]);
};
