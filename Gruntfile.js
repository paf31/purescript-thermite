module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({

    libFiles: [
      "src/**/*.purs",
      "bower_components/purescript-*/src/**/*.purs"
    ],

    clean: ["tmp", "output"],

    pscDocs: {
      lib: {
        src: [
          "src/Thermite.purs",
          "src/Thermite/Types.purs",
          "src/Thermite/Action.purs",
          "src/Thermite/Html.purs",
          "src/Thermite/Html/Attributes.purs",
          "src/Thermite/Html/Elements.purs",
          "src/Thermite/Events.purs"
        ],
        dest: "docs/README.md"
      }
    },

    pscMake: {
      lib: {
        src: ["<%=libFiles%>"]
      }
    },

    psc: {
      options: {
        main: "Main",
        modules: ["Main"]
      },
      example: {
        dest: "example/index.js",
        src: ["example/Main.purs", "<%=libFiles%>"]
      }
    },

    dotPsci: ["<%=libFiles%>"]
  });

  grunt.loadNpmTasks("grunt-contrib-clean");
  grunt.loadNpmTasks("grunt-purescript");

  grunt.registerTask("make", ["pscMake:lib", "pscDocs", "dotPsci"]);
  grunt.registerTask("default", ["clean", "make", "psc:example"]);
};
