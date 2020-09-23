(defproject puppetlabs/trapperkeeper "3.0.0"
  :description "A framework for configuring, composing, and running Clojure services."

  :license {:name "Apache License, Version 2.0"
            :url "http://www.apache.org/licenses/LICENSE-2.0.html"}

  :min-lein-version "2.9.0"

  :parent-project {:coords [puppetlabs/clj-parent "4.0.1"]
                   :inherit [:managed-dependencies]}

  ;; Abort when version ranges or version conflicts are detected in
  ;; dependencies. Also supports :warn to simply emit warnings.
  ;; requires lein 2.2.0+.
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/tools.logging]
                 [org.clojure/tools.macro]
                 [org.clojure/core.async]

                 [com.nedap.staffing-solutions/speced.def "2.0.0"]

                 [org.slf4j/log4j-over-slf4j]
                 [ch.qos.logback/logback-classic]
                 ;; even though we don't strictly have a dependency on the following two
                 ;; logback artifacts, specifying the dependency version here ensures
                 ;; that downstream projects don't pick up different versions that would
                 ;; conflict with our version of logback-classic
                 [ch.qos.logback/logback-core]
                 [ch.qos.logback/logback-access]
                 ;; Janino can be used for some advanced logback configurations
                 [org.codehaus.janino/janino]

                 [clj-time]
                 [clj-commons/fs]
                 [clj-commons/clj-yaml]

                 [prismatic/plumbing]
                 [prismatic/schema]

                 [beckon]

                 [puppetlabs/typesafe-config]
                 [puppetlabs/kitchensink]
                 [puppetlabs/i18n]
                 [nrepl/nrepl]
                 ]

  :deploy-repositories [["releases" {:url "https://clojars.org/repo"
                                     :username :env/clojars_jenkins_username
                                     :password :env/clojars_jenkins_password
                                     :sign-releases false}]]

  ;; Convenience for manually testing application shutdown support - run `lein test-external-shutdown`
  :aliases {"cljfmt" ["with-profile" "+cljfmt" "cljfmt"]
            "test-external-shutdown" ["trampoline" "run" "-m" "examples.shutdown-app.test-external-shutdown"]}

  ;; By declaring a classifier here and a corresponding profile below we'll get an additional jar
  ;; during `lein jar` that has all the code in the test/ directory. Downstream projects can then
  ;; depend on this test jar using a :classifier in their :dependencies to reuse the test utility
  ;; code that we have.
  :classifiers [["test" :testutils]]

  :profiles {:cljfmt {:plugins [[lein-cljfmt "0.5.0"]]
                      :parent-project {:path "ext/pl-clojure-style/project.clj"
                                       :inherit [:cljfmt]}}
             :dev {:source-paths ["examples/shutdown_app/src"
                                  "examples/java_service/src/clj"]
                   :java-source-paths ["examples/java_service/src/java"]
                   :dependencies [[puppetlabs/kitchensink :classifier "test"]]}

             :testutils {:source-paths ^:replace ["test"]}
             :uberjar {:aot [puppetlabs.trapperkeeper.main]
                       :classifiers ^:replace []}}

  :plugins [[lein-parent "0.3.7"]
            [puppetlabs/i18n "0.8.0"]]

  :main puppetlabs.trapperkeeper.main
  )
