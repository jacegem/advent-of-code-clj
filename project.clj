(defproject advent-of-code-clj "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.clojure/test.check "1.1.0"]]
  ;; https://blog.taylorwood.io/2017/10/15/fspec.html
  ;; :injections [(require 'lib.core) ;; all instrumented fns should be loaded here
  ;;              (require 'clojure.spec.test.alpha)
  ;;              (clojure.spec.test.alpha/instrument)]
  :main ^:skip-aot advent-of-code-clj.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
