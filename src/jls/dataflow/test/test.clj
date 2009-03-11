;;  Copyright (c) Jeffrey Straszheim. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  test.clj
;;
;;
;;  A Library to Support a Dataflow Model of State - Tests
;;
;;  straszheimjeffrey (gmail)
;;  Created 11 March 2009


(ns jls.dataflow.test.test
  (:use clojure.contrib.test-is))


(def tests [:test-dataflow])

(defn test-name
  [test]
  (symbol (str "jls.dataflow.test." (name test))))

(doseq [test tests]
  (require :reload (test-name test)))

(apply run-tests (map test-name tests))



;; End of file
