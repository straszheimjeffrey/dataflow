;;  Copyright (c) Jeffrey Straszheim. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  dataflow.clj
;;
;;  A Library to Support a Dataflow Model of State
;;
;;  straszheimjeffrey (gmail)
;;  Created 10 March 2009


(ns clojure.contrib.dataflow)


;;; Chief Data Structures


;; Source Cell

; The data of a source cell is directly set by a calling function.  It
; never depends on other cells.

(defstruct source-cell
  :name             ; The name, a symbol
  :value            ; Its value, a Ref
  :cell-type)       ; Should be ::source-cell

;; Cell

; A standard cell that computes its value from other cells.

(defstruct cell
  :name            ; The name, a symbol
  :value           ; Its value, a Ref
  :dependents      ; The names of cells on which this depends
  :fun             ; A closure that computes the value, given an environment
  :cell-type)      ; Should be ::cell

(derive ::cell ::dependent-cell) ; A cell that has a dependents field

;; An sentinal value

(def *empty-value* (java.lang.Object.))


;; Dataflow

; A collection of cells and dependency information

(defstruct dataflow
  :cells          ; A map of cell names (symbols) to collection of cells
  :back-graph     ; A graph of dependencies
  :fore-graph)    ; The inverse of back-graph, shows dataflow


;;; Environment Access

(defn get-one-value
  "Gets a value from the cells-map (env) matching the passed symbol.
   Signals an error if the name is not present, or if it not a single
   value."
  [env name]
  (let [[result] (env name)]
    (do (assert (not= result *empty-value*))
        result)))

(defn get-values
  "Gets a collection of values from the cells-map (env) by name"
  [env name]
  (let [result (env name)]
    (do
      (assert (not-any? #(= % *empty-value*) result))
      result)))


;;; Dependency Graph





;; End of file
